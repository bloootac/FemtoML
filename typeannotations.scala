package femtoml

import femtoml.AST
import femtoml.DataType
import femtoml.Infer

import java.util.IdentityHashMap

/*
	this object defines the functions used to verify and commit a user's type annotation.
	
	a user's type annotation may be less general than the inferred type, but it cannot be more general than the inferred type.
	
	f : Num -> Num; f = \x.x;;              is correct
	f : @a. a -> a; f = \x.0;;              is not correct because Num does not match the generic type variable a 
	
*/

object TypeAnnotations

case class TypeAnnotationException(message: String) extends Exception(message)

// verify a user's type annotation and commit its constraints if successful
def checkAnnotatedType[A](id: String, rec: Boolean, typeExpr: TypeExpr, expr: Expr, env: TypeContext, cont: TypeEnv => A) : A = {

		typeExpr match 
			
			/* if this is a polytype, we need to perform "safe" unification without modifying type unknowns,
			   and then check that the result doesn't require type unknowns from the wider environment to be "rigid" types */
			case PolyType(vars, body) => 	
				
				// verify that the type is a valid annotation
				resolveType(body, env.typeDefs, vars.toSet, resolvedBody =>
					val ta = PolyType(vars, resolvedBody)
					
					// infer the type of the expression, then generalise it with respect to the environment, then instantiate it
					inferAnnotatedDef(id, rec, expr, env, t => generalise(env.variables, t, tg => instantiate(tg, ti => 
						
						// create a mapping from each variable in the annotation to a unique "rigid type"
						val rigidMap = vars.map(v => v -> (new RigidType(v) : TypeExpr)).toMap
						
						// substitute the type variables in the expression with these rigid types and attempt to unify this with the inferred type
						substitute(rigidMap, resolvedBody, ra =>
							safeUnify(ti, ra, new IdentityHashMap[UnknownType, TypeExpr](), cs => 
							
								// check that the resulting constraints do not require a non-free type unknown to be rigid
								checkRigidEscape(env, cs, rigidMap.values.collect({case r: RigidType => r}).toList, res => 		
								
									// if so, commit the constraints, then return a mapping to the annotated type 
									if (!res) commitAnnotation(env, cs, () => cont(Map(id -> ta)))
									else throw new TypeAnnotationException(s"Invalid polymorphic type annotation for $id"))
								
			))))))
				
			// if this is a monotype, the process is much simpler
			case _                    =>
				// verify that the annotated type has correct variables and constructor names
				resolveType(typeExpr, env.typeDefs, Set.empty, ta => 
					// infer the type of the expression
					inferAnnotatedDef(id, rec, expr, env,  ti =>
						// attempt to unify the two types; return a mapping to the annotated type if successful
						unify(ta, ti, () => cont(Map(id -> ta))
			)))
	
}

// infer the type of a given expression, depending on whether it came from a recursive definition or not
def inferAnnotatedDef[A](id: String, rec: Boolean, expr: Expr, env: TypeContext, cont: TypeExpr => A) : A = {
	if (!rec) evalType(env, expr, cont)
	else
		val tu = UnknownType(None)
		val env2 = extendContextVars(env, Map(id -> tu))
		evalType(env2, expr, ti => unify(tu, ti, () => cont(tu)))
}

// given a list of constraints and an environment, check that the constraints do not require a type unknown in the environment to be "rigid"
def checkRigidEscape[A](env: TypeContext, cs: Constraints, rs: List[RigidType], cont: Boolean => A) : A = {
	
	// collect all unique unknowns in the environment
	collectUnknownsInList(env.variables.values.toList, us => 
		uniqueUnknowns(us, Nil, protectedUnknowns => 
			// then check if any of them are constrained to be rigid
			escapedUnknowns(protectedUnknowns, cs, rs, cont)))
	
}

// given a list of protected type unknowns and constraints, check if the constraints require one of the unknowns to be "rigid"
def escapedUnknowns[A](protectedUnknowns: List[UnknownType], cs: Constraints, rigids: List[RigidType], cont: Boolean => A) : A = {
	protectedUnknowns match
		case u::us => 
		    // apply the constraints to the type unknown
			applyConstraints(u, cs, t => 
				// check if the type now contains a rigid type variable
				containsRigid(t, rigids, res =>
					// return true if so; otherwise, continue
					if (res) cont(true) else escapedUnknowns(us, cs, rigids, cont)
			))
		case Nil   => cont(false)
}

// apply constraints to a type expression
def applyConstraints[A](t: TypeExpr, cs: Constraints, cont: TypeExpr => A) : A = {
	safeResolve(t, cs, _ match
		case ConstructorType(id, args) => applyConstraintsList(args, cs, rs => cont(ConstructorType(id, rs)))
		case FunType(arg, ret)         => applyConstraints(arg, cs, tf => applyConstraints(ret, cs, tx => cont(FunType(tf, tx))))
		case other                     => cont(other) 
	)
}

// helper function for applying constraints to each element of a list of type expression
def applyConstraintsList[A](ts: List[TypeExpr], cs: Constraints, cont: List[TypeExpr] => A) : A = {
	ts match
		case t::tys => applyConstraints(t, cs, r => applyConstraintsList(tys, cs, rs => cont(r::rs))) 
		case Nil    => cont(Nil)
}

// given a type expression and a list of rigid type variables, check if the expression contains one of the rigid types
def containsRigid[A](t: TypeExpr, rs: List[RigidType], cont: Boolean => A) : A = {
	t match
		case r : RigidType          => cont(rs.exists(r2 => r2 eq r))
		case ConstructorType(_, ts) => containsRigidList(ts, rs, cont)
		case FunType(arg, ret)      => containsRigid(arg, rs, res => if (res) cont(true) else containsRigid(ret, rs, cont))
		case _                      => cont(false)
}

// helper function for checking whether any type expression in a list contains a rigid type variable
def containsRigidList[A](ts: List[TypeExpr], rs: List[RigidType], cont: Boolean => A) : A = {
	ts match
		case t::tys => containsRigid(t, rs, res => if (res) cont(true) else containsRigidList(tys, rs, cont))
		case Nil    => cont(false)
}

// apply constraints to any relevant type unknowns in the environment
def commitAnnotation[A](env: TypeContext, cs: Constraints, cont: () => A) : A = {
	// gather the environment's type unknowns
	collectUnknownsInList(env.variables.values.toList, res => 
		uniqueUnknowns(res, Nil, us => 
			// apply the constraints to the type unknowns
			commitEnvUnknowns(us, cs, cont)))
}

// given a list of type unknowns and a set of constraints, apply the constraints to any of the relevant type unknowns
def commitEnvUnknowns[A](us: List[UnknownType], cs: Constraints, cont: () => A) : A = {
	us match 
		case u::rs => 
			// evaluate the type unknown with respect to the constraints
			applyConstraints(u, cs, res => 
				// if the type unknown has been constrained, change it
				if (!(u eq res)) u.texpr = Some(res)
				// continue with the rest of the list
				commitEnvUnknowns(rs, cs, cont))
		case Nil   => cont()
}


