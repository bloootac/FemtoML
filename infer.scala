package femtoml 

import femtoml.AST

import java.util.IdentityHashMap

object Infer

/*
	this object defines the main components of the type inference engine:
		- type environments
		- unification
		- "safe" unification, for checking type annotations written by the user
		- generalisation
*/

case class TypeInferenceException(message: String) extends Exception(message)

// ============================= environment ============================= 

// environments for user-defined types and constructors 
case class ConstructorSignature(vars: List[String], argTypes: List[TypeExpr], returnType: TypeExpr) {override def toString : String = 
																										s"[${argTypes.mkString(", ")}] -> $returnType"}
type ConstructorEnv = Map[String, ConstructorSignature]
type TypeDefEnv = Map[String, ConstructorType]

// constraints are used for "safe" unification
type Constraints = IdentityHashMap[UnknownType, TypeExpr]

// environment for variables
type TypeEnv = Map[String, TypeExpr]

// wider type context
case class TypeContext(variables: TypeEnv, constructors: ConstructorEnv, typeDefs: TypeDefEnv)

// helper function for adding a new variable to a type context
def extendContextVars(env: TypeContext, vs: TypeEnv) = TypeContext(env.variables ++ vs, env.constructors, env.typeDefs)

// ============================= unification ============================= 

// given two types, attempt to equate them while solving type constraints on unknowns as a side effect
def unify[A](t1: TypeExpr, t2: TypeExpr, cont: () => A) : A = {
		
	// find constrained types of t1 and t2
	typeclass(t1, false, ty1 => typeclass(t2, false, ty2 =>
		
		(ty1, ty2) match
			case (NumType, NumType)                                               => cont()
			case (CharType, CharType)                                             => cont()
			
			case (ConstructorType(id1, t1s), ConstructorType(id2, t2s))           => 
				// check that constructor names and argument lengths match
				if (t1s.length != t2s.length || id1 != id2) throw new TypeInferenceException(s"Constructor types $ty1 and $ty2 do not match.")
				
				// unify the types of each argument in t1 with its corresponding argument in t2
				unifyTuples(t1s.zip(t2s), cont)
				
			case (FunType(t1args, t1body), FunType(t2args, t2body))               => 
				// unify argument types, then unify return types
				unify(t1args, t2args, () => unify(t1body, t2body, cont))
				
			// if one of the types is unknown, set it to the other type
			case (x: UnknownType, _)                                              => equate(x, ty2, cont)
			case (_, y: UnknownType)                                              => equate(y, ty1, cont)
			
			case _                                                                => throw new TypeInferenceException(s"Types $ty1 and $ty2 do not match.")
		
	))
}

// helper function for unifying each pair in a list of tuples
def unifyTuples[A](ts: List[(TypeExpr, TypeExpr)], cont: () => A): A = {
	ts match
		case (t1, t2)::tys => unify(t1, t2, () => unifyTuples(tys, cont))
		case Nil           => cont()
}

// map a set type unknown to the type to which it is constrained to be equivalent; map all other types to themselves
def typeclass[A](texpr: TypeExpr, safe : Boolean, cont: TypeExpr => A) : A = {
	texpr match
		case t1: UnknownType      =>
			t1.texpr match
				// set t1 to its constraint if not in safe mode, then return that constrained type
				case Some(t2) => typeclass(t2, safe, { t3 => if (!safe) t1.texpr = Some(t3); cont(t3) })
				case None     => cont(texpr)
		case _                    => cont(texpr)
}

// set the given type unknown to the given type expression, as long as that wouldn't produce an infinite type
def equate[A](t1: UnknownType, t2: TypeExpr, cont: () => A) : A = {
	t2 match
		case _: UnknownType if t1 eq t2 => cont()
		case _                          =>
			occurs(t1, t2, res => if (!res) { t1.texpr = Some(t2); cont() } else throw new TypeInferenceException(s"type $t1 cannot be the same as type $t2"))
}


// check whether the first type occurs in the second
def occurs[A](t1: UnknownType, t2: TypeExpr, cont: Boolean => A) : A = {
	typeclass(t2, false, r => r match 
		case u: UnknownType            => cont(t1 eq u)
		case ConstructorType(_, ts)    => occursInList(t1, ts, cont)
		case FunType(targ, tbody)      => occurs(t1, targ, res => if (res) cont(true) else occurs(t1, tbody, cont))
		case _                         => cont(false)
	)
}

// helper function for checking if a type unknown occurs in a list of types
def occursInList[A](t1: UnknownType, ts: List[TypeExpr], cont: Boolean => A): A = {
	ts match
		case t2::tys => occurs(t1, t2, res => if (!res) occursInList(t1, tys, cont) else cont(true))
		case Nil     => cont(false)
}

// ============================= "safe" unification ============================= 

// a unification function that returns a map from type unknowns to their proposed constraints
// used when checking a user's polymorphic type annotations
def safeUnify[A](t1: TypeExpr, t2: TypeExpr, constraints: Constraints, cont: Constraints => A) : A = {
		
	safeResolve(t1, constraints, ty1 => safeResolve(t2, constraints, ty2 =>

		(ty1, ty2) match
			case (NumType, NumType)                                               => cont(constraints)
			case (CharType, CharType)                                             => cont(constraints)
			
			// we only unify rigid types if they are of the same instance
			case (r1: RigidType, r2: RigidType) if r1 eq r2                       => cont(constraints)
			
			case (ConstructorType(id1, t1s), ConstructorType(id2, t2s))           => 
				// check that constructor names and argument lengths match
				if (t1s.length != t2s.length || id1 != id2) throw new TypeInferenceException(s"Constructor types $ty1 and $ty2 do not match.")
				
				// unify the types of each argument in t1 with its corresponding argument in t2
				safeUnifyTuples(t1s.zip(t2s), constraints, cont)
				
			case (FunType(t1args, t1body), FunType(t2args, t2body))               => 
				// unify argument types, then unify return types
				safeUnify(t1args, t2args, constraints, c1 => safeUnify(t1body, t2body, c1, cont))

				
			// if one of the types is unknown, set it to the other type
			case (x: UnknownType, _)                                              => safeEquate(x, ty2, constraints, cont)
			case (_, y: UnknownType)                                              => safeEquate(y, ty1, constraints, cont)
			
			case _                                                                => throw new TypeInferenceException(s"Types $ty1 and $ty2 do not match.")
		
	))
	
}

// helper function for safely unifying a list of tuples
def safeUnifyTuples[A](ts: List[(TypeExpr, TypeExpr)], cs: Constraints, cont: Constraints => A) : A = {
	ts match
		case (t1, t2)::tys => safeUnify(t1, t2, cs, rcs => safeUnifyTuples(tys, rcs, cont))
		case Nil           => cont(cs)
} 

// a version of equate() which returns an updated set of constraints instead of directly modifying type unknowns
def safeEquate[A](t1: UnknownType, t2: TypeExpr, cs : Constraints, cont: Constraints => A) : A = {

	safeResolve(t2, cs, ty2 => 
		ty2 match
			case _: UnknownType if t1 eq ty2         => cont(cs)
			case _                                   =>
				safeOccurs(t1, t2, cs, res => 
					if (!res) {val updated = new IdentityHashMap(cs); updated.put(t1, ty2); cont(updated)} 
					else throw new TypeInferenceException(s"type $t1 cannot be the same as type $t2"))
	)
}

// given a type expression and set of constraints, map the expression to its propsed constraint
def safeResolve[A](t: TypeExpr, cs: Constraints, cont: TypeExpr => A) : A = {

	typeclass(t, true, ty => ty match
		case u: UnknownType if cs.containsKey(u)         => safeResolve(cs.get(u), cs, cont)
		case other                                       => cont(other)
	)	
}

// a version of occurs() which takes a constraints mapping into account
def safeOccurs[A](t1: UnknownType, t2: TypeExpr, cs: Constraints, cont: Boolean => A) : A = {
	safeResolve(t2, cs, r => r match 
		case u: UnknownType            => cont(t1 eq u)
		case ConstructorType(_, ts)    => safeOccursInList(t1, ts, cs, cont)
		case FunType(targ, tbody)      => safeOccurs(t1, targ, cs, r => if (r) cont(true) else safeOccurs(t1, tbody, cs, cont))
		case _                         => cont(false)
		
	)
}

// helper function for safely checking if a type unknown occurs in a list 
def safeOccursInList[A](t1: UnknownType, ts: List[TypeExpr], cs: Constraints, cont: Boolean => A) : A = {
	ts match
		case t2::tys => safeOccurs(t1, t2, cs, res => if (res) cont(true) else safeOccursInList(t1, tys, cs, cont))
		case Nil     => cont(false)
}


// ============================= generalisation ============================= 

// convert a type expression containing unknowns to a polymorphic type with type variables
def generalise[A](env: TypeEnv, t: TypeExpr, cont: TypeExpr => A) : A = {
	
	// collect all the unknowns in the type expression
	collectUnknownsInList(env.values.toList, envVars => unknowns(t, res => 
	
		// get the unknowns which are not defined in the wider environment (free variables)
		val freeVars = res.filterNot { u => envVars.exists(_ eq u) }
		
		// collect all type identifiers used in the expression and the environment
		collectNames(t, res1 => 
			collectNamesInList(env.values.toList, res2 =>
				val usedNames = res1++res2

				// obtain a mapping from each free variable in the type expression to a unique name
				freshNamesForList(freeVars, usedNames, 0, replacements => 
					// carry out the substitutions in the type expression
					replace(t, replacements, body =>
					
						// if there are no free variables, return a monotype - otherwise, return a polymorphic type
						val vars = replacements.map(_._2)
						if (vars.isEmpty) cont(body) else cont(PolyType(vars, body))
	))))))
}

// given a type expression, generate a list of the unique type unknows that occur in the expression.
def unknowns[A](t: TypeExpr, cont: List[UnknownType] => A) : A = collectUnknowns(t, us => uniqueUnknowns(us, Nil, cont))


// given a type expression, generate a list of all unique type unknowns that occur in the expression
def collectUnknowns[A](t: TypeExpr, cont: List[UnknownType] => A) : A = {
	typeclass(t, false, r => r match
		case u: UnknownType             => cont(List(u))
		case FunType(tf, tx)            => collectUnknowns(tf, rfs => collectUnknowns(tx, rxs => cont(rfs ++ rxs)))
		case ConstructorType(_, ts)     => collectUnknownsInList(ts, cont)
		case PolyType(_, body)          => collectUnknowns(body, cont)
		case _                          => cont(Nil)  
	)
}

// helper function for generating a list of distinct unknown types in a type expression
def uniqueUnknowns[A](rs: List[UnknownType], acc: List[UnknownType], cont: List[UnknownType] => A) : A = {
	rs match
		case u::rest => if (acc.exists(_ eq u)) uniqueUnknowns(rest, acc, cont) else uniqueUnknowns(rest, u::acc, cont)
		case Nil     => cont(acc)
}

// helper function for generating a list of unknown types from a list of type expressions
def collectUnknownsInList[A](ts: List[TypeExpr], cont: List[UnknownType] => A) : A = {
	ts match
		case t::tys => collectUnknowns(t, r => collectUnknownsInList(tys, rs => cont(r ++ rs)))
		case Nil    => cont(Nil)
}


// given a type expression, collect all the identifiers used for types in the expression
def collectNames[A](t: TypeExpr, cont: Set[String] => A) : A = {
	t match
		case VarType(id)               => cont(Set(id))
		case FunType(tf, tx)           => collectNames(tf, s1 => collectNames(tx, s2 => cont(s1 ++ s2)))
		case ConstructorType(_, ts)    => collectNamesInList(ts, cont)
		case PolyType(vs, body)        => collectNames(body, s => cont(vs.toSet ++ s))
		case UnknownType(Some(texpr))  => collectNames(texpr, cont)
		case other                     => cont(Set.empty)
}

def collectNamesInList[A](ts: List[TypeExpr], cont: Set[String] => A) : A = {
	ts match
		case t::tys => collectNames(t, r1 => collectNamesInList(tys, r2 => cont(r1 ++ r2)))
		case Nil    => cont(Set.empty)
}

// given a set of used names and a counter, generate an identifier that is not in the set, along with an updated counter and set
def freshName[A](usedNames: Set[String], next: Int, cont: (String, Int, Set[String]) => A) : A = {

	var name = s"t$next"
	var newNext = next
	
	while (usedNames.contains(name)) {
		newNext += 1
		name = s"t$newNext"
	}
	
	cont(name, newNext + 1, usedNames+name)
}

// helper function for generating a fresh name for every element in a list of type unknowns
def freshNamesForList[A](freeVars: List[UnknownType], usedNames: Set[String], next: Int, cont: List[(UnknownType, String)] => A): A = {
	freeVars match
		case v::vs => 
			freshName(usedNames, next, {case (name, next2, usedNames2) => 
				freshNamesForList(vs, usedNames2, next2, names => cont((v, name)::names))
			})
		case Nil  => cont(List.empty)
}

// given a type expression and a map from type unknowns to identifiers, return the type expression with all substitutions made
def replace[A](t: TypeExpr, replacements: List[(UnknownType, String)], cont: TypeExpr => A) : A = {
	typeclass(t, false, r => r match
	
		case u: UnknownType           =>
			// attempt to find an identifier for the type unknown
			replacements.find({case (v, _) => v eq u}) match {
				// if one exists, return it
				case Some((_, name)) => cont(VarType(name))
				case None            => cont(u)
			}
			
		case FunType(tf,tx)           => replace(tf, replacements, rf => replace(tx, replacements, rx => cont(FunType(rf, rx))))
		case ConstructorType(id, ts)  => replaceInList(ts, replacements, res => cont(ConstructorType(id, res)))
		case PolyType(vs, body)       => replace(body, replacements, rb => cont(PolyType(vs, rb)))
		case other                    => cont(other)
		
	)
			
}

// helper function for making substitutions in every element of a list of type expressions
def replaceInList[A](ts: List[TypeExpr], rs: List[(UnknownType, String)], cont: List[TypeExpr] => A) : A = {
	ts match
		case t::tys => replace(t, rs, r1 => replaceInList(tys, rs, r2 => cont(r1::r2)))
		case Nil    => cont(Nil)
}

