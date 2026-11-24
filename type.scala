package femtoml 

import femtoml.AST
import femtoml.DataType
import femtoml.Infer
import femtoml.TypeAnnotations

import java.util.IdentityHashMap

object Type

case class TypeException(message: String) extends Exception(message)

// top-level type checking
def typeProgram[A](p: Program, env: TypeContext, cont: TypeContext => A): A = {
	p match
		case EmptyStmt          => cont(env)
		case SeqStmt(h, t)      =>
			h match
				case e : Expr => evalType(env, e, r => typeProgram(t, env, cont))
				case d : Decl => evalDeclTypes(env, d, tenv => typeProgram(t, extendContextVars(env, tenv), cont))
}

// evaluate a single type expression in a given context
def evalType[A](env: TypeContext, expr: Expr, cont: TypeExpr => A) : A = {

	expr match
		case Num(n)                                => cont(NumType)
		case Character(c)                          => cont(CharType)

		// make sure the variable exists in the environment, then instantiate it
		case Identifier(id)                        => 
			if (!env.variables.contains(id)) throw new TypeException(s"Variable $id does not exist in the given type environment")
			else instantiate(env.variables(id), cont)
			
		case Constructor(id, xs)                   => 
		
			// attempt to fetch constructor information from environment
			val ctype = env.constructors.getOrElse(id, throw new TypeException(s"Undefined constructor $id."))
			
			// instantiate the constructor and check that the right number of arguments has been supplied
			instantiateConstructor(ctype, {case (argTypes, returnType) => 
				if (xs.length != argTypes.length) throw new TypeException(s"Constructor $id expects ${argTypes.length} arguments, but received ${xs.length}")
				
				// ensure that each argument is of the correct type, then return the constructor's return type
				evalArgTypes(id, env, xs.zip(argTypes), () => cont(returnType) )
			})
				
		case Apply(f, x)                           => 
			// create a new type unknown for the return type
			var tr = UnknownType(None)
			// infer the function's type, then infer the argument's type
			evalType(env, f, tf => evalType(env, x, tx =>	
				// attempt to unify the types and constrain the return type; return the return type if successful
				unify(tf, FunType(tx, tr), () => cont(tr))
			))
			
		case Fun(id, body)                         => 
			// create a new type unknown for the argument
			var targ = UnknownType(None)
			// infer the function's type with an environment containing the argument's type
			evalType(extendContextVars(env, Map(id -> targ)), body, tbody => cont(FunType(targ, tbody)))
			
		case CaseFun(cases)                        => 
			// the case function's argument type and return type are unknown
			val targ = UnknownType(None)
			val tres = UnknownType(None)
			
			// attempt to unify the types of all the patterns, and then the types of all the cases; if that succeeds, return the resulting types 
			evalCaseTypes(cases, targ, tres, env, () => cont(FunType(targ, tres)))
		
		case Let(d, e)                             => 
			// evaluate types of declared variables, then evaluate the body in that context
			evalDeclTypes(env, d, ds => evalType(extendContextVars(env, ds), e, cont))
			
}

// given an argument type, a result type, and environment, attempt to unify all the patterns and all the bodies in a case function
def evalCaseTypes[A](cases: List[(Expr, Expr)], targ: TypeExpr, tres: TypeExpr, env: TypeContext, cont: () => A) : A = {
	cases match
		// first evaluate the pattern to get its type and bindings
		case (pat, body)::cs => evalPattern(env, pat, {case (tp, vs) =>
			// attempt to unify this type with the argument type
			unify(targ, tp, () => 
				// then evaluate the type of the body in the augmented environment
				evalType(extendContextVars(env, vs), body, tb => 
					// attempt to unify the result with the return type
					unify(tres, tb, () => 
						// and repeat for the rest of the cases
						evalCaseTypes(cs, targ, tres, env, cont))))
		}) 
		case Nil             => cont()
} 

// given a list of tuples containing an expression and expected type, attempt to unify all pairs
def evalArgTypes[A](id: String, env: TypeContext, args: List[(Expr, TypeExpr)], cont: () => A) : A = {
	args match
		case (x, t)::ets => evalType(env, x, ta => unify(ta, t, () => evalArgTypes(id, env, ets, cont))) 
		case Nil => cont()
}

// given a pattern, infer its type and return the type environment of all of its variables
def evalPattern[A](env: TypeContext, pattern: Expr, cont: (TypeExpr, TypeEnv) => A) : A = {
	pattern match
		case Num(_)                => cont(NumType, Map.empty)
		case Character(_)          => cont(CharType, Map.empty)
		
		// return an environment mapping an identifier to a new type unknown
		case Identifier(id)        => 
			val tu = UnknownType(None)
			cont(tu, Map(id -> tu))
			
		case Constructor(id, args) =>
			
			// retrieve type of constructor 
			val ctype = env.constructors.getOrElse(id, throw new TypeException(s"Undefined constructor $id"))
			
			// instantiate constructor type
			instantiateConstructor(ctype, { case (argTypes, returnType) =>
			
				// check that the correct number of arguments has been supplied
				if (args.length != argTypes.length) throw new TypeException(s"Constructor $id expects ${argTypes.length} arguments, but received ${args.length}")
				
				// evaluate each of the supplied arguments, then check if they match the expected argument types for the constructor
				evalPatterns(args, env, rs => unifyTuples(rs.map(_._1).zip(argTypes), () => 

					// make sure no duplicate variables are used
					val allBindings = rs.flatMap(_._2)
					val names = allBindings.map(_._1)
					if (names.distinct.length != names.length) throw new TypeException(s"Duplicate variables in pattern $pattern")

					// return the constructor's return type along with all of the bindings connected to each argument
					cont(returnType, allBindings.toMap)
			
			))})
			
		case _                     => throw new TypeException(s"Invalid pattern $pattern")
			
}

// given a list of patterns, infer the types of each of them and generate their relevant bindings
def evalPatterns[A](args: List[Expr], env: TypeContext, cont: List[(TypeExpr, TypeEnv)] => A) : A = {
	args match
		case a::as => evalPattern(env, a, (a, b) => evalPatterns(as, env, rs => cont((a,b)::rs)))
		case Nil   => cont(Nil)
}


// when given a polymorphic type, instantiate it with fresh type unknowns
// otherwise, return the given type
def instantiate[A](texpr: TypeExpr, cont: TypeExpr => A) : A = {
	texpr match
		case PolyType(vs, t) => substitution(vs, rs => substitute(rs, t, cont))
		case _               => cont(texpr)
}


def instantiateConstructor[A](sig: ConstructorSignature, cont: (List[TypeExpr], TypeExpr) => A) : A = {
	
	// create a mapping from each variable in the constructor type to a new type unknown
	substitution(sig.vars, env =>
		// then make the substitutions in each of the constructor's argument types
		substituteList(env, sig.argTypes, args => 
			substitute(env, sig.returnType, result => 
				// return the list of substituted argument types and the substituted result type
				cont(args, result)))
	)
	 
}

// given a mapping of substitutions and a type expression, return the type expression with all appropriate substitutions made
def substitute[A](env: Map[String, TypeExpr], texpr: TypeExpr, cont: TypeExpr => A) : A = {
	texpr match
		case VarType(id)                     => cont(env.getOrElse(id, VarType(id)))
		case ConstructorType(id, ts)         => substituteList(env, ts, rs => cont(ConstructorType(id, rs)))
		case FunType(t1, t2)                 => substitute(env, t1, r1 => substitute(env, t2, r2 => cont(FunType(r1, r2))))
		case PolyType(vars, body)            => substitute(env -- vars, body, rb => cont(PolyType(vars, rb)))
		case _                               => cont(texpr)

}

// helper function for making all substitutions in a list of type expressions
def substituteList[A](env: TypeEnv, ts: List[TypeExpr], cont: List[TypeExpr] => A) : A = {
	ts match
		case t::tys => substitute(env, t, r => substituteList(env, tys, rs => cont(r::rs)))
		case Nil    => cont(Nil)
}

// given a list of type variables, return an environment mapping each identifier to a new type unknown
def substitution[A](vs: List[String], cont: Map[String, TypeExpr] => A) : A = cont(vs.map(x => x -> (UnknownType(None) : TypeExpr)).toMap)


// infer the types of a simultaneous group of declarations without generalising
def evalDeclGroup[A](env: TypeContext, decl: Decl, cont: TypeEnv => A) : A = {
	decl match
		case ValDec(id, expr)          => evalType(env, expr, res => cont(Map(id -> res)))

		case AndDec(d1, d2)            =>
			// evaluate d1 and d2 
			evalDeclGroup(env, d1, env1 => evalDeclGroup(env, d2, env2 => 
				
				// check for duplicates
				val duplicates = env1.keySet.intersect(env2.keySet)
				if (duplicates.nonEmpty) throw new TypeException(s"Duplicate declarations: ${duplicates.mkString(", ")}")
			
				// return combined environment
				cont(env1 ++ env2)
			))
			
		
		case RecDec(ValDec(id, expr)) =>
		
			// create a new type unknown for the identifier
			val tu = UnknownType(None)
			
			// evaluate the expression in that context; unify the type unknown with the inferred type; add the type unknown to the type environment
			evalType(extendContextVars(env, Map(id -> tu)), expr, ti => unify(tu, ti, () => cont(Map(id -> tu))))
			
		// ignore user-defined datatype declarations, since they have already been dealt with by datatype.scala
		case DataDec(_, _, _) => cont(Map.empty)
		
		// a type annotation shouldn't occur on its own 
		case HasType(id	, _) => throw new TypeException(s"Expected a declaration of $id immediately after type annotation.")
}


// infer the types of a declaration and generalise it
def evalDeclTypes[A](env: TypeContext, decl: Decl, cont: TypeEnv => A) : A = {
	decl match 
	
		// type annotation + declaration
		case SeqDec(HasType(id, typeExpr), d2) =>
	
			val (expr, rec) = d2 match
				case ValDec(id2, e) if id == id2         => (e, false) 
				case RecDec(ValDec(id2, e)) if id == id2 => (e, true)
				case _                                   => throw new TypeException(s"Expected a declaration of $id immediately after type annotation.")
				
			// verify the user's type annotation
			checkAnnotatedType(id, rec, typeExpr, expr, env, cont)
			
		case SeqDec(d1, d2) =>
			// evaluate d1, evaluate d2, return combined type environment
			evalDeclTypes(env, d1, env1 => evalDeclTypes(extendContextVars(env, env1), d2, env2 => cont(env1 ++ env2)))
		
		case _              =>
			// evaluate the declaration and generalise every entry in the resulting environment
			evalDeclGroup(env, decl, vs => generaliseEnv(vs.toList, env, cont))
	
	
}

// given a type environment in list form, return the type environment gained from generalising every element
def generaliseEnv[A](envList: List[(String, TypeExpr)], env: TypeContext, cont: TypeEnv => A) : A = {
	envList match
		case (id, t)::es => generalise(env.variables, t, tg => generaliseEnv(es, env, rs => cont(Map(id -> tg) ++ rs)))
		case Nil         => cont(Map.empty)
}












