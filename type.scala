package femtoml 

import femtoml.AST
import femtoml.DataType
import femtoml.Infer

import java.util.IdentityHashMap

object Type

/*

TODO: 


resolve type annotations

actually check type annotations...
	- check monomorphic types
	- check polymorphic types

clean up and check

convert all to cps?

*/

case class TypeException(message: String) extends Exception(message)

type TypeEnv = Map[String, TypeExpr]
case class TypeContext(variables: TypeEnv, constructors: ConstructorEnv, typeDefs: TypeDefEnv)

def extendContextVars(env: TypeContext, vs: TypeEnv) = TypeContext(env.variables ++ vs, env.constructors, env.typeDefs)

// top-level type checking
def typeProgram(p: Program, env: TypeContext): Unit = {
	p match
		case EmptyStmt          => ()
		case SeqStmt(h, t)      =>
			h match
				case e : Expr => evalType(env, e); typeProgram(t, env)
				case d : Decl =>
					val tenv = evalDeclTypes(env, d)
					typeProgram(t, extendContextVars(env, tenv))
}

// evaluate a single type expression in a given context
def evalType(env: TypeContext, expr: Expr) : TypeExpr = {

	expr match
		case Num(n)                                => NumType
		case Character(c)                          => CharType

		case Identifier(id)                        => 
			if (!env.variables.contains(id)) throw new TypeException(s"Variable $id does not exist in the given type environment")
			else instantiate(env.variables(id))
			
		case Constructor(id, xs)                   => 
		
			// attempt to fetch constructor information from environment
			val ctype = env.constructors.getOrElse(id, throw new TypeException(s"Undefined constructor $id."))
			
			val (argTypes, returnType) = instantiateConstructor(ctype)
			
			if (xs.length != argTypes.length) throw new TypeException(s"Constructor $id expects ${argTypes.length} arguments, but received ${xs.length}")
			
			xs.zip(argTypes).foreach { case (x, expectedType) =>
				val actualType = evalType(env, x)
				
				try unify (actualType, expectedType)
				catch
					case error: TypeException => throw new TypeException(s"Constructor $id: argument $x has type $actualType, but expected $expectedType.")		
			}
				
			returnType
			
				
		case Apply(f, x)                           => 
			
			var tr = UnknownType(None)
			var tf = evalType(env, f)
			var tx = evalType(env, x)
			
			try unify(tf, FunType(tx, tr))
			catch
				error => throw new TypeException(s"$f : $tf can't be applied to $x : $tx because $error")
				
			tr
			
		case Fun(id, body)                         => 
			var targ = UnknownType(None)
			//var tbody = evalType(env + (id -> targ), body)
			var tbody = evalType(extendContextVars(env, Map(id -> targ)), body)
			FunType(targ, tbody)
			
		case CaseFun(cases)                        => 
			val targ = UnknownType(None)
			val tres = UnknownType(None)
			
			cases.foreach({
				case(pattern, body) => 
					val (tp, bindings) = evalPattern(env, pattern)
					unify(targ, tp)
					
					//val tb = evalType(env ++ bindings, body)
					val tb = evalType(extendContextVars(env, bindings), body)
					unify(tres, tb)
			})
			
			FunType(targ, tres)
		
		case Let(d, e)                             => 
			val bindings = evalDeclTypes(env, d)
			//val generalised = bindings.map({ case (id, t) => (id -> generalise(env.variables, t))})
			
			//evalType(env ++ generalised, e)
			evalType(extendContextVars(env, bindings), e)
			
}


def evalPattern(env: TypeContext, pattern: Expr) : (TypeExpr, TypeEnv) = {
	pattern match
		case Num(_)                => (NumType, Map.empty)
		case Character(_)          => (CharType, Map.empty)
		case Identifier(id)        => 
			val tu = UnknownType(None)
			(tu, Map(id -> tu))
		case Constructor(id, args) =>
			
			val ctype = env.constructors.getOrElse(id, throw new TypeException(s"Undefined constructor $id"))
			
			val (argTypes, returnType) = instantiateConstructor(ctype)
			
			if (args.length != argTypes.length) throw new TypeException(s"Constructor $id expects $argTypes.length arguments, but received $args.length")
			
			val results = args.map(arg => evalPattern(env, arg))
			
			results.map(_._1).zip(argTypes).foreach { case (actualType, expectedType) => unify(actualType, expectedType) }

			//val bindings = Map.from(results.flatMap(_._2))
			
			val allBindings = results.flatMap(_._2)
			val names = allBindings.map(_._1)
			
			if (names.distinct.length != names.length) throw new TypeException(s"Duplicate variables in pattern $pattern")
			
			
			(returnType, Map.from(allBindings))
			
		case _                     => throw new TypeException(s"Invalid pattern $pattern")
			
			
			
}


// when given a polymorphic type, instantiate it with fresh type unknowns
// otherwise, return the given type
def instantiate(texpr: TypeExpr) : TypeExpr = {
	texpr match
		case PolyType(vs, t) => substitute(substitution(vs), t)
		case _               => texpr
}


def instantiateConstructor(sig: ConstructorSignature): (List[TypeExpr], TypeExpr) = {
	val env = substitution(sig.vars)
	
	val args = sig.argTypes.map(t => substitute(env, t))
	
	val result = substitute(env, sig.returnType)
	
	(args, result) 
	
	 
}

def substitute(env: Map[String, TypeExpr], texpr: TypeExpr) : TypeExpr = {
	texpr match
		case VarType(id)                     => env.getOrElse(id, VarType(id))
		/*
			try env(id)
			catch
				case e : java.util.NoSuchElementException => throw new TypeException(s"Free type variable $id in polytype body.")
				
				*/
		case ConstructorType(id, ts)         => ConstructorType(id, ts.map(t => substitute(env,t)))
		case FunType(t1, t2)                 => FunType(substitute(env, t1), substitute(env,t2))
		case PolyType(vars, body)            =>
			var filtered = env -- vars
			PolyType(vars, substitute(filtered, body))
		case _                               => texpr

}

// given a list of type variables, return an environment mapping each identifier to a new type unknown
def substitution(vs: List[String]) : Map[String, TypeExpr] = Map.from(vs.map(x => x -> (UnknownType(None) : TypeExpr)))



// infer the types of a simultaneous group of declarations without generalising
def evalDeclGroup(env: TypeContext, decl: Decl) : TypeEnv = {
	decl match
		case ValDec(id, expr)          => Map(id -> evalType(env, expr))

		case AndDec(d1, d2)            =>
			val env1 = evalDeclGroup(env, d1)
			val env2 = evalDeclGroup(env, d2)
			
			val duplicates = env1.keySet.intersect(env2.keySet)
			if (duplicates.nonEmpty) throw new TypeException(s"Duplicate declarations: ${duplicates.mkString(", ")}")
			
			env1 ++ env2
			
			
		case RecDec(ValDec(id, value)) =>
			val tu = UnknownType(None)
			
			val inferred = evalType(extendContextVars(env, Map(id -> tu)), value)
			
			unify(tu, inferred)
			
			Map(id -> tu)
			
		case DataDec(_, _, _) => Map.empty
		
		case HasType(id	, _) => throw new TypeException(s"Expected a declaration of $id immediately after type annotation.")
}


// infer the types of a declaration and generalise it
def evalDeclTypes(env: TypeContext, decl: Decl) : TypeEnv = {
	decl match 
	
		case SeqDec(HasType(id, typeExpr), d2) =>
	
			val expr = d2 match
				case SeqDec(ValDec(id2, e), _) if id == id2        => e 
				case SeqDec(RecDec(ValDec(id2, e)), _) if id == id2 => e
				case _                                             => throw new TypeException(s"Expected a declaration of $id immediately after type annotation.")
				
				
			checkAnnotatedType(id, typeExpr, expr, env)
			
	
		case SeqDec(d1, d2) =>
			val env1 = evalDeclTypes(env, d1)
			
			val env2 = evalDeclTypes(extendContextVars(env, env1), d2)
			
			env1 ++ env2
		
		case _              =>
			
			val bindings = evalDeclGroup(env, decl)
			
			Map.from(bindings.map { case (id, t) => id -> generalise(env.variables, t) })
	
	
}






def generalise(env: TypeEnv, t: TypeExpr) : TypeExpr = {

	val envVars = env.values.toList.flatMap(unknowns)
	val freeVars = unknowns(t).filterNot { u => envVars.exists(_ eq u) }
	
	var usedNames = collectNames(t)
	
	env.values.foreach(v => usedNames ++= collectNames(v))
	

	var next = 0
	val replacements = freeVars.map(u => { val (s, n, ns) = freshName(usedNames, next); next = n; usedNames = ns; (u, s) } )
	
	val body = replace(t, replacements)
	
	val vars = replacements.map(_._2)
	
	if (vars.isEmpty) body
	else PolyType(vars, body)
}




def checkAnnotatedType(id: String, typeExpr: TypeExpr, expr: Expr, env: TypeContext) : TypeEnv = {

		typeExpr match 
			case PolyType(vars, body) => 
				
			
				val resolvedBody = resolveType(typeExpr, env.typeDefs, vars.toSet)
				val ta = PolyType(vars, resolvedBody)
				
				val ti = instantiate(generalise(env.variables, evalType(env, expr)))
				
				val rigidMap = vars.map(v => v -> (new RigidType : TypeExpr)).toMap
				
				val ra = substitute(rigidMap, resolvedBody)
				
				val cs = safeUnify(ti, ra, new IdentityHashMap[UnknownType, TypeExpr]())
				
				try checkRigidEscape(env, cs, rigidMap.values.collect{case r : RigidType => r}.toList)
				catch 
					case error => throw new TypeException(s"Invalid polymorphic type annotation for $id")
					
				Map(id -> ta)
				
				
			case _                    =>
			
				// resolve annotated type
				val ta = resolveType(typeExpr, env.typeDefs, Set.empty)

				// infer type from expression
				val ti = evalType(env, expr)
				
				// attempt to unify the two
				unify(ta, ti)
				
				// if that succeeds, add the annotated type to the environment
				Map(id -> ta)
	
}


def checkRigidEscape(env: TypeContext, cs: Constraints, rs: List[RigidType]) : Unit = {
	val protectedUnknowns = env.variables.values.toList.flatMap(unknowns)
	
	val escaped = protectedUnknowns.exists { u => containsRigid(applyConstraints(u, cs), rs) }
	
	if (escaped) throw new TypeInferenceException("Rigid type variable escaped into surrounding environment")
}

def applyConstraints(t: TypeExpr, cs: Constraints) : TypeExpr = {
	safeResolve(t, cs) match
		case ConstructorType(id, args) => ConstructorType(id, args.map(t => applyConstraints(t, cs)))
		case FunType(arg, ret)         => FunType(applyConstraints(arg, cs), applyConstraints(ret, cs))
		case other                     => other 
}

def containsRigid(t: TypeExpr, rs: List[RigidType]) : Boolean = {
	t match
		case r : RigidType => rs.exists(r2 => r2 eq r)
		case ConstructorType(_, ts) => ts.exists(t => containsRigid(t, rs))
		case FunType(arg, ret)      => containsRigid(arg, rs) || containsRigid(ret, rs)
		case _ => false
}










