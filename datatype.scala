package femtoml

import femtoml.AST

/*
	this object defines the functions used to evaluate user-defined algebraic types.
	
	we execute two passes:
		- first, we register the names and arities of all user-defined types in the program
		- then, we register the types of all user-defined type constructors in the program
		
	this approach is used to deal with recursive types.
*/

object DataType

case class TypeDefException(message: String) extends Exception(message)

case class ConstructorSignature(vars: List[String], argTypes: List[TypeExpr], returnType: TypeExpr)
type ConstructorEnv = Map[String, ConstructorSignature]
type TypeDefEnv = Map[String, ConstructorType]

// main entry point
def checkDataTypes(p: Program): (TypeDefEnv, ConstructorEnv) = {
	val defenv = evalDataDecls(p)
	val conenv = evalConstructors(p, defenv)
	
	(defenv, conenv)
}

// first pass
def evalDataDecls(p: Program): TypeDefEnv = {

	val reservedTypes = Set("Num", "Char")
	
	p match 
		case EmptyStmt => Map.empty

		case SeqStmt(h, t) =>
		
			// look through rest of program
			val rest = evalDataDecls(t)

			h match 
				case DataDec(id, vars, _) =>

					// check if data type has been defined before or has duplicate type parameters
					if (rest.contains(id)) throw new TypeDefException(s"Type $id is already defined")
					if (reservedTypes.contains(id)) throw new TypeDefException(s"Cannot redefine built-in type $id")
					if (vars.distinct.length != vars.length) throw new TypeDefException(s"Duplicate type parameters in $id")

					// add declaration to type definition environment
					rest + (id -> ConstructorType(id, vars.map(VarType(_))))

				case _ => rest
		
	
}

// second pass
def evalConstructors(p: Program, env: TypeDefEnv): ConstructorEnv = {
	p match 
		case EmptyStmt => Map.empty

		case SeqStmt(h, t) =>
			val rest = evalConstructors(t, env)

			h match
				case DataDec(id, vars, constructors) =>

					// retrieve type associated with identifier
					val data = env.getOrElse(id, throw new TypeDefException(s"Unknown data type: $id"))

					// check that all constructors are distinct 
					val names = constructors.map(_._1)
					if (names.distinct.length != names.length) throw new TypeDefException(s"Duplicate constructors in $id")

					// generate constructor signatures
					val current = constructors.map { case (cid, cargs) => 
						// resolve all argument types
						val argTypes = cargs.map(resolveType(_, env, vars.toSet))
						
						//create mapping to constructor signature 
						cid -> ConstructorSignature(vars, argTypes, data)
					}.toMap

						// Check duplicate constructors across declarations
						val duplicates = current.keySet.intersect(rest.keySet)

						if (duplicates.nonEmpty) throw new TypeDefException(s"Duplicate constructors: ${duplicates.mkString(", ")}")

						current ++ rest

				case _ => rest

}

/*

	the types of a constructor's arguments may include incorrectly parsed VarTypes that actually refer to constructors defined earlier in the program.
	the parser does not handle this issue; instead, we fix it here by replacing any VarTypes with their matching ConstructorType
	
	we check for undefined types and incorrect constructor arguments along the way 
*/
def resolveType(t: TypeExpr, env: TypeDefEnv, vars: Set[String]): TypeExpr = {
	t match 
		case NumType => NumType
		case CharType => CharType
		case VarType(v) if vars.contains(v) => VarType(v)

		// user-defined type without arguments
		// (this will have incorrectly been parsed as a VarType)
		case VarType(v) if env.contains(v) =>

			// check that the constructor indeed requires 0 arguments
			val expectedArity = env(v).argTypes.length
			if (expectedArity != 0) throw new TypeDefException(s"Type $v expects $expectedArity arguments")

			// return the correct ConstructorType
			ConstructorType(v, Nil)

		// an unknown type variable
		case VarType(v) => throw new TypeDefException(s"Unknown type variable: $v")

		case ConstructorType(id, args) =>
			// look for constructor information in environment
			val definition = env.getOrElse(id, throw new TypeDefException(s"Unknown type constructor: $id"))

			// check that the correct number of type arguments have been supplied
			val expectedArity = definition.argTypes.length

			if (args.length != expectedArity) throw new TypeDefException(s"Type $id expects $expectedArity arguments, " + s"but received ${args.length}")

			// return resolved constructor type
			ConstructorType(id, args.map(resolveType(_, env, vars)))

		case FunType(arg, ret) => FunType(resolveType(arg, env, vars), resolveType(ret, env, vars))

		case other => throw new TypeDefException(s"Invalid type in type definition: $other")
	
}


