package femtoml

import femtoml.AST
import femtoml.Infer


/*
	this object defines the functions used to evaluate user-defined algebraic types.
	
	we execute two passes:
		- first, we register the names and arities of all user-defined types in the program
		- then, we register the types of all user-defined type constructors in the program
		
	this approach is used to deal with recursive types.
*/

object DataType

case class TypeDefException(message: String) extends Exception(message)


// main entry point
def checkDataTypes[A](p: Program, cont: (TypeDefEnv, ConstructorEnv) => A): A = {
	// register the names of all new data types, then go back and process all type constructors
	evalDataDecls(p, d => evalConstructors(p, d, c => cont(d, c)))
}


// first pass
def evalDataDecls[A](p: Program, cont: TypeDefEnv => A): A = {

	val reservedTypes = Set("Num", "Char")
	
	p match 
		case EmptyStmt => cont(Map.empty)
		case SeqStmt(h, t) =>
	
			// look through rest of program first, then analyse h
			evalDataDecls(t, rest => h match
				case DataDec(id, vars, _) => 
				
					// check if data type has been defined before or has duplicate type parameters
					if (rest.contains(id)) throw new TypeDefException(s"Type $id is already defined")
					if (reservedTypes.contains(id)) throw new TypeDefException(s"Cannot redefine built-in type $id")
					if (vars.distinct.length != vars.length) throw new TypeDefException(s"Duplicate type parameters in $id")
					
					// add declaration to type definition environment
					cont(rest + (id -> ConstructorType(id, vars.map(VarType(_)))))
					
				case _ => cont(rest)
			)
					
	
}

// second pass
def evalConstructors[A](p: Program, env: TypeDefEnv, cont: ConstructorEnv => A): A = {
	p match 
		case EmptyStmt => cont(Map.empty)

		case SeqStmt(h, t) =>
						
			evalConstructors(t, env, rest =>
				h match
					case DataDec(id, vars, constructors) =>

						// retrieve type associated with identifier
						val data = env.getOrElse(id, throw new TypeDefException(s"Unknown data type: $id"))

						// check that all constructors are distinct 
						val names = constructors.map(_._1)
						if (names.distinct.length != names.length) throw new TypeDefException(s"Duplicate constructors in $id")

						// generate constructor signatures
						resolveConstructorList(constructors, vars, data, env, current => 

							// Check for duplicate constructors across declarations
							val duplicates = current.keySet.intersect(rest.keySet)
							if (duplicates.nonEmpty) throw new TypeDefException(s"Duplicate constructors: ${duplicates.mkString(", ")}")

							cont(current ++ rest)
						)

					case _ => cont(rest)
			)

}

// given a list of constructors, construct their signatures and add them to the environment
def resolveConstructorList[A](cs: List[(String, List[TypeExpr])], vars: List[String], data: ConstructorType, env: TypeDefEnv, cont: ConstructorEnv => A) : A = {
	cs match
		case (cid, cargs)::rest => 
			// resolve the types of the constructor's arguments
			resolveConstructorArgs(cargs, env, vars.toSet, r => 
				// generate a map of all other constructor signatures
				resolveConstructorList(rest, vars, data, env, rs => 
					// add this constructor's signature to the resulting map
					cont(Map(cid -> ConstructorSignature(vars, r, data )) ++ rs)))
		case Nil                => cont(Map.empty)
}

/*

	the types of a constructor's arguments may include incorrectly parsed VarTypes that actually refer to constructors defined earlier in the program.
	the parser does not handle this issue; instead, we fix it here by replacing any VarTypes with their matching ConstructorType.
	
	we check for undefined types and incorrect constructor arguments along the way.
*/
def resolveType[A](t: TypeExpr, env: TypeDefEnv, vars: Set[String], cont: TypeExpr => A): A = {
	t match 
		case NumType                                       => cont(NumType)
		case CharType                                      => cont(CharType)
		case VarType(v) if vars.contains(v)                => cont(VarType(v))

		// user-defined type without arguments
		// (this will have incorrectly been parsed as a VarType)
		case VarType(v) if env.contains(v)                 =>

			// check that the constructor indeed requires 0 arguments
			val expectedArity = env(v).argTypes.length
			if (expectedArity != 0) throw new TypeDefException(s"Type $v expects $expectedArity arguments")

			// return the correct ConstructorType
			cont(ConstructorType(v, Nil))

		// an unknown type variable
		case VarType(v)                                    => throw new TypeDefException(s"Unknown type variable: $v")

		case ConstructorType(id, args)                     =>
			// look for constructor information in environment
			val definition = env.getOrElse(id, throw new TypeDefException(s"Unknown type constructor: $id"))

			// check that the correct number of type arguments have been supplied
			val expectedArity = definition.argTypes.length
			if (args.length != expectedArity) throw new TypeDefException(s"Type $id expects $expectedArity arguments, " + s"but received ${args.length}")

			// return resolved constructor type
			resolveConstructorArgs(args, env, vars, rargs => cont(ConstructorType(id, rargs)))

		// resolve argument type and return type
		case FunType(arg, ret) => resolveType(arg, env, vars, rf => resolveType(ret, env, vars, rx => cont(FunType(rf, rx))))
		
		case other => throw new TypeDefException(s"Invalid type in type definition: $other")
	
}

// helper function for resolving each type in a constructor's list of argument types
def resolveConstructorArgs[A](args: List[TypeExpr], env: TypeDefEnv, vars: Set[String], cont: List[TypeExpr] => A) : A = {
	args match
		case a::as => resolveType(a, env, vars, ra => resolveConstructorArgs(as, env, vars, ras => cont(ra::ras)))
		case Nil   => cont(Nil)
}


