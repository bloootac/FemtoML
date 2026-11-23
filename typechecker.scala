//point of contact for type.scala, datatype.scala, infer.scala
package femtoml

import femtoml.DataType
import femtoml.Type
import femtoml.AST


object TypeChecker

def emptyTypeContext = TypeContext(Map.empty, Map.empty, Map.empty)

def runTypeChecker[A](p: Program, env: TypeContext, cont: TypeContext => A) : A = {
	
	// check user-defined datatypes 
	checkDataTypes(p, {case (defenv, conenv) => 
	
	// augment environment with user-defined datatypes and constructors 
		val env2 = TypeContext(env.variables, env.constructors ++ conenv, env.typeDefs ++ defenv)
	
	// type the program in this environment
		typeProgram(p, env2, cont)})
}




def printTypeEnv[A](env: TypeContext, cont: () => A) : A = {
	env match
		case TypeContext(vars, constructors, typeDefs) =>
			for ((k, v) <- vars) println(s"$k : $v")
			for ((k, v) <- constructors) println(s"$k : $v")
			for ((k, v) <- typeDefs) println(s"$k : $v")
			cont()
}
