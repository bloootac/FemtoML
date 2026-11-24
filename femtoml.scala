object FemtoML {

	import femtoml._
	import femtoml.Scanner._
	import femtoml.Components._
	import femtoml.Semantics._
	import femtoml.TypeChecker._


	import org.sufrin.scalalr._
	import org.sufrin.scalalr.LRParser._
	import org.sufrin.utility._
	import org.sufrin.utility.PrettyPrint._

	import scala.collection.mutable.Map

	import java.nio.file.{Files, Paths}

	import Console.{GREEN, RED, BLUE, RESET}


	val error_str = s"${RED}ERROR:${RESET}"
	val femtoml_str = s"${BLUE}femtoml>${RESET} "

	// interpreter's main entry point
	def main(args: Array[String]): Unit = {
		
		//attempt to import and run file if one is given
		var (env, typenv) = if (!args.isEmpty) loadFiles(args) else (Map[String, Value](), emptyTypeContext)
		
		//run interactive interpreter 
		runInTerminal(env, typenv)
		
	}

	// terminal interface for interpreter
	def runInTerminal(env: Env = Map.empty, tenv: TypeContext): Unit = {
		// TODO: allow multi-line statements

		var currentTypeEnv = tenv
		
		while (true) 
			// collect one-line user input
			print(femtoml_str)
			val program = scala.io.StdIn.readLine()
			
			// parse, type-check, and interpret input
			val res = parseTypeInterpret(program, (tc, xs) => printTypeEnv(tc, () => { for (x <- xs) println(x); currentTypeEnv = tc }), env, currentTypeEnv)
				
			// if an error occured, print it
			res match
				case Left(_)      => ()
				case Right(err)   => println(s"$error_str $err")
				

			
	}

	// run interpreter on multiple files
	def loadFiles(filePaths: Array[String]): (Env, TypeContext) = {

		// initialise environments
		var env : Env = Map.empty[String, Value]
		var typenv = emptyTypeContext

		for (file <- filePaths) 
		do
			val path = Paths.get(file)
			
			if (Files.exists(path)) 
				// retrieve file content
				val content = scala.io.Source.fromFile(file).getLines().mkString("\n")
				// attempt to parse, type-check, and interpret the file 
				val res = parseTypeInterpret(content, (ts, xs) => { for (x <- xs) println(x); typenv = ts }, env, typenv)
				
				// print out success/error message
				res match
					case Left(_)      => println(s"Loaded $file")
					case Right(err)   => println(s"$error_str: File $file could not be loaded. $err")
				
			else
				println(s"$error_str: File $file not found.")
				
		println("Running interpreter in terminal...")
		(env, typenv)
	}

	// given a program string, attempt to parse, type-check, and interpret it
	def parseTypeInterpret[A](program: String, cont: (TypeContext, List[Value]) => A, env: Env, typenv: TypeContext): Either[A, Throwable] = {
	
		// initialise scanner and parser
		val scanner = new FemtoMLScanner(SourceTextCursor(program))
		val parser  = org.sufrin.scalalr.LRParser.Pull[Token](Components)(scanner.sourceLocation)
		
		try {
			// run parser on input
			parser.run(scanner.next) match {
				case ACCEPTED(value) => 
				
					// Uncomment to print AST:
					//value.prettyPrint()
					//println("***")
					
					value match
						case p : Program => 
							try
								// run type checker on program
								runTypeChecker(p, typenv, typenv2 => 
									// run interpreter if type checking is successful
									evalProgram(p, env, vs => Left(cont(typenv2, vs))))

							catch
								case s => Right(s)
							
				// print error message if parsing failed
				case ERRONEOUS(diagnosis) => Right(new Exception(diagnosis))
				case _                    => Right(new Exception)
			}
			
		} catch {
				case err                  => Right(err)

		}
	}

}



