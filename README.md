# FemtoML [WIP] 

TODO: description of language

FemtoML is a "minimal" subset of [PicoML](https://github.com/sufrin/PicoML).




## Prerequisites
- [Scala](https://www.scala-lang.org/download/) version 3.9.0 or above 
- [ScalaLR](https://github.com/sufrin/ScalaLR) for parser generation


## Building the parser
ScalaLR is used to generate the parser files. See `femtoml-parser.scalalr` for the reduction rules. These files are already included in this project under `generated/`, but you can re-generate them using this command:  
`PATH_TO_SCALALR/Assets/scalalr femtoml-parser.scalalr`

Compile the parser files:  
`scalac   -classpath PATH_TO_SCALALR/Assets/scalalrruntime.jar   -d .   AST.scala   FemtoMLScanner.scala   generated/components/*.scala`




## Interpreter 

TODO: description of type-checker and interpreter; example output

Building the interpreter:
```sh
scalac   -classpath ../ScalaLR/Assets/scalalrruntime.jar   -d .   AST.scala   FemtoMLScanner.scala   generated/components/*.scala   datatype.scala infer.scala typeannotations.scala type.scala typechecker.scala semantics.scala femtoml.scala
```
Running the interpreter: 
```sh
scala run --classpath ".:../ScalaLR/Assets/scalalrruntime.jar" --main-class FemtoML
```





