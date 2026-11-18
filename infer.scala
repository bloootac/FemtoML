package femtoml 

import femtoml.AST

object Infer

/*
	this object contains the main components of the type inference engine.
*/

case class TypeInferenceException(message: String) extends Exception(message)


// ============================= unification ============================= 

// given two types, attempt to equate them while solving type constraints on unknowns as a side effect
def unify(t1: TypeExpr, t2: TypeExpr) : Unit = {

	// find constrained types of t1 and t2
	var ty1 = typeclass(t1)
	var ty2 = typeclass(t2)

	(ty1, ty2) match
		case (NumType, NumType)                                               => ()
		case (CharType, CharType)                                             => ()
		
		case (ConstructorType(id1, t1s), ConstructorType(id2, t2s))           => 
			// check that constructor names and argument lengths match
			if (t1s.length != t2s.length || id1 != id2) throw new TypeInferenceException(s"Constructor types $ty1 and $ty2 do not match.")
			
			// unify the types of each argument in t1 with its corresponding argument in t2
			t1s.zip(t2s).foreach{ case (a,b) => unify(a,b) }
			
		case (FunType(t1args, t1body), FunType(t2args, t2body))               => 
			// unify argument types
			unify(t1args, t2args)
			
			// unify return types
			unify(t1body, t2body)
			
		// if one of the types is unknown, set it to the other type
		case (x: UnknownType, _)                                              => equate(x, ty2)
		case (_, y: UnknownType)                                              => equate(y, ty1)
		
		case _                                                                => throw new TypeInferenceException(s"Types $ty1 and $ty2 do not match.")
		
}

// map a set type unknown to the type to which it is constrained to be equivalent; map all other types to themselves
def typeclass(texpr: TypeExpr) : TypeExpr = {
	texpr match
		case t1: UnknownType      =>
			t1.texpr match
				case Some(t2) => 
					val t3 = typeclass(t2)
					t1.texpr = Some(t3)
					t3
				case None     => texpr
			
		case _                    => texpr
}

// set the given type unknown to the given type expression, as long as that wouldn't produce an infinite type
def equate(t1: UnknownType, t2: TypeExpr) : Unit = {
	t2 match
		case _: UnknownType if t1 eq t2 => ()
		case _ if !occurs(t1, t2)       => t1.texpr = Some(t2)
		case _                          => throw new TypeInferenceException(s"type $t1 cannot be the same as type $t2")
}


// check whether the first type occurs in the second
def occurs(t1: UnknownType, t2: TypeExpr) : Boolean = {
	typeclass(t2) match 
		case u: UnknownType            => t1 eq t2
		case ConstructorType(_, ts)   => ts.exists(occurs(t1, _))
		case FunType(targ, tbody)      => occurs(t1, targ) || occurs(t1, tbody)
		case _                         => false
}

// ============================= generalisation helpers ============================= 
// generalise() is defined in type.scala as it requires type environments

// given a type expression, generate a list of the unique type unknows that occur in the expression
def unknowns(t: TypeExpr) : List[UnknownType] = collectUnknowns(t).foldLeft(List.empty[UnknownType]) { (acc, u) => if (acc.exists(_ eq u)) acc else acc :+ u }

// given a type expression, generate a list of all unique type unknowns that occur in the expression
def collectUnknowns(t: TypeExpr) : List[UnknownType] = {
	typeclass(t) match
		case u: UnknownType            => List(u)
		case FunType(tf, tx)            => collectUnknowns(tf) ++ collectUnknowns(tx)
		case ConstructorType(_, ts)    => ts.flatMap(collectUnknowns)
		case PolyType(_, body)          => collectUnknowns(body)
		case _                          => Nil  
}

// given a type expression, collect all the identifiers used for types in the expression
def collectNames(t: TypeExpr): Set[String] = {
	t match
		case VarType(id)               => Set(id)
		case FunType(tf, tx)           => collectNames(tf) ++ collectNames(tx)
		case ConstructorType(_, ts)    => ts.flatMap(collectNames).toSet
		case PolyType(vs, body)        => vs.toSet ++ collectNames(body)
		case UnknownType(Some(texpr))  => collectNames(texpr)
		case other                     => Set.empty
}

// given a set of used names and a counter, generate an identifier that is not in the set, along with an updated counter and set
def freshName(usedNames: Set[String], next: Int) : (String, Int, Set[String]) = {

	var name = s"t$next"
	var newNext = next
	
	while (usedNames.contains(name)) {
		newNext += 1
		name = s"t$newNext"
	}
	
	(name, newNext, usedNames+name)
}

// given a type expression and a map from type unknowns to identifiers, return the type expression with all substitutions made
def replace(t: TypeExpr, replacements: List[(UnknownType, String)]) : TypeExpr = {
	typeclass(t) match
	
		case u: UnknownType           =>
			// attempt to find an identifier for the type unknown
			replacements.find({case (v, _) => v eq u}) match {
				// if one exists, return it
				case Some((_, name)) => VarType(name)
				case None            => u
			}
			
		case FunType(tf,tx)           => FunType(replace(tf, replacements), replace(tx, replacements))
		case ConstructorType(id, ts)  => ConstructorType(id, ts.map(t => replace(t, replacements)))
		case PolyType(vs, body)       => PolyType(vs, replace(body, replacements))
		case other                    => other
			
}


