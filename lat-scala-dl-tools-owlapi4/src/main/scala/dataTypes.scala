package de.tu_dresden.inf.lat.prettyPrinting.datatypes

//import com.dongxiguo.fastring.Fastring.Implicits._


import com.typesafe.scalalogging.Logger


import de.tu_dresden.inf.lat.prettyPrinting.tools.MultiSet


abstract class Expression { 
  def signature: Set[String]
  def atomicConcepts: Set[String]
  def roleSymbols: Set[String]
  def roles: Set[Role] = Set()
  def size: Int
  def subConcepts: MultiSet[Concept]

  def foreachNested(function: Expression => Unit): Unit = { 
    function(this)
  }
}

abstract class Concept extends Expression

object TopConcept extends Concept { 
  override def toString = "TOP"
  override def signature = Set() //Set("TOP")
  override def atomicConcepts = Set()
  override def roleSymbols = Set()
  override def size = 1
  override def subConcepts = MultiSet(TopConcept)
  
}

object BottomConcept extends Concept { 
  override def toString = "BOTTOM"
  override def signature = Set() //Set("BOTTOM")
  override def atomicConcepts = Set()
  override def roleSymbols = Set()
  override def size = 1
  override def subConcepts = MultiSet(BottomConcept)
}

trait Symbol

case class BaseConcept(name: String) extends Concept with Symbol { 
  override def toString = name
  override def signature = Set(name)
  override def atomicConcepts = Set(name)
  override def roleSymbols = Set()
  override def size = 1
  override def subConcepts = MultiSet(this)
}

case class ConceptComplement(concept: Concept) extends Concept { 
  override def toString = "-"+concept.toString
  override def signature = concept.signature
  override def atomicConcepts = concept.atomicConcepts
  override def roleSymbols = concept.roleSymbols
  override def size = concept.size+1
  override def subConcepts = concept.subConcepts+this

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    concept.foreachNested(function)
  }

  override def roles = concept.roles
}

case class ConceptConjunction(var conjuncts: Seq[Concept]) extends Concept { 
  override def toString = conjuncts.mkString("(", " n ", ")")
  override def signature = conjuncts.toSet[Concept].flatMap(_.signature)
  override def atomicConcepts = conjuncts.toSet[Concept].flatMap(_.atomicConcepts)
  override def roleSymbols = conjuncts.toSet[Concept].flatMap(_.roleSymbols)
  override def size = conjuncts.map(_.size).foldLeft(0)((a,b)=> a+b) + conjuncts.size - 1
  override def subConcepts = conjuncts.toSet[Concept].map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_) + this

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    conjuncts.foreach(_.foreachNested(function))
  }

  override def roles = conjuncts.toSet[Concept].flatMap(_.roles)
}

case class ConceptDisjunction(var disjuncts: Seq[Concept]) extends Concept { 
  override def toString = disjuncts.mkString("(", " u ", ")")
  override def signature = disjuncts.toSet[Concept].flatMap(_.signature)
  override def atomicConcepts = disjuncts.toSet[Concept].flatMap(_.atomicConcepts)
  override def roleSymbols = disjuncts.toSet[Concept].flatMap(_.roleSymbols)
  override def size = disjuncts.map(_.size).foldLeft(0)((a,b)=> a+b) + disjuncts.size - 1
  override def subConcepts = disjuncts.toSet[Concept].map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_) + this

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    disjuncts.foreach(_.foreachNested(function))
  }

  override def roles = disjuncts.toSet[Concept].flatMap(_.roles)
}

case class NominalSet(nominals: Seq[Individual]) extends Concept { 
  override def toString = nominals.mkString("{", ", ", "}")
  override def signature = Set()
  override def atomicConcepts = Set()
  override def roleSymbols = Set()
  override def size = nominals.size*2-1
  override def subConcepts = MultiSet(this)
}

abstract class Role extends Expression { 
  override def roleSymbols = signature
  override def atomicConcepts = Set()
  override def subConcepts = MultiSet()
  override def roles = Set(this)
}

case class BaseRole(name: String) extends Role with Symbol { 
  override def toString = name
  override def signature = Set(name)
  override def size = 1
}

object TopRole extends BaseRole("TOP")

case class InverseRole(role: Role) extends Role { 
  override def toString = "("+role.toString+")^-1"
  override def signature = role.signature
  override def size = role.size+1
  
  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
  }
}

case class RoleConjunction(cs: Iterable[Role]) extends Role { 
  override def toString = cs.mkString("(", " n ", ")")
  override def signature = cs.flatMap(_.signature).toSet[String]
  override def size = cs.map(_.size).reduce((a,b)=> a+b) + cs.size - 1

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    cs.foreach(_.foreachNested(function))
  }
}

case class RoleDisjunction(ds: Iterable[Role]) extends Role { 
  override def toString = ds.mkString("(", " u ", ")")
  override def signature = ds.flatMap(_.signature).toSet[String]
  override def size = ds.map(_.size).reduce((a,b)=> a+b) + ds.size - 1

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    ds.foreach(_.foreachNested(function))
  }
}

case class RoleComplement(role: Role) extends Role { 
  override def toString = "¬"+role.toString
  override def signature = role.signature
  override def size = role.size + 1

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
  }
}

case class ExistentialRoleRestriction(role: Role, filler: Concept) extends Concept { 
  override def toString = "E" + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}

case class UniversalRoleRestriction(role: Role, filler: Concept) extends Concept { 
  override def toString = "A" + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}

case class MinNumberRestriction(number: Int, role: Role, filler: Concept) extends Concept { 
  //  assert(number>=1)
  assert(number>=0)
  override def toString = ">=" + number.toString + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size // + number
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}

case class MaxNumberRestriction(number: Int, role: Role, filler: Concept) extends Concept { 
  assert(number>=0)
  override def toString = "=<" + number.toString + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size // + number // unary encoding
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}

case class EqNumberRestriction(number: Int, role: Role, filler: Concept) extends Concept { 
  assert(number>=0)
  override def toString = "=" + number.toString + role + "." + filler
  override def signature = role.signature ++ filler.signature
  override def atomicConcepts = filler.atomicConcepts
  override def roleSymbols = role.signature ++ filler.roleSymbols
  override def size = 1 + role.size + filler.size // + number // unary encoding
  override def subConcepts = filler.subConcepts + this

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
    filler.foreachNested(function)
  }

  override def roles = filler.roles + role
}



abstract class DLStatement extends Expression

abstract class Axiom extends DLStatement

case class Subsumption(subsumer: Concept, subsumee: Concept) extends Axiom { 
  override def toString = subsumer + " <= " + subsumee 
  override def signature = subsumer.signature ++ subsumee.signature
  override def atomicConcepts = subsumer.atomicConcepts ++ subsumee.atomicConcepts
  override def roleSymbols = subsumer.roleSymbols ++ subsumee.roleSymbols
  override def size = subsumer.size + subsumee.size + 1
  override def subConcepts = subsumer.subConcepts ++ subsumee.subConcepts

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    subsumer.foreachNested(function)
    subsumee.foreachNested(function)
  }

  override def roles = subsumer.roles ++ subsumee.roles
}

case class ConceptEquivalence(concepts: Seq[Concept]) extends Axiom {
  override def toString = concepts.mkString(" = ")
  override def signature = concepts.toSet[Concept].flatMap(_.signature)
  override def atomicConcepts = concepts.toSet[Concept].flatMap(_.atomicConcepts)
  override def roleSymbols = concepts.toSet[Concept].flatMap(_.roleSymbols)
  override def size = concepts.map(_.size).sum
  override def subConcepts =
    concepts.toSet[Concept].map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    concepts.foreach(_.foreachNested(function))
  }

  override def roles = concepts.toSet[Concept].flatMap(_.roles)
}

case class DisjointnessAxiom(concepts: Seq[Concept]) extends Axiom {
  override def toString = "disjoint("+concepts.mkString(", ")+")"
  override def signature = concepts.toSet[Concept].flatMap(_.signature)
  override def atomicConcepts = concepts.toSet[Concept].flatMap(_.atomicConcepts)
  override def roleSymbols = concepts.toSet[Concept].flatMap(_.roleSymbols)
  override def size = concepts.map(_.size).sum
  override def subConcepts =
    concepts.toSet[Concept].map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    concepts.foreach(_.foreachNested(function))
  }

  override def roles = concepts.toSet[Concept].flatMap(_.roles)

}

case class DomainAxiom(role: Role, concept: Concept) extends Axiom {
  override def toString = "domain("+role+") = "+concept
  override def signature = role.signature ++ concept.signature
  override def atomicConcepts = concept.atomicConcepts
  override def roleSymbols = role.roleSymbols
  override def size = role.size + concept.size + 1
  override def subConcepts = concept.subConcepts

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
    concept.foreachNested(function)
  }


  def toSubsumption = 
    Subsumption(
      ExistentialRoleRestriction(role, TopConcept),
      concept)
}

case class RangeAxiom(role: Role, concept: Concept) extends Axiom {
  override def toString = "range("+role+") = "+concept
  override def signature = role.signature ++ concept.signature
  override def atomicConcepts = concept.atomicConcepts
  override def roleSymbols = role.roleSymbols
  override def size = role.size + concept.size + 1
  override def subConcepts = concept.subConcepts

  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    role.foreachNested(function)
    concept.foreachNested(function)
  }

  def toSubsumption =
    Subsumption(TopConcept,
	  UniversalRoleRestriction(role, concept))
}


case class TBox(var axioms: Set[Axiom]) extends DLStatement { 
  def add(axiom: Axiom) = axioms = axioms + axiom

  override def toString = axioms.mkString("\n")
  override def signature = axioms.flatMap(_.signature).toSet[String]
  override def atomicConcepts = axioms.flatMap(_.atomicConcepts).toSet[String]
  override def roleSymbols = axioms.flatMap(_.roleSymbols).toSet[String]

  def isEmpty = axioms.isEmpty

  override def size =  
    axioms.toSeq.map(_.size).foldLeft(0)((a,b) => a+b)
		     
  override def subConcepts = axioms.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    axioms.foreach(_.foreachNested(function))
  }

  override def roles = axioms.flatMap(_.roles)
}

abstract class RoleAxiom extends DLStatement { 
  override def atomicConcepts = Set()
  override def subConcepts = MultiSet()
}

case class RoleSubsumption(subsumer: Role, subsumee: Role) extends RoleAxiom { 
  override def toString = subsumer.toString+ " <r "+subsumee.toString
  override def signature = subsumer.signature ++ subsumee.signature
  override def roleSymbols = subsumer.signature ++ subsumee.signature
  override def size = subsumer.size + subsumee.size

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    subsumer.foreachNested(function)
    subsumee.foreachNested(function)
  }

  override def roles = subsumer.roles ++ subsumee.roles
}

case class TransitiveRoleAxiom(role: Role) extends RoleAxiom { 
  override def toString = "trans("+role+")"
  override def signature = role.signature
  override def roleSymbols = role.signature
  override def size = role.size+1

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
  }

  override def roles = role.roles
}

case class SymmetricRoleAxiom(role: Role) extends RoleAxiom { 
  override def toString = "trans("+role+")"
  override def signature = role.signature
  override def roleSymbols = role.signature
  override def size = role.size+1

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
  }

  override def roles = role.roles
}


case class FunctionalRoleAxiom(role: Role) extends RoleAxiom {
  override def toString = "func("+role+")"
  override def signature = role.signature
  override def roleSymbols = role.signature
  override def size = role.size+1

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
  }

  override def roles = role.roles
}

case class ComplexRoleSubsumption(sub: List[Role], sup: Role) extends RoleAxiom {
  override def toString = sub.mkString(" o ")+" <r "+sup
  override def signature = sub.toSet[Role].flatMap(_.signature) ++ sup.signature
  override def roleSymbols = sub.toSet[Role].flatMap(_.signature) ++ sup.signature
  override def size = sub.map(_.size).foldLeft(0)((a,b) => a+b) + sub.size + sup.size
  
  override def foreachNested(function: Expression => Unit) = {
    super.foreachNested(function)
    sub.foreach(_.foreachNested(function))
    sup.foreachNested(function)
  }
  
  override def roles = sub.toSet[Role].flatMap(_.roles) ++ sup.roles
}


case class RBox(var axioms: Set[RoleAxiom]) extends DLStatement { 
  def add(axiom: RoleAxiom) = axioms = axioms + axiom

  override def toString = axioms.mkString("\n")
  override def signature = axioms.flatMap(_.signature).toSet[String]
  override def roleSymbols = axioms.flatMap(_.roleSymbols).toSet[String]
  override def atomicConcepts = Set()
  override def subConcepts = MultiSet()

  def isEmpty = axioms.isEmpty

  override def size =  
    axioms.toSeq.map(_.size).foldLeft(0)((a,b) => a+b)  

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    axioms.foreach(_.foreachNested(function))
  }

  override def roles = axioms.flatMap(_.roles)
}

case class Individual(name: String) extends Expression { 
  override def toString = name

  override def signature = Set()
  override def atomicConcepts = Set()
  override def roleSymbols = Set()
  override def size = 1
  override def subConcepts = MultiSet()
}


abstract class Assertion extends DLStatement

case class ConceptAssertion(concept: Concept, individual: Individual) extends Assertion { 
  override def toString = concept.toString+"("+individual.toString+")"
  override def signature = concept.signature ++ individual.signature
  override def atomicConcepts = concept.atomicConcepts ++ individual.atomicConcepts
  override def roleSymbols = concept.roleSymbols ++ individual.roleSymbols
  override def size = concept.size + individual.size
  override def subConcepts = concept.subConcepts ++ individual.subConcepts

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    concept.foreachNested(function)
    individual.foreachNested(function)
  }  

  override def roles = concept.roles
}

/**
 * For the results of ABox forgetting
 */
case class DisjunctiveConceptAssertion(cas: Set[ConceptAssertion]) extends Assertion { 
  override def toString = cas.mkString(" v ")
  override def signature = cas.flatMap(_.signature)
  override def atomicConcepts = cas.flatMap(_.atomicConcepts)
  override def roleSymbols = cas.flatMap(_.roleSymbols)
  override def size = cas.map(_.size).sum + cas.size-1
  assert(cas.size>0, "unsupported dca: "+toString+" inconsistent ontology?") 
  override def subConcepts = cas.map(_.subConcepts).toSet[MultiSet[Concept]].foldLeft(MultiSet[Concept])(_++_)

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    cas.foreach(_.foreachNested(function))
  }
  
  override def roles = cas.flatMap(_.roles)
}

case class RoleAssertion(role: Role, individual1: Individual, individual2: Individual) extends Assertion { 
  override def toString = role.toString+"("+individual1+", "+individual2+")"
  override def signature = role.signature
  override def atomicConcepts = Set()
  override def subConcepts = MultiSet()
  override def roleSymbols = role.signature
  override def size = role.size + 2

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    role.foreachNested(function)
  }

  override def roles = role.roles
}

case class ABox(var assertions: Set[Assertion]) extends DLStatement { 
  override def toString = assertions.mkString("\n")
  override def signature = assertions.flatMap(_.signature)
  override def atomicConcepts = assertions.flatMap(_.atomicConcepts)
  override def roleSymbols = assertions.flatMap(_.roleSymbols)
  override def subConcepts = assertions.map(_.subConcepts).foldLeft(MultiSet[Concept])(_++_)

  def isEmpty = assertions.isEmpty

  override def size =  
    assertions.toSeq.map(_.size).foldLeft(0)((a,b) => a+b)

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    assertions.foreach(_.foreachNested(function))
  }

  override def roles = assertions.flatMap(_.roles)
}

object Ontology { 
  def buildFrom(statements: Iterable[DLStatement]): Ontology = { 
    val result = new Ontology()
    statements.foreach(result.addStatement)
    result
  }
}

class Ontology(var tbox:TBox = new TBox(Set()), 
	       var abox: ABox = new ABox(Set()), 
	       var rbox: RBox = new RBox(Set())) extends DLStatement { 

  def this(tbox: TBox, abox: ABox) = this(tbox, abox, new RBox(Set()))

  def this() = this(new TBox(Set()), new ABox(Set()))

  def isEmpty = tbox.isEmpty && abox.isEmpty && rbox.isEmpty


  def addStatement(statement: DLStatement) = statement match { 
    case a: Assertion => abox.assertions = abox.assertions + a
    case a: Axiom => tbox.axioms = tbox.axioms + a
    case a: RoleAxiom => rbox.axioms = rbox.axioms + a
  }

  def addStatements(statements: Iterable[DLStatement]) = 
    statements.foreach(addStatement)

  def statements: Iterable[DLStatement] = tbox.axioms ++ rbox.axioms ++ abox.assertions

  override def toString = "TBox:\n" + tbox.toString + "\n\nRBox:\n" + rbox.toString+"\n\n"+ "\n\nABox:\n" + abox.toString+"\n\n"
  override def signature = tbox.signature ++ rbox.signature ++ abox.signature
  override def atomicConcepts = tbox.atomicConcepts ++ abox.atomicConcepts
  override def roleSymbols = tbox.roleSymbols ++ rbox.roleSymbols ++ abox.roleSymbols
  override def subConcepts = tbox.subConcepts ++ rbox.subConcepts ++ abox.subConcepts

  override def foreachNested(function: Expression => Unit) = { 
    super.foreachNested(function)
    tbox.foreachNested(function)
    abox.foreachNested(function)
    rbox.foreachNested(function)
  }

  override def roles = tbox.roles ++ rbox.roles ++ abox.roles		 
  
  override def equals(other: Any) = other match { 
    case other: Ontology =>    tbox==other.tbox && abox==other.abox && rbox==other.rbox
    case _ => false
  }

  override def hashCode = 
    13*tbox.hashCode+abox.hashCode+rbox.hashCode

  def size: Int = tbox.size+abox.size+rbox.size


  def remove(dlStatement: DLStatement) = dlStatement match { 
    case a: Axiom => tbox.axioms -= a
    case a: Assertion => abox.assertions -= a
    case ra: RoleAxiom => rbox.axioms -= ra
  }
}

object DLHelpers { 
  
  //implicit val (logger, formatter, appender) = ZeroLoggerFactory.newLogger(this)
  //import formatter._
  val logger = Logger(DLHelpers.getClass)

  def nnf(ontology: Ontology): Ontology =
    new Ontology(tbox = nnf(ontology.tbox), 
		 abox = nnf(ontology.abox),
		 rbox = ontology.rbox)

  def nnf(tbox: TBox): TBox = new TBox(tbox.axioms.flatMap(nnf))

  def nnf(axiom: Axiom): Set[Subsumption] = axiom match { 
    case ConceptEquivalence(concepts) =>
      concepts.toSet[Concept].flatMap(c1 => (concepts.toSet-c1).flatMap(c2 => nnf(Subsumption(c1,c2))))
  }

  def nnf(abox: ABox): ABox = new ABox(abox.assertions.flatMap(nnf))

  def nnf(assertion: Assertion): Set[Assertion] = assertion match { 
    case _: RoleAssertion => Set(assertion)
    case ConceptAssertion(c, a) => Set(ConceptAssertion(nnf(c), a))
    case DisjunctiveConceptAssertion(cas) => 
      Set(DisjunctiveConceptAssertion(cas.flatMap(ca => nnf(ca).map(_.asInstanceOf[ConceptAssertion]))))
  }


  def nnf(concept: Concept): Concept = concept match { 
    case ConceptComplement(TopConcept) => BottomConcept
    case ConceptComplement(BottomConcept) => TopConcept
    case ConceptComplement(ExistentialRoleRestriction(r, f)) => 
      UniversalRoleRestriction(r, nnf(ConceptComplement(f)))
    case ConceptComplement(UniversalRoleRestriction(r, f)) => 
      ExistentialRoleRestriction(r, nnf(ConceptComplement(f)))
    case ConceptComplement(MinNumberRestriction(n, r, f)) => 
      MaxNumberRestriction(n-1, r, ConceptComplement(nnf(ConceptComplement(f))))
    case ConceptComplement(MaxNumberRestriction(n, r, f)) => 
      MinNumberRestriction(n+1, r, nnf(f))
    case ConceptComplement(ConceptConjunction(cs)) => 
      ConceptDisjunction(cs.map(f => nnf(ConceptComplement(f))))
    case ConceptComplement(ConceptDisjunction(ds)) => 
      ConceptConjunction(ds.map(f => nnf(ConceptComplement(f))))
    case ConceptComplement(ConceptComplement(f)) => nnf(f)
    case ConceptComplement(f) => ConceptComplement(nnf(f))    
    case ExistentialRoleRestriction(r, f) => ExistentialRoleRestriction(r, nnf(f))
    case UniversalRoleRestriction(r, f) => UniversalRoleRestriction(r, nnf(f))
    case MinNumberRestriction(n, r, f) => MinNumberRestriction(n, r, nnf(f))
    case MaxNumberRestriction(n, r, f) => 
      MaxNumberRestriction(n, r, ConceptComplement(nnf(ConceptComplement(f))))
    case ConceptConjunction(cs) => ConceptConjunction((cs.toSet[Concept].map(nnf)-TopConcept).toSeq)
    case ConceptDisjunction(ds) => ConceptDisjunction((ds.toSet[Concept].map(nnf)-BottomConcept).toSeq)
    case b: BaseConcept => b
    case TopConcept => TopConcept
    case BottomConcept => BottomConcept
    case _: NominalSet => concept
  }

  def disjoint(c1: Concept, c2: Concept) =
    new Subsumption(ConceptConjunction(Seq(c1, c2)), BottomConcept)
//    new Subsumption(c1, new ConceptComplement(c2))



  def neg(concept: Concept): Concept = concept match { 
    case TopConcept => BottomConcept
    case BottomConcept => TopConcept
    case c: BaseConcept => ConceptComplement(c)
    case ConceptComplement(f) => f
    case ConceptDisjunction(fs) => ConceptConjunction(fs.map(neg))
    case ConceptConjunction(fs) => ConceptDisjunction(fs.map(neg))
    case UniversalRoleRestriction(r, f) => ExistentialRoleRestriction(r, neg(f))
    case ExistentialRoleRestriction(r, f) => UniversalRoleRestriction(r, neg(f))
    case MinNumberRestriction(n, r, f) => MaxNumberRestriction(n-1, r, f)
    case MaxNumberRestriction(n, r, f) => MinNumberRestriction(n+1, r, f)
  } 

  def conjunction(concepts: Iterable[Concept]): Concept = { 

    if(concepts.exists(c1 => concepts.exists(c2 => c1==neg(c2))))
      return BottomConcept
    if(concepts.exists(_==BottomConcept))
      return BottomConcept

    var set = concepts.toSet[Concept] - TopConcept
    if(set.isEmpty)
      TopConcept
    else if(set.size==1)
      set.head
    else { 
      // flatten
      while(set.exists(_.isInstanceOf[ConceptConjunction])) { 
	val nested = (set.collectFirst{ case c:ConceptConjunction => c }).get.asInstanceOf[ConceptConjunction]
	set -= nested
	set ++= nested.conjuncts
      }
      
      ConceptConjunction(set.toSeq)
    }
  }  

  def conjunction(roles: Iterable[Role]): Role = { 
    if(roles.size==1)
      roles.head
    else if(roles.size>1)
      new RoleConjunction(roles)
    else { 
      assert(false, "Not implemented yet!")
      null
    }
  }

  
  def disjunction(c1: Concept, c2: Concept): Concept = 
    disjunction(Set(c1, c2))

  def disjunction(concepts: Iterable[Concept]): Concept = { 
    if(concepts.toSet(TopConcept))
      return TopConcept
    if(concepts.exists(c1 => concepts.exists(c2 => c1==neg(c2))))
      return TopConcept

    var set = concepts.toSet[Concept] - BottomConcept
    if(set.isEmpty)
      BottomConcept
    else if(set.size==1)
      set.head
    else { 
      //flatten
      while(set.exists(_.isInstanceOf[ConceptDisjunction])) { 
	val nested = (set.collectFirst{ case c:ConceptDisjunction => c }).get.asInstanceOf[ConceptDisjunction]
	set -= nested
	set ++= nested.disjuncts
      }

      ConceptDisjunction(set.toSeq) 
    }
  }  // <---- looks correct

  def disjunction(roles: Iterable[Role]): Role = { 
    if(roles.size==1)
      roles.head
    else if(roles.size>1)
      new RoleDisjunction(roles)
    else { 
      assert(false, "Not implemented yet!")
      null
    }
  }

  def inverse(role: Role): Role = role match { 
    case r: BaseRole => InverseRole(r)
    case InverseRole(r: BaseRole) => r
    case InverseRole(InverseRole(r)) => inverse(r)
    case _ => assert(false, "complex roles not supported"); null
  }

  def inverseOf(role1: Role, role2: Role): Boolean = 
    inverse(role1)==inverse(inverse(role2))

  // General simplifications
  def simplify(ont: Ontology): Ontology = { 
    logger.info(s"Simplifying ontology of ${ont.statements.size} statements.")
    new Ontology(simplify(ont.tbox), simplify(ont.abox), ont.rbox)
  }
  def simplify(tbox: TBox): TBox = { 
    var axioms = tbox.axioms.map(simplify)
    axioms = axioms.filterNot(_ match { 
      case Subsumption(BottomConcept, _) => true
      case Subsumption(_, TopConcept) => true
      case _ => false
    })
    new TBox(axioms)
}
  def simplify(abox: ABox): ABox = new ABox(abox.assertions.map(simplify))
  def simplify(axiom: Axiom): Axiom = { 
    logger.trace(s"Simplifying ${axiom}") 
    axiom match { 
      case Subsumption(c1, c2) => Subsumption(simplify(c1), simplify(c2))
      case ConceptEquivalence(cs) => ConceptEquivalence(cs.map(simplify))
    }
  }

  def simplify(assertion: Assertion): Assertion = assertion match { 
    case ConceptAssertion(c, i) => ConceptAssertion(simplify(c), i)
    case DisjunctiveConceptAssertion(ds) => DisjunctiveConceptAssertion(ds.map((simplify(_).asInstanceOf[ConceptAssertion])))
    case r: RoleAssertion => r
  }

  def simplify(concept: Concept): Concept = concept match { 
    case ConceptComplement(c) => neg(simplify(c))
    case ConceptConjunction(cs) => { 
      if(cs.size==1)
	simplify(cs.head)
      else if(cs.isEmpty)
	BottomConcept
      else { 
	val csX = cs.filter{ _ match { 
	  case ConceptDisjunction(ds) => !ds.toSet.exists(cs.toSet) // redundant then
	  case _ => true
	}}
	if(csX.forall(d => d.isInstanceOf[ConceptDisjunction])){ 
	  // (A u B u C) n (A u B u D) => (A u B u (C n D)), if reasonable

	  val diss = csX.map(_.asInstanceOf[ConceptDisjunction])
	  var overlap = diss.toSeq(0).disjuncts.toSet
	  diss.foreach { dis =>
	    overlap = overlap.filter(dis.disjuncts.contains)
		      }
	  if(overlap.isEmpty)
	    conjunction(csX.map(simplify))
	  else disjunction(overlap.map(simplify) + conjunction(
	    diss.map(dis => simplify(ConceptDisjunction((dis.disjuncts.toSet--overlap).toSeq)))))
	}      
	  else
	    conjunction(csX.map(simplify))
      }
    }
    case ConceptDisjunction(ds) => { 
      if(ds.size==1)
	simplify(ds.head)
      else
	disjunction(ds.map(simplify))
    }
    case UniversalRoleRestriction(r1, ExistentialRoleRestriction(r2, c)) if inverseOf(r1, r2) =>
      simplify(c)

    case ExistentialRoleRestriction(r, c) => 
      val filler = simplify(c)
      if(filler==BottomConcept)
	BottomConcept
      else
	ExistentialRoleRestriction(r, filler)
    case UniversalRoleRestriction(r, c) => 
      val filler = simplify(c)
      if(filler==TopConcept)
	TopConcept
      else
	UniversalRoleRestriction(r, filler)
    case MinNumberRestriction(1, r, c) => simplify(ExistentialRoleRestriction(r,c))
    case MaxNumberRestriction(0, r, ConceptComplement(c)) => simplify(UniversalRoleRestriction(r,c))
    case MinNumberRestriction(n, r, c) => { 
      val filler = simplify(c)
      if(filler==BottomConcept)
	BottomConcept
      else
	MinNumberRestriction(n, r, filler)
    }
    case MaxNumberRestriction(n, r, c) => { 
      val filler = simplify(c)
      if(filler==BottomConcept)
	TopConcept
      else
	MaxNumberRestriction(n, r, simplify(c))
    }
    case c => c
  }

  /**
   * Splits subsumptions of the kind C <= (C1 n C2) to subsumptions C <= C1, C <= C2
   */
  def splitConjunctions(ontology: Ontology) = { 
    val result = new Ontology(abox=ontology.abox, rbox=ontology.rbox)
    val axioms = ontology.tbox.axioms.flatMap{_ match {  
      case Subsumption(c, ConceptConjunction(cs)) => cs.map(Subsumption(c,_)).toSet[Axiom]
      case other => Set(other)
    }}
    result.tbox = TBox(axioms)
    result
  }
}
