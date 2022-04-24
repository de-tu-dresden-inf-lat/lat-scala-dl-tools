package de.tu_dresden.inf.lat.prettyPrinting.formatting

import java.io.File

import scala.collection.JavaConversions._

import org.semanticweb.owlapi.model._

import de.tu_dresden.inf.lat.prettyPrinting.datatypes._
import de.tu_dresden.inf.lat.prettyPrinting.owlapi.{ OWLParser, OWLApiConverter }


trait Formatter[A] { 
  def format(a: A): String
}

object SimpleOWLFormatter extends SimpleOWLFormatterCl(true, SimpleDLFormatter)

object SimpleOWLFormatterLongNames extends SimpleOWLFormatterCl(false, SimpleDLFormatter)

class SimpleOWLFormatterCl(simplifiedNames: Boolean = true, dlFormatter: SimpleDLFormatterCl = SimpleDLFormatter)
  extends Formatter[OWLObject]  {
  def format(ontology: OWLOntology): String = {
    converter = new OWLApiConverter(simplifiedNames, referenceOntology = Some(ontology))
    ontology.getLogicalAxioms(false).map(format).mkString("\n")
  }

  var converter = new OWLApiConverter(simplifiedNames)

  def setReferenceOntology(ontology: OWLOntology) = {
    converter = new OWLApiConverter(simplifiedNames, referenceOntology = Some(ontology))
  }

  override def format(owlObject: OWLObject) = owlObject match {
      case dca: OWLDisjointClassesAxiom =>
        "disjoint"+
        dca.getClassExpressions.map(
          x => dlFormatter.format(converter.convert(x))).mkString("(",", ",")")
      case axiom: OWLAxiom => {
        val converted = converter.convert(axiom)
        converted.map(dlFormatter.format).mkString("\n")
      }
      case classExpression: OWLClassExpression =>
        val converted = converter.convert(classExpression)
        dlFormatter.format(converted)
      case entity: OWLEntity =>
        converter.getName(entity)
      case other => assert(false, "unsupported: "+other); ""
    }


}


object SimpleDLFormatter extends SimpleDLFormatterCl

/**
  *  simplifyNames is used if full IRIs are still used in the DL objects, but shouldn't be 
  *  shown in the formatting result. Note that when converting OWL to DL objects, not the 
  *  IRI, but some other representation may be used, based on the label of the entity or 
  *  the label of the IRI.  
  */  
class SimpleDLFormatterCl(simplifyNames: Boolean = false) extends Formatter[Expression] {

  def main(args: Array[String]): Unit = { 
    val file = new File(args(0))

    val parser = new OWLParser()

    val ontology = parser.parseFile(file)

    println(format(ontology))
  }

  def format(ontology: Ontology): String = {

    var statements = " TBox: \n"
    statements += ontology.tbox.axioms.toSeq.map(format).sorted.mkString("\n")
    statements += "\n\n RBox: \n" 
    statements += ontology.rbox.axioms.toSeq.map(format).sorted.mkString("\n")
    statements += "\n\n ABox: \n" 
    statements += ontology.abox.assertions.toSeq.map(format).sorted.mkString("\n")

    statements
  }

  override def format(stat: Expression) = stat match { 
    case TopConcept => ""+TOP
    case BottomConcept => ""+BOT
    case BaseConcept(name) => getName(name) 
    case ConceptComplement(c) => NEG + format(c)
//    case ConceptConjunction(cs) => cs.map(format).mkString("(", " \n\t"+SQ_AND+" ", ")")
//    case ConceptDisjunction(cs) => cs.map(format).mkString("(", " \n\t"+SQ_OR+" ", ")")
    case ConceptConjunction(cs) => cs.map(format).mkString("(", " "+SQ_AND+" ", ")")
    case ConceptDisjunction(cs) => cs.map(format).mkString("(", " "+SQ_OR+" ", ")")
    case Individual(name) => getName(name) //name.split('#').last
    case NominalSet(ns) => ns.map(format).mkString("{", ", ", "}")


    case BaseRole(name) => getName(name) //name.split('#').last
    case InverseRole(r) => format(r)+INVERSE
    case RoleConjunction(cs) => cs.map(format).mkString("(", " "+SQ_AND+" ", ")")
    case RoleDisjunction(cs) => cs.map(format).mkString("(", " "+SQ_OR+" ", ")")
    case RoleComplement(c) => NEG + format(c)

    case ExistentialRoleRestriction(r, c) => EXISTS + format(r) + "." + format(c)
    case UniversalRoleRestriction(r, c) => FORALL + format(r) + "." + format(c)
    case MinNumberRestriction(n, r, c) => GEQ + n.toString + format(r) + "." + format(c)
    case MaxNumberRestriction(n, r, c) => LEQ + n.toString + format(r) + "." + format(c)

//    case Subsumption(c, d) => format(c) + " \n\t" + SQ_SUBSETEQ + " " + format(d)
    case Subsumption(c, d) => format(c) + " " + SQ_SUBSETEQ + " " + format(d)
//    case ConceptEquivalence(cs) => cs.map(format).mkString(" \n\t"+SQ_EQUIV+" ")
    case ConceptEquivalence(cs) => cs.map(format).mkString(" "+SQ_EQUIV+" ")
    case DisjointnessAxiom(cs) => cs.map(format).mkString("disjoint(", ", ", ")")
    case DomainAxiom(r,c) => "domain("+format(r)+") = "+format(c)
    case RangeAxiom(r,c) => "range("+format(r)+") = "+format(c)

    case RoleSubsumption(r, q) => format(r) + " " + SQ_SUBSETEQ + " " + format(q)
    case ComplexRoleSubsumption(r, q) => r.map(format).mkString(" "+COMP+" ") + " " + SQ_SUBSETEQ + " " + format(q)
    case TransitiveRoleAxiom(r) => "trans(" + format(r) + ")"
    case SymmetricRoleAxiom(r) => "symmetric("+format(r) + ")"

    case ConceptAssertion(c, i) => format(c) + "(" + format(i) + ")"
    case RoleAssertion(r, i, j) => format(r) + "(" + format(i) + ", " + format(j) + ")"
    case DisjunctiveConceptAssertion(ds) => ds.map(format).mkString(" "+VEE+" ")
  }

  def getName(iri: String) = {
    if(!simplifyNames)
      iri
    else if(iri.contains('#'))
      iri.split('#').last
    else
      iri.split('/').last 
  }

  // Symbols
  val TOP = 0x22A4.toChar
  val BOT = 0x22A5.toChar
  val NEG = 0x00AC.toChar

  val SQ_AND = 0x2293.toChar
  val SQ_OR = 0x2294.toChar
  val EXISTS = 0x2203.toChar
  val FORALL = 0x2200.toChar
  val INVERSE = 0x207B.toChar
  val LEQ = 0x2A7D.toChar
  val GEQ = 0x2A7E.toChar
  val COMP = 0x25CB.toChar

  val SQ_SUBSETEQ = 0x2291.toChar
  val SQ_EQUIV = 0x2261.toChar

  val VEE = 0x2228.toChar
} 
