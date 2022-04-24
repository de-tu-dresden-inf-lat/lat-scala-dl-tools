package de.tu_dresden.inf.lat.prettyPrinting.formatting.statistical

import de.tu_dresden.inf.lat.prettyPrinting.datatypes.{BaseConcept, BaseRole, BottomConcept, ComplexRoleSubsumption, ConceptAssertion, ConceptComplement, ConceptConjunction, ConceptDisjunction, ConceptEquivalence, DisjointnessAxiom, DisjunctiveConceptAssertion, DomainAxiom, ExistentialRoleRestriction, Expression, Individual, InverseRole, MaxNumberRestriction, MinNumberRestriction, NominalSet, Ontology, RangeAxiom, RoleAssertion, RoleComplement, RoleConjunction, RoleDisjunction, RoleSubsumption, Subsumption, SymmetricRoleAxiom, TopConcept, TransitiveRoleAxiom, UniversalRoleRestriction}
import de.tu_dresden.inf.lat.prettyPrinting.formatting.Formatter
import de.tu_dresden.inf.lat.prettyPrinting.owlapi.OWLParser

import java.io.{ File, PrintWriter }

        object StatisticalALCFormatter extends StatisticalALCFormatter {
        def main(args: Array[String]): Unit = {
        val inputFile = new File(args(0))
        val outputFile = new File(args(1))

        val parser = new OWLParser()

        val ontology = parser.parseFile(inputFile)

        val formatted = format(ontology)
        new PrintWriter(outputFile) { write(formatted); write("\n"); close }
        }
        }

class StatisticalALCFormatter extends Formatter[Expression] {


        def format(ontology: Ontology): String = {
        var statements = ontology.tbox.axioms.toSeq.map(format).sorted.mkString("\n")

        statements
        }

        override def format(stat: Expression): String = {

        if(stat.subConcepts.keys.exists(c => !c.equals(stat) && format(c).equals("")))
        return ""

        stat match   {
        case TopConcept =>
        format(ConceptDisjunction(Seq(BaseConcept("A"),ConceptComplement(BaseConcept("A")))))
        case BottomConcept =>
        format(ConceptConjunction(Seq(BaseConcept("A"),ConceptComplement(BaseConcept("A")))))
        case BaseConcept(name) => getName(name)
        case ConceptComplement(c) => "NOT(" + format(c)+")"
        case ConceptConjunction(cs) => cs.map(format).mkString("AND(", ", ", ")")
        case ConceptDisjunction(cs) => cs.map(format).mkString("OR(", ", ", ")")
        //  case Individual(name) => name.split('#').last
        //  case NominalSet(ns) => ns.map(format).mkString("{", ", ", "}")


        case BaseRole(name) => name.split('#').last
        // case InverseRole(r) => format(r)+INVERSE
        // case RoleConjunction(cs) => cs.map(format).mkString("(", " "+SQ_AND+" ", ")")
        // case RoleDisjunction(cs) => cs.map(format).mkString("(", " "+SQ_OR+" ", ")")
        // case RoleComplement(c) => NEG + format(c)

        case ExistentialRoleRestriction(r, c) => "SOME("+ format(r) + ", " + format(c)+")"
        case UniversalRoleRestriction(r, c) => "ALL(" + format(r) + ", " + format(c)+")"
//    case MinNumberRestriction(n, r, c) => GEQ + n.toString + format(r) + "." + format(c)
//    case MaxNumberRestriction(n, r, c) => LEQ + n.toString + format(r) + "." + format(c)

        //    case Subsumption(c, d) => format(c) + " \n\t" + SQ_SUBSETEQ + " " + format(d)
        case Subsumption(c, d) =>
        "PROB("+format(d)+", "+format(c)+")[1,1]"
        //    case ConceptEquivalence(cs) => cs.map(format).mkString(" \n\t"+SQ_EQUIV+" ")
        case ConceptEquivalence(cs) =>
        cs.flatMap(c1 => cs.map(c2 =>
        if(c1.equals(c2))
        ""
        else
        format(Subsumption(c1,c2))+"\n"+format(Subsumption(c2,c1))
        )
        ).mkString("\n")
//    case DisjointnessAxiom(cs) => cs.map(format).mkString("disjoint(", ", ", ")")
//    case DomainAxiom(r,c) => "domain("+format(r)+") = "+format(c)
//    case RangeAxiom(r,c) => "range("+format(r)+") = "+format(c)

//    case RoleSubsumption(r, q) => format(r) + " " + SQ_SUBSETEQ + " " + format(q)
//    case ComplexRoleSubsumption(r, q) => r.map(format).mkString(" "+COMP+" ") + " " + SQ_SUBSETEQ + " " + format(q)
//    case TransitiveRoleAxiom(r) => "trans(" + format(r) + ")"
//    case SymmetricRoleAxiom(r) => "symmetric("+format(r) + ")"

//    case ConceptAssertion(c, i) => format(c) + "(" + format(i) + ")"
//    case RoleAssertion(r, i, j) => format(r) + "(" + format(i) + ", " + format(j) + ")"
//    case DisjunctiveConceptAssertion(ds) => ds.map(format).mkString(" "+VEE+" ")
        case _ => println("Not supported: "+stat+"\n Formattting as empty string")
        ""
        }
        }

        def getName(iri: String) = {
        iri.split('#').last.replaceAll("[^a-zA-Z]","")
        }

        }
