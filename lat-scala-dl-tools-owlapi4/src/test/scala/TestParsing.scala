
import de.tu_dresden.inf.lat.prettyPrinting.datatypes.{BaseConcept, BaseRole, ConceptEquivalence, DisjointnessAxiom, DomainAxiom, Ontology, TransitiveRoleAxiom}
import de.tu_dresden.inf.lat.prettyPrinting.parsing.{DLParser, OWLParser}
import org.junit.Test
import org.junit.Assert.assertEquals
import org.scalatest.junit.JUnitSuite
import org.semanticweb.owlapi.util.DefaultPrefixManager


class TestParsing extends JUnitSuite {

  val disjointnessString =
    "disjoint(<http://www.co-ode.org/ontologies/pizza/pizza.owl#FruitTopping>, <http://www.co-ode.org/ontologies/pizza/pizza.owl#CheeseTopping>, <http://www.co-ode.org/ontologies/pizza/pizza.owl#SauceTopping>)"
  val disjointnessOntology = Ontology.buildFrom(Set(DisjointnessAxiom(Seq(
    BaseConcept("<http://www.co-ode.org/ontologies/pizza/pizza.owl#FruitTopping>"),
    BaseConcept("<http://www.co-ode.org/ontologies/pizza/pizza.owl#CheeseTopping>"),
    BaseConcept("<http://www.co-ode.org/ontologies/pizza/pizza.owl#SauceTopping>")))))

  val domainString =
    "domain(<http://www.co-ode.org/ontologies/pizza/pizza.owl#hasTopping>) = <http://www.co-ode.org/ontologies/pizza/pizza.owl#Pizza>"
  val domainOntology = Ontology.buildFrom(Set(DomainAxiom(
    BaseRole("<http://www.co-ode.org/ontologies/pizza/pizza.owl#hasTopping>"),
    BaseConcept("<http://www.co-ode.org/ontologies/pizza/pizza.owl#Pizza>"))))

  val transitiveString =
    "trans(<http://www.co-ode.org/ontologies/pizza/pizza.owl#hasTopping>)"
  val transitiveOntology = Ontology.buildFrom(Set(TransitiveRoleAxiom(
    BaseRole("<http://www.co-ode.org/ontologies/pizza/pizza.owl#hasTopping>"))))

  val equivalenceString =
    "<http://www.co-ode.org/ontologies/pizza/pizza.owl#FruitTopping>=<http://www.co-ode.org/ontologies/pizza/pizza.owl#CheeseTopping>"
  val equivalenceOntology = Ontology.buildFrom(Set(ConceptEquivalence(Seq(
    BaseConcept("<http://www.co-ode.org/ontologies/pizza/pizza.owl#FruitTopping>"),
    BaseConcept("<http://www.co-ode.org/ontologies/pizza/pizza.owl#CheeseTopping>")))))

  @Test
  def testParsingStrings() = {

    val parsedDisjointness = DLParser.parse(disjointnessString)
    assertEquals(parsedDisjointness, disjointnessOntology)
    
    val parsedTransitive = DLParser.parse(transitiveString)
    assertEquals(parsedTransitive, transitiveOntology)
    
    val parsedDomain = DLParser.parse(domainString)
    assertEquals(parsedDomain, domainOntology)

    val parsedEquivalence = DLParser.parse(equivalenceString)
    assertEquals(parsedEquivalence, equivalenceOntology)
  }

  @Test
  def testPrefixManager() = {
    val prefixManager = new DefaultPrefixManager()
    prefixManager.setPrefix("test", "http://test.de#")
    prefixManager.setDefaultPrefix("http://nothing.de#")

    println(prefixManager.getIRI("test:C").toString)

    val owlParser = new OWLParser(prefixManager)
    println(owlParser.parse(":test:C <= ::D"))
    println(owlParser.parse("::C(:test:a)"))
    println(owlParser.parse("<r>(<a>, <b>)"))
    //println(owlParser.parse("r(a, b)"))
    println(owlParser.parse("::r(::a, ::b)"))

  }
}
