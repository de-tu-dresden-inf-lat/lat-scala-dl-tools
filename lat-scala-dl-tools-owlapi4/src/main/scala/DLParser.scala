package de.tu_dresden.inf.lat.prettyPrinting.parsing

import java.io.File
import java.net.URL
import scala.io.Source
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.RegexParsers
import de.tu_dresden.inf.lat.prettyPrinting.datatypes._
import de.tu_dresden.inf.lat.prettyPrinting.formatting.SimpleDLFormatter
import de.tu_dresden.inf.lat.prettyPrinting.owlapi.OWLExporter
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.expression.ShortFormEntityChecker
import org.semanticweb.owlapi.model.{OWLAxiom, PrefixManager}
import org.semanticweb.owlapi.util.{BidirectionalShortFormProviderAdapter, DefaultPrefixManager}

object DL2OWL {

  def main(args: Array[String]) = {

    val parser = new OWLParser();

    println(parser.parse("<A> <= E<r>.<C>"))

    val ontology = DLParser.parse(new File(args(0)))

    println(SimpleDLFormatter.format(ontology))

    //val exporter = new OWLExporter()

    // exporter.exportOntology(ontology, new File(args(0)+".owl"))
  }

}

class OWLParser(optPrefixManager: Option[PrefixManager]) {

  def this() = this(None)

  def this(prefixManager: PrefixManager) = this(Some(prefixManager))

  val exporter = new OWLExporter(simplifiedNames = false);

  val ontology = OWLManager.createOWLOntologyManager().createOntology()

  def parse(string: String): OWLAxiom = {
    optPrefixManager.foreach(DLParser.setPrefixManager(_))
    exporter.toOwl(ontology, DLParser.parseDLStatement(string))
  }
}

object DLParser extends RegexParsers {
  
  def rep2sep[T](p : => Parser[T], q : => Parser[Any]): Parser[List[T]] =
    p ~ q ~ rep1sep(p,q) ^^ {case x ~ _ ~ y => x :: y}

  var optPrefixManager: Option[PrefixManager] = None

  def setPrefixManager(prefixManager: PrefixManager): Unit ={
    optPrefixManager = Some(prefixManager)
  }

  def parse(url: URL): Ontology = {
    val result = new Ontology()

    assert(url != null)

    Source.fromURL(url).getLines().foreach(line =>
      if (!line.matches("""(\s*|//.*)"""))
        result.addStatement(parseDLStatement(line)))

    result
  }

  def parse(file: File): Ontology = {
    val result = new Ontology()

    Source.fromFile(file, "iso-8859-1").getLines().foreach(line =>
      if (!line.matches("""(\s*|//.*|#.*|\*\*.*)"""))
        result.addStatement(parseDLStatement(line)))

    result
  }

  def parse(string: String): Ontology = {
    val result = new Ontology()

    string.split("\n").foreach { line => result.addStatement(parseDLStatement(line)) }

    result
  }

  def parseConcept(input: String): Concept = parseAll(concept, input) match {
    // def parseConcept(string: String): Concept = phrase(concept)(new lexical.Scanner(string)) match {
    case Success(c, _) => c
    case Failure(m, remaining) => throw new RuntimeException(m + "\n" + remaining.pos.longString)
    case Error(m, remaining) => throw new RuntimeException(m + "\n" + remaining.pos.longString)
  }

  def parseDLStatement(input: String): DLStatement = parseAll(dlStatement, input) match {
    case Success(s, _) => s
    case Failure(m, remaining) => throw new RuntimeException(m + "\n" + remaining.pos.longString)
    case Error(m, remaining) => throw new RuntimeException(m + "\n" + remaining.pos.longString)
  }

  val CONCEPT_NAME = """\w+'*(?!\.)""" r
  val IRI_STRING = """<[^>]+>""" r
  val PREFIX_STRING = """:[a-z0-9A-Z_]*:\w+"""r
  val ROLE_NAME = """\w+'*""" r
  val IND_NAME = """[a-z0-9_]+'*""".r

  val INT = """\d+""" r

  def dlStatement: Parser[DLStatement] =
    disjointness | subsumption | roleSubsumption | functionalRoleAxiom | transitiveRoleAxiom |
      complexRoleSubsumption | roleAssertion | conceptAssertion |
      domainAxiom | rangeAxiom | equivalence | disjunctiveConceptAssertion

  def subsumption: Parser[DLStatement] =
    concept ~ "<=" ~ concept ^^ { case a ~ _ ~ b => Subsumption(a, b) }

  def equivalence: Parser[DLStatement] =
    rep1sep(concept, "=") ^^ { case cs: List[Concept] => ConceptEquivalence(cs) }

  def disjointness: Parser[DLStatement] =
    "disjoint(" ~ rep1sep(concept, ",") ~ ")" ^^ { case _ ~ (cs: List[Concept]) ~ _ => DisjointnessAxiom(cs)}

  def roleSubsumption: Parser[DLStatement] =
    role ~ "<r" ~ role ^^ { case r1 ~ _ ~ r2 => RoleSubsumption(r1, r2) }

  def complexRoleSubsumption: Parser[DLStatement] =
    rep2sep(role, "o") ~ "<r" ~ role ^^ { case (sub: List[Role]) ~ _ ~ sup => ComplexRoleSubsumption(sub, sup) }

  def conceptAssertion: Parser[DLStatement] =
    concept ~ "(" ~ individual ~ ")" ^^ { case c ~ _ ~ ind ~ _ => ConceptAssertion(c, Individual(ind)) }

  def roleAssertion: Parser[DLStatement] =
    role ~ "(" ~ individual ~ ", " ~ individual ~ ")" ^^ {
      case r ~ _ ~ a ~ _ ~ b ~ _ => RoleAssertion(r, Individual(a), Individual(b))
    }

  def individual: Parser[String] =
    IRI_STRING | PREFIX_STRING ^^ { s => parseName(s.substring(1))} //| IND_NAME

  //def individualName: Parser[String] =
  //  IND_NAME ^^ { s => new Individual(s) }


  //def individualIRI: Parser[Individual] =
  //  IRI_STRING ^^ { s => new Individual(s)}

  // in its current form, the following causes parsing problems
  def disjunctiveConceptAssertion: Parser[DLStatement] =
    rep1sep(conceptAssertion, "v") ^^ {
      case List(onlyOne) => onlyOne
      case (cas: List[DLStatement]) =>
        DisjunctiveConceptAssertion(cas.toSet[DLStatement].map(_.asInstanceOf[ConceptAssertion]))
    }

  def functionalRoleAxiom: Parser[DLStatement] =
    "func(" ~ role ~ ")" ^^ {
      case _ ~ r ~ _ => FunctionalRoleAxiom(r)
    }

  def transitiveRoleAxiom: Parser[DLStatement] =
    "trans(" ~ role ~ ")" ^^ {
      case _ ~ r ~ _ => TransitiveRoleAxiom(r)
    }

  def domainAxiom: Parser[DLStatement] =
    "domain(" ~ role ~ ") = " ~ concept ^^ {
      case _ ~ r ~ _ ~ c => DomainAxiom(r,c)
    }

  def rangeAxiom: Parser[DLStatement] =
    "range(" ~ role ~ ") = " ~ concept ^^ {
      case _ ~ r ~ _ ~ c => RangeAxiom(r,c)
    }


  def concept: Parser[Concept] =
    disjunction | conjunction | conceptComplement | existentialRestriction | universalRestriction | nominalSetIRI |
      minNumberRestriction | maxNumberRestriction | eqNumberRestriction | constant | // baseConcept |
      baseConceptIRI


  def constant: Parser[Concept] =
    ("TOP" | "BOTTOM") ^^ { case "TOP" => TopConcept
    case "BOTTOM" => BottomConcept
    }


  def baseConcept: Parser[Concept] =
    CONCEPT_NAME ^^ { s => BaseConcept(s) }

  def baseConceptIRI: Parser[Concept] =
    PREFIX_STRING ^^ {s => BaseConcept(parseName(s.substring(1)))} | IRI_STRING ^^ { s => BaseConcept(s) }

  def parseName(string: String) = {
    optPrefixManager match {
      case None => string
      case Some(prefixManager: PrefixManager) => prefixManager.getIRI(string).toQuotedString
    }
  }

  def conceptComplement: Parser[Concept] =
    """(-|¬)""".r ~> concept ^^ { case c => ConceptComplement(c) }

  def disjunction: Parser[Concept] =
    "(" ~> repsep(concept, "u") <~ ")" ^^ { (ds: List[Concept]) => ConceptDisjunction(ds) }

  def conjunction: Parser[Concept] =
    "(" ~> repsep(concept, "n") <~ ")" ^^ { (cs: List[Concept]) => ConceptConjunction(cs) }


  def existentialRestriction: Parser[Concept] =
    "E" ~> role ~ "." ~ concept ^^ { case r ~ _ ~ c => ExistentialRoleRestriction(r, c) }

  def universalRestriction: Parser[Concept] =
    "A" ~> role ~ "." ~ concept ^^ { case r ~ _ ~ c => UniversalRoleRestriction(r, c) }

  def nominalSetIRI: Parser[Concept] =
   "{" ~> repsep(IRI_STRING, ",") <~ "}" ^^ { (is: List[String]) => NominalSet(is.map(s => Individual(s))) }

  def eqNumberRestriction: Parser[Concept] =
    "=" ~> INT ~ role ~ "." ~ concept ^^ { case n ~ r ~ _ ~ c => EqNumberRestriction(n.toInt, r, c) }

  def minNumberRestriction: Parser[Concept] =
    ">=" ~> INT ~ role ~ "." ~ concept ^^ { case n ~ r ~ _ ~ c => MinNumberRestriction(n.toInt, r, c) }

  def maxNumberRestriction: Parser[Concept] =
    "=<" ~> INT ~ role ~ "." ~ concept ^^ { case n ~ r ~ _ ~ c => MaxNumberRestriction(n.toInt, r, c) }


  def role: Parser[Role] =
    topRole | roleInverse2 | //| baseRole
  baseRoleIRI | roleInverse | roleConjunction | roleDisjunction | roleComplement

  def topRole: Parser[Role] =
    "TOP" ^^ { case "TOP" => TopRole }

  def baseRole: Parser[Role] =
    ROLE_NAME ^^ { case s => BaseRole(s) }

  def baseRoleIRI: Parser[Role] =
    PREFIX_STRING ^^ {case s=> BaseRole(parseName(s.substring(1)))} | IRI_STRING ^^ { case s => BaseRole(s) }

  def roleInverse: Parser[Role] =
    "(" ~> role <~ ")^-1" ^^ { case r => InverseRole(r) }

  def roleInverse2: Parser[Role] =
    "inv(" ~> role <~ ")" ^^ { case r => InverseRole(r) }

  def roleConjunction: Parser[Role] =
    "(" ~> repsep(role, "n") <~ ")" ^^ { case rs: List[Role] => RoleConjunction(rs) }

  def roleDisjunction: Parser[Role] =
    "(" ~> repsep(role, "u") <~ ")" ^^ { case rs: List[Role] => RoleDisjunction(rs) }

  def roleComplement: Parser[Role] =
    "¬" ~ role ^^ { case _ ~ r => RoleComplement(r) }

}
