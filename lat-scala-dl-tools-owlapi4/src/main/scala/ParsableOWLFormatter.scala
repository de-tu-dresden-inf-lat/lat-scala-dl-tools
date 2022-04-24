package de.tu_dresden.inf.lat.prettyPrinting.formatting

import de.tu_dresden.inf.lat.prettyPrinting.owlapi.OWLApiConverter
import org.semanticweb.owlapi.model.OWLAxiom

class ParsableOWLFormatter extends Formatter[OWLAxiom] {

  val converter = new OWLApiConverter(simplifiedNames = false)

  override def format(axiom: OWLAxiom): String = {
    converter.convert(axiom).map(_.toString).mkString("\n");
  }
}
