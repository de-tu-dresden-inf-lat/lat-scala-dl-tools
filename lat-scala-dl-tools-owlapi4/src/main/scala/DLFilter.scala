package de.tu_dresden.inf.lat.dltools

import com.typesafe.scalalogging.Logger
import de.tu_dresden.inf.lat.prettyPrinting.formatting.SimpleOWLFormatter
import org.semanticweb.owlapi.model.parameters.Imports
import org.semanticweb.owlapi.model.{AxiomType, OWLAxiom, OWLClass, OWLClassExpression, OWLDisjointClassesAxiom, OWLObjectAllValuesFrom, OWLObjectComplementOf, OWLObjectIntersectionOf, OWLObjectOneOf, OWLObjectProperty, OWLObjectPropertyDomainAxiom, OWLObjectPropertyRangeAxiom, OWLObjectSomeValuesFrom, OWLObjectUnionOf, OWLOntology, OWLOntologyManager, OWLSubClassOfAxiom, OWLSubClassOfAxiomSetShortCut, OWLSubClassOfAxiomShortCut, OWLSubPropertyAxiom}

import scala.collection.JavaConverters.{seqAsJavaListConverter, setAsJavaSetConverter}


abstract class DLFilter {

  private val logger = Logger[DLFilter]

  def supported(axiom: OWLAxiom): Boolean
  def supported(expression: OWLClassExpression): Boolean

  def removeUnsupportedAxioms(ontology: OWLOntology): Unit = {
    var toRemove = List[OWLAxiom]()
    ontology.getAxioms(Imports.INCLUDED).forEach(axiom => {
      if(!supported(axiom) || axiom.getNestedClassExpressions.stream().anyMatch(!supported(_)))
        toRemove = axiom::toRemove
    })
    logger.debug(s"Unsupported axioms: ${toRemove.map(SimpleOWLFormatter.format)}")
    ontology.getOWLOntologyManager.removeAxioms(ontology, toRemove.toSet.asJava)
  }

  def filteredCopy(ontology: OWLOntology, manager: OWLOntologyManager)
  : OWLOntology = {
    val copy = manager.createOntology()
    ontology.getAxioms(Imports.INCLUDED).forEach(axiom => {
      if(supported(axiom) && axiom.getNestedClassExpressions.stream().allMatch(supported(_)))
        manager.addAxiom(copy, axiom)
    })
    copy
  }
}

object ALCHTBoxFilter extends DLFilter {

  val logger = Logger(ALCHTBoxFilter.getClass)

  override def supported(axiom: OWLAxiom) = {
    val result = axiom.isOfType(AxiomType.TBoxAndRBoxAxiomTypes) && (axiom match {
      case sp: OWLSubPropertyAxiom[_] =>
        sp.getSubProperty.isInstanceOf[OWLObjectProperty] && sp.getSuperProperty.isInstanceOf[OWLObjectProperty]
      case _ =>
        axiom.isInstanceOf[OWLSubClassOfAxiom] ||
          axiom.isInstanceOf[OWLSubClassOfAxiomShortCut] || axiom.isInstanceOf[OWLSubClassOfAxiomSetShortCut] ||
          axiom.isInstanceOf[OWLDisjointClassesAxiom] ||
        axiom.isInstanceOf[OWLObjectPropertyDomainAxiom] || axiom.isInstanceOf[OWLObjectPropertyRangeAxiom]
    })
    if(!result)
      logger.debug(s"axiom type not supported: ${axiom}")
    result
  }

  override def supported(expression: OWLClassExpression) = {
    val result = expression.getObjectPropertiesInSignature.stream().allMatch(_.isInstanceOf[OWLObjectProperty]) &&
      (expression.isInstanceOf[OWLClass] || expression.isInstanceOf[OWLObjectUnionOf] ||
      expression.isInstanceOf[OWLObjectIntersectionOf] || expression.isInstanceOf[OWLObjectSomeValuesFrom] ||
      expression.isInstanceOf[OWLObjectAllValuesFrom] || expression.isInstanceOf[OWLObjectComplementOf])

    if(!result)
      logger.debug(s"class expression not supported: ${expression}")
    result
  }
}


object ALCTBoxFilter extends DLFilter {

  val logger = Logger(ALCHTBoxFilter.getClass)

  override def supported(axiom: OWLAxiom) = {
    val result = axiom.isOfType(AxiomType.TBoxAndRBoxAxiomTypes) && (axiom match {
      case _ =>
        axiom.isInstanceOf[OWLSubClassOfAxiom] ||
          axiom.isInstanceOf[OWLSubClassOfAxiomShortCut] || axiom.isInstanceOf[OWLSubClassOfAxiomSetShortCut] ||
          axiom.isInstanceOf[OWLDisjointClassesAxiom] ||
          axiom.isInstanceOf[OWLObjectPropertyDomainAxiom] || axiom.isInstanceOf[OWLObjectPropertyRangeAxiom]
    })
    if(!result)
      logger.debug(s"axiom type not supported: ${axiom}")
    result
  }

  override def supported(expression: OWLClassExpression) = {
    val result = expression.getObjectPropertiesInSignature.stream().allMatch(_.isInstanceOf[OWLObjectProperty]) &&
      (expression.isInstanceOf[OWLClass] || expression.isInstanceOf[OWLObjectUnionOf] ||
        expression.isInstanceOf[OWLObjectIntersectionOf] || expression.isInstanceOf[OWLObjectSomeValuesFrom] ||
        expression.isInstanceOf[OWLObjectAllValuesFrom] || expression.isInstanceOf[OWLObjectComplementOf])

    if(!result)
      logger.debug(s"class expression not supported: ${expression}")
    result
  }
}

object ALCOITBoxFilter extends DLFilter {
  override def supported(axiom: OWLAxiom) = {
    axiom.isOfType(AxiomType.TBoxAndRBoxAxiomTypes) && (axiom match {
      case _ =>
        axiom.isInstanceOf[OWLSubClassOfAxiom] ||
          axiom.isInstanceOf[OWLSubClassOfAxiomShortCut] || axiom.isInstanceOf[OWLSubClassOfAxiomSetShortCut] ||
          axiom.isInstanceOf[OWLObjectPropertyDomainAxiom] || axiom.isInstanceOf[OWLObjectPropertyRangeAxiom]
    })

  }

  override def supported(expression: OWLClassExpression) = {
    expression.isInstanceOf[OWLClass] || expression.isInstanceOf[OWLObjectUnionOf] ||
      expression.isInstanceOf[OWLObjectIntersectionOf] || expression.isInstanceOf[OWLObjectSomeValuesFrom] ||
      expression.isInstanceOf[OWLObjectAllValuesFrom] || expression.isInstanceOf[OWLObjectOneOf] ||
      expression.isInstanceOf[OWLObjectComplementOf]
  }
}