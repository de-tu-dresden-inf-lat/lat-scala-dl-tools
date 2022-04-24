// import java.io.File

// import de.tu_dresden.inf.lat.prettyPrinting.formatting.{SimpleDLFormatter, SimpleOWLFormatter}

// import scala.collection.JavaConverters._
// import org.semanticweb.HermiT.ReasonerFactory
// import org.semanticweb.owlapi.apibinding.OWLManager
// import org.semanticweb.owlapi.model.OWLClass
// import org.semanticweb.owlapi.reasoner.{Node, OWLReasoner}

// object PrintClassHierarchy {

//   def main(args: Array[String]) = {

//     if(args.size!=1 || !(new File(args(0)).exists)) {
//       println("Usage: ")
//       println(this.getClass.getName+" OWL_FILE_NAME")
//       println()
//       println("Computes and prints the class hierarchy.")
//       System.exit(0)
//     }

//     val manager = OWLManager.createOWLOntologyManager()
//     val ontology = manager.loadOntologyFromOntologyDocument(new File(args(0)))

//     val reasoner = (new ReasonerFactory()).createReasoner(ontology)

//     printHierarchy("", reasoner.getTopClassNode, reasoner)
//   }

//   def printHierarchy(prefix: String, node: Node[OWLClass], reasoner: OWLReasoner): Unit = {
//     println(prefix+node.getEntities.asScala.map(SimpleOWLFormatter.format).mkString(" "+SimpleDLFormatter.SQ_EQUIV+" "))
//     reasoner.getSubClasses(node.getRepresentativeElement, true).forEach(node =>
//       printHierarchy(" "+prefix, node, reasoner)
//     )
//   }
// }
