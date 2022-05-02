Various classes to make work with OWL ontologies more convenient. Contains a formatter to format OWL ontologies using DL syntax, which can also be used from the command line to make it convenient to browse large OWL files. Provides new datastructures for representing OWL axioms and concepts to allow for a more convenient access from Scala using pattern matching (uses case classes). Provides a parser for a simplified DL syntax.

To build a fat jar of the library, compile as follows:

mvn clean compile assembly:single

Then the formatter can be used from the commad line as follows:

java -cp target/scala-2.12/lat-scala-dl-tools-standalone.jar de.tu_dresden.lat.prettyPrinting.tools.formatting.SimpleDLFormatter OWL-FILE


To run tests via Maven use:
mvn clean install -DskipTests=false

To compile and publish via Maven use:
mvn clean install

