Classes for printing OWL objects in DL syntax.

Provides also for a main class for directly pretty printing an OWL file to the
standard output. To use this functionality, compile with:

mvn clean assembly:single

Then it can be used with

java -cp target/scala-2.12/dl-pretty-printer-standalone.jar de.tu_dresden.lat.prettyPrinting.tools.formatting.SimpleDLFormatter OWL-FILE


To run tests via Maven use:
mvn clean install -DskipTests=false

To compile and publish via Maven use:
mvn clean install

To compile and publish an uber-Jar via Maven use:
mvn clean assembly:single
