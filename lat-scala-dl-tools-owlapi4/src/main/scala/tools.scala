package de.tu_dresden.inf.lat.prettyPrinting.tools

import java.io.{ File, PrintWriter, Writer, FileOutputStream }

import scala.io.Source
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.RegexParsers

/**
 * Simple parsing for more complex data structures
 */
object ParsingTools  { 

  def parseObj2SetMap[T](parseFunction: (String => T))(string: String): Map[T, Set[T]] = { 
    val ep = new ExtendedParser(parseFunction)
    ep.parseDirect(ep.obj2SetMap, string) 
  }

  def parseSet2ObjMap[T](parseFunction: (String => T))(string: String): Map[Set[T], T] = { 
    val ep = new ExtendedParser(parseFunction)
    ep.parseDirect(ep.set2ObjMap, string)
  }

  private class ExtendedParser[T](parseFunction: (String => T)) extends RegexParsers { 

    def parseDirect[T](parser: Parser[T], string: String) = parseAll(parser, string) match { 
      case Success(result, _) => result
      case Failure(m, remaining) => throw new RuntimeException(m+"\n"+remaining.pos.longString)
      case Error(m, remaining) => throw new RuntimeException(m+"\n"+remaining.pos.longString)      
    }

    override def skipWhitespace = false

    def set2ObjMap: Parser[Map[Set[T], T]] = 
      "Map(" ~> repsep(set2ObjPair, ", ") <~ ")" ^^ { case pairs => pairs.toMap}

    def set2ObjPair: Parser[(Set[T], T)] = 
      objSet ~ " -> " ~ obj ^^ { case key ~ _ ~ value => key -> value }

    def obj2SetMap: Parser[Map[T, Set[T]]] = 
      "Map(" ~> repsep(obj2SetPair, ", ") <~ ")" ^^ { case pairs => pairs.toMap }
    
    def obj2SetPair: Parser[(T, Set[T])] = 
      obj ~ " -> " ~ objSet ^^ { case key ~ _ ~ set => key -> set }

    def objSet: Parser[Set[T]] =
      "Set(" ~> repsep(obj, ", ") <~ ")" ^^ { case list => list.toSet }

    def obj = 
      string ^^ { case string => parseFunction(string) }

    val string = """[^\s,\)]+"""r 
  }
}
