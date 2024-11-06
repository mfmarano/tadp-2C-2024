import scala.util.{Try, Success, Failure}

case class Parser[+T](parse: String => Try[(T, String)])

class ParserException(val message: String) extends RuntimeException

object Parsers {

  val anyChar: Parser[Char] = Parser { input =>
    if (input.isEmpty) Failure(new ParserException("Entrada vacía"))
    else Success((input.head, input.tail))
  }

  def char(c: Char): Parser[Char] = Parser { input =>
    anyChar.parse(input).flatMap { case (result, rest) =>
      if (result == c) Success((c, rest))
      else Failure(new ParserException(s"Esperaba $c pero encontré $result"))
    }
  }

  val digit: Parser[Char] = Parser { input =>
    anyChar.parse(input).flatMap { case (result, rest) =>
      if (result.isDigit) Success((result, rest))
      else Failure(new ParserException(s"$result no es un dígito"))
    }
  }

  def string(str: String): Parser[String] = Parser { input =>
    if (input.startsWith(str)) Success((str, input.drop(str.length)))
    else Failure(new ParserException(s"Esperaba $str"))
  }

  val integer: Parser[Int] = Parser { input =>
    val pattern = """^-?\d+""".r
    pattern.findPrefixOf(input) match {
      case Some(n) => Try((n.toInt, input.drop(n.length)))
      case None => Failure(new ParserException("Esperaba un entero"))
    }
  }

  val double: Parser[Double] = Parser { input =>
    val pattern = """^-?\d+(\.\d+)?""".r
    pattern.findPrefixOf(input) match {
      case Some(n) => Try((n.toDouble, input.drop(n.length)))
      case None => Failure(new ParserException("Esperaba un decimal"))
    }
  }
}