import scala.util.{Try, Success, Failure}

case class Parser[+T](parse: String => Try[(T, String)]) {
  def <|>[A >: T](otherParser: => Parser[A]): Parser[A] = Parser { input =>
    for {
      result <- parse(input).recoverWith(_ => otherParser.parse(input))
    } yield result
  }

  def <>[A](otherParser: => Parser[A]): Parser[(T, A)] = Parser { input =>
    for {
      (result1, rest1) <- parse(input)
      (result2, rest2) <- otherParser.parse(rest1)
    } yield ((result1, result2), rest2)
  }

  def ~>[A](otherParser: => Parser[A]): Parser[A] = Parser { input =>
    for {
      (_, rest1) <- parse(input)
      (result2, rest2) <- otherParser.parse(rest1)
    } yield (result2, rest2)
  }

  def <~[A](otherParser: => Parser[A]): Parser[T] = Parser { input =>
    for {
      (result1, rest1) <- parse(input)
      (_, rest2) <- otherParser.parse(rest1)
    } yield (result1, rest2)
  }
}

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