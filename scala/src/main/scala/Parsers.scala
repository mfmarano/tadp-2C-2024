import scala.util.{Failure, Success}

object Parsers {
  val anyChar: Parser[Char] = Parser { input =>
    if (input.isEmpty) Failure(new ParserException("Entrada vacía"))
    else Success(input.head, input.tail)
  }

  def char(c: Char): Parser[Char] = anyChar.satisfies(_ == c)

  val digit: Parser[Char] = anyChar.satisfies(_.isDigit)

  def string(str: String): Parser[String] = Parser { input =>
    if (input.startsWith(str)) Success((str, input.drop(str.length)))
    else Failure(new ParserException(s"Esperaba $str"))
  }

  val integer: Parser[Int] = Parser { input =>
    val negativeDigits = char('-').opt <> digit.*
    negativeDigits.parse(input).flatMap((result, rest) => result match {
      case (Some(menos), list) => Success((menos :: list).mkString.toInt, rest)
      case (None, list) => Success(list.mkString.toInt, rest)
    })
  }

  val double: Parser[Double] = Parser { input =>
    val decimalParser = integer <> char('.').opt <> digit.*
    decimalParser.parse(input).flatMap((result, rest) => result match {
      case ((int, Some(_)), list) => Success((int.toString + "." + list.mkString).toDouble, rest)
      case ((int, None), _) => Success(int.toDouble, rest)
    })
  }
}