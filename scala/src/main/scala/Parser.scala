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

  def sepBy[A](sep: => Parser[A]): Parser[List[T]] = (this <~ sep.opt).+


  def satisfies(condition: T => Boolean): Parser[T] = Parser { input =>
    parse(input).flatMap { (result, rest) =>
      if (condition(result)) Success(result, rest)
      else Failure(new ParserException(s"El resultado $result no satisface el predicado"))
    }
  }

  def opt: Parser[Option[T]] = Parser { input =>
    parse(input) match {
      case Success(result, rest) => Success(Some(result), rest)
      case Failure(_) => Success(None, input)
    }
  }

  def * : Parser[List[T]] = Parser { input =>
    parse(input) match {
      case Success(_) => +.parse(input)
      case Failure(_) => Success(List.empty, input)
    }
  }

  def + : Parser[List[T]] = Parser { input =>
    parse(input).flatMap { (result, rest) =>
      +.parse(rest) match {
        case Success(moreResults, finalRest) => Success(result :: moreResults, finalRest)
        case Failure(_) => Success(List(result), rest)
      }
    }
  }

  def map[A](f: T => A): Parser[A] = Parser { input =>
    parse(input).map { (result, rest) => (f(result), rest) }
  }
}

class ParserException(val message: String) extends RuntimeException

object Parsers {

  val anyChar: Parser[Char] = Parser { input =>
    if (input.isEmpty) Failure(new ParserException("Entrada vacía"))
    else Success(input.head, input.tail)
  }

  def char(c: Char): Parser[Char] = Parser { input =>
    anyChar.parse(input).flatMap { (result, rest) =>
      if (result == c) Success(c, rest)
      else Failure(new ParserException(s"Esperaba $c pero encontré $result"))
    }
  }

  val digit: Parser[Char] = Parser { input =>
    anyChar.parse(input).flatMap { (result, rest) =>
      if (result.isDigit) Success(result, rest)
      else Failure(new ParserException(s"$result no es un dígito"))
    }
  }

  def string(str: String): Parser[String] = Parser { input =>
    str.foldLeft[Try[(String, String)]](Success("", input)) { (acc, c) =>
      acc.flatMap { (result, rest) =>
        char(c).parse(rest).map { (parsedChar, rest) =>
          (result :+ parsedChar, rest)
        }
      }
    }
  }

  val integer: Parser[Int] = Parser { input =>
    val negativeDigits = char('-').opt <> digit.*
    negativeDigits.parse(input).flatMap((result, rest) => result match {
      case (Some(menos), list) => Success((menos::list).mkString.toInt, rest)
      case (None, list) => Success(list.mkString.toInt, rest)
    })
  }

  val double: Parser[Double] = Parser { input =>
    val decimalParser = integer <> char('.').opt <> digit.*
    decimalParser.parse(input).flatMap( (result, rest) => result match {
      case ((int, Some(_)), list) => Success((int.toString + "." + list.mkString).toDouble, rest)
      case ((int, None), _) => Success(int.toDouble, rest)
    })
  }
}