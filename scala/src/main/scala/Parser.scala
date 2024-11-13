import scala.util.{Try, Success, Failure}

case class Parser[+T](parse: String => Try[(T, String)]) {
  def <|>[A >: T](otherParser: => Parser[A]): Parser[A] = Parser { input =>
    parse(input).recoverWith(_ => otherParser.parse(input))
  }

  def <>[A](otherParser: => Parser[A]): Parser[(T, A)] = for {
    thisResult <- this
    otherResult <- otherParser
  } yield (thisResult, otherResult)

  def ~>[A](otherParser: => Parser[A]): Parser[A] = for {
    _ <- this
    otherResult <- otherParser
  } yield otherResult

  def <~[A](otherParser: => Parser[A]): Parser[T] = for {
    thisResult <- this
    _ <- otherParser
  } yield thisResult

  def sepBy[A](sep: => Parser[A]): Parser[List[T]] = for {
    thisResult <- this
    restSeparated <- (sep ~> this).*
  } yield thisResult :: restSeparated

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
    +.parse(input).recover { _ => (List.empty[T], input) }
  }

  def + : Parser[List[T]] = for {
    thisResult <- this
    rest <- this.*
  } yield thisResult :: rest

  def map[A](f: T => A): Parser[A] = Parser { input =>
    parse(input).map { (result, rest) => (f(result), rest) }
  }

  def flatMap[A](f: T => Parser[A]): Parser[A] = Parser { input =>
    parse(input).flatMap { (result, rest) => f(result).parse(rest) }
  }
}

class ParserException(val message: String) extends RuntimeException

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