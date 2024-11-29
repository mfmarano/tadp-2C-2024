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