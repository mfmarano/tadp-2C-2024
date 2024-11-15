sealed trait Figura

case class Punto(x: Int, y: Int)
case class Triangulo(v1: Punto, v2: Punto, v3: Punto) extends Figura
case class Rectangulo(vSupIzq: Punto, vInfDer: Punto) extends Figura
case class Circulo(centro: Punto, radio: Int) extends Figura
case class Grupo(figuras: List[Figura]) extends Figura
case class Color(r: Int, g: Int, b: Int, figura: Figura) extends Figura
case class Escala(factorX: Double, factorY: Double, figura: Figura) extends Figura
case class Rotacion(grados: Double, figura: Figura) extends Figura
case class Traslacion(desplazamientoX: Int, desplazamientoY: Int, figura: Figura) extends Figura

object ParserImagenes {
  import Parsers._

  val espacios: Parser[List[Char]] = char(' ').*

  val punto: Parser[Punto] = for {
    x <- integer
    _ <- espacios
    _ <- char('@')
    _ <- espacios
    y <- integer
  } yield Punto(x, y)

  def argumentos[T](parser: Parser[T]): Parser[List[T]] = for {
    _ <- char('[')
    args <- parser.sepBy(char(',') <~ espacios)
    _ <- char(']')
  } yield args

  val triangulo: Parser[Triangulo] = for {
    _ <- string("triangulo")
    puntos <- argumentos(punto)
  } yield puntos match {
    case v1 :: v2 :: v3 :: Nil => Triangulo(v1, v2, v3)
    case _ => throw new ParserException("Un triángulo necesita 3 puntos")
  }

  val rectangulo: Parser[Rectangulo] = for {
    _ <- string("rectangulo")
    puntos <- argumentos(punto)
  } yield puntos match {
    case vSupIzq :: vInfDer :: Nil => Rectangulo(vSupIzq, vInfDer)
    case _ => throw new ParserException("Un rectángulo necesita 2 puntos")
  }

  val circulo: Parser[Circulo] = for {
    _ <- string("circulo")
    args <- argumentos(punto <|> integer.map(Punto(_, 0)))
  } yield args match {
    case centro :: radio :: Nil => Circulo(centro, radio.x)
    case _ => throw new ParserException("Un círculo necesita un centro y un radio")
  }
}