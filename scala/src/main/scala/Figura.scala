import scala.util.Try

sealed trait Figura

case class Punto(x: Int, y: Int)
case class Triangulo(v1: Punto, v2: Punto, v3: Punto) extends Figura
case class Rectangulo(vSupIzq: Punto, vInfDer: Punto) extends Figura
case class Circulo(centro: Punto, radio: Int) extends Figura
case class Grupo(figuras: List[Figura]) extends Figura
case class Color(r: Int, g: Int, b: Int, figura: Figura) extends Figura
case class Escala(factorX: Double, factorY: Double, figura: Figura) extends Figura
case class Rotacion(grados: Double, figura: Figura) extends Figura
case class Traslacion(dX: Int, dY: Int, figura: Figura) extends Figura

object ParserImagenes {

  import Parsers._

  private lazy val figuraSimple: Parser[Figura] = triangulo <|> rectangulo <|> circulo

  private lazy val transformacion: Parser[Figura] = color <|> escala <|> rotacion <|> traslacion

  private lazy val figura: Parser[Figura] = figuraSimple <|> transformacion <|> grupo

  private val espacios: Parser[List[Char]] = (char(' ') <|> char('\r') <|> char('\n') <|> char('\t')).*

  private val rgb: Parser[Int] = integer.satisfies(n => n >= 0 && n <= 255)

  private def normalizarGrados(grados: Double) = ((grados % 360) + 360) % 360

  private def delimitadoPor[T](parser: Parser[T], principio: Char, fin: Char): Parser[T] = for {
    _ <- char(principio)
    _ <- espacios
    t <- parser
    _ <- espacios
    _ <- char(fin)
  } yield t

  private def argumentos[T](parser: Parser[T], principio: Char, fin: Char): Parser[List[T]] =
    delimitadoPor(parser.sepBy(char(',') <~ espacios), principio, fin)

  def parsearFigura(entrada: String): Try[Figura] = figura.parse(entrada).map(_._1)

  val punto: Parser[Punto] = for {
    x <- integer
    _ <- espacios
    _ <- char('@')
    _ <- espacios
    y <- integer
  } yield Punto(x, y)

  val triangulo: Parser[Triangulo] = for {
    _ <- string("triangulo")
    puntos <- argumentos(punto, '[', ']')
  } yield puntos match {
    case v1 :: v2 :: v3 :: Nil => Triangulo(v1, v2, v3)
    case _ => throw new ParserException("Un triángulo necesita 3 puntos")
  }

  val rectangulo: Parser[Rectangulo] = for {
    _ <- string("rectangulo")
    puntos <- argumentos(punto, '[', ']')
  } yield puntos match {
    case vSupIzq :: vInfDer :: Nil => Rectangulo(vSupIzq, vInfDer)
    case _ => throw new ParserException("Un rectángulo necesita 2 puntos")
  }

  val circulo: Parser[Circulo] = for {
    _ <- string("circulo")
    args <- argumentos(punto <|> integer.map(Punto(_, 0)), '[', ']')
  } yield args match {
    case centro :: radio :: Nil => Circulo(centro, radio.x)
    case _ => throw new ParserException("Un círculo necesita un centro y un radio")
  }

  val grupo: Parser[Grupo] = for {
    _ <- string("grupo")
    figuras <- argumentos(figura, '(', ')')
  } yield Grupo(figuras)

  val color: Parser[Color] = for {
    _ <- string("color")
    valores <- argumentos(rgb, '[', ']')
    figura <- delimitadoPor(figura, '(', ')')
  } yield valores match {
    case r :: g :: b :: Nil => Color(r, g, b, figura)
    case _ => throw new ParserException("Color necesita 3 valores RGB")
  }

  val escala: Parser[Escala] = for {
    _ <- string("escala")
    factores <- argumentos(double, '[', ']')
    figura <- delimitadoPor(figura, '(', ')')
  } yield factores match {
    case factorX :: factorY :: Nil => Escala(factorX, factorY, figura)
    case _ => throw new ParserException("Escala necesita 2 factores")
  }

  val rotacion: Parser[Rotacion] = for {
    _ <- string("rotacion")
    grados <- argumentos(double, '[', ']')
    figura <- delimitadoPor(figura, '(', ')')
  } yield grados match {
    case grados :: Nil => Rotacion(normalizarGrados(grados), figura)
    case _ => throw new ParserException("Rotación necesita grados");
  }

  val traslacion: Parser[Traslacion] = for {
    _ <- string("traslacion")
    desplazamientos <- argumentos(integer, '[', ']')
    figura <- delimitadoPor(figura, '(', ')')
  } yield desplazamientos match {
    case dx :: dy :: Nil => Traslacion(dx, dy, figura)
    case _ => throw new ParserException("Traslación necesita 2 factores de desplazamiento");
  }
}