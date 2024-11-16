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

  private val espacios: Parser[List[Char]] =
    (char(' ') <|> char('\r') <|> char('\n')).*

  val punto: Parser[Punto] = for {
    x <- integer
    _ <- espacios
    _ <- char('@')
    _ <- espacios
    y <- integer
  } yield Punto(x, y)

  def argumentos[T](delimiter1: Char, delimiter2: Char, parser: Parser[T]): Parser[List[T]] = for {
    _ <- char(delimiter1)
    _ <- espacios
    args <- parser.sepBy(char(',') <~ espacios)
    _ <- espacios
    _ <- char(delimiter2)
  } yield args

  val triangulo: Parser[Triangulo] = for {
    _ <- string("triangulo")
    puntos <- argumentos('[', ']', punto)
  } yield puntos match {
    case v1 :: v2 :: v3 :: Nil => Triangulo(v1, v2, v3)
    case _ => throw new ParserException("Un triángulo necesita 3 puntos")
  }

  val rectangulo: Parser[Rectangulo] = for {
    _ <- string("rectangulo")
    puntos <- argumentos('[', ']', punto)
  } yield puntos match {
    case vSupIzq :: vInfDer :: Nil => Rectangulo(vSupIzq, vInfDer)
    case _ => throw new ParserException("Un rectángulo necesita 2 puntos")
  }

  val circulo: Parser[Circulo] = for {
    _ <- string("circulo")
    args <- argumentos('[', ']', punto <|> integer.map(Punto(_, 0)))
  } yield args match {
    case centro :: radio :: Nil => Circulo(centro, radio.x)
    case _ => throw new ParserException("Un círculo necesita un centro y un radio")
  }

  private val figuraSimple: Parser[Figura] = triangulo <|> rectangulo <|> circulo

  private val figura: Parser[Figura] = figuraSimple <|> grupo

  val grupo: Parser[Grupo] = for {
    _ <- string("grupo")
    figuras <- argumentos('(', ')', figura)
  } yield Grupo(figuras)


  private val rgb: Parser[Int] = integer.satisfies(n => n >= 0 && n <= 255)

  val color: Parser[Color] = for {
    _ <- string("color")
    valores <- argumentos('[', ']', rgb)
    _ <- espacios
    figs <- argumentos('(', ')', figura).satisfies(figs => figs.size == 1)
  } yield valores match {
    case r :: g :: b :: Nil => Color(r, g, b, figs.head)
    case _ => throw new ParserException("Color necesita tres valores RGB")
  }

  val escala: Parser[Escala] = for {
    _ <- string("escala")
    valores <- argumentos('[', ']', double)
    _ <- espacios
    figs <- argumentos('(', ')', figura).satisfies(figs => figs.size == 1)
  } yield valores match {
    case factorX :: factorY :: Nil => Escala(factorX, factorY, figs.head)
    case _ => throw new ParserException("Escala necesita dos factores")
  }


  private def normalizarGrados(grados: Double): Double = {
    val normalizado = grados % 360
    if (normalizado < 0) normalizado + 360 else normalizado
  }


  val rotacion: Parser[Rotacion] = for {
    _ <- string("rotacion")
    valores <- argumentos('[', ']', double)
    _ <- espacios
    figs <- argumentos('(', ')', figura).satisfies(figs => figs.size == 1)
  } yield valores match {
    case grados :: Nil => Rotacion(normalizarGrados(grados), figs.head)
    case _ => throw new ParserException("Rotación necesita un grados");
  }

  val traslacion: Parser[Traslacion] = for {
    _ <- string("traslacion")
    valores <- argumentos('[', ']', integer)
    _ <- espacios
    figs <- argumentos('(', ')', figura).satisfies(figs => figs.size == 1)
  } yield valores match {
    case dx :: dy :: Nil => Traslacion(dx, dy, figs.head)
    case _ => throw new ParserException("Traslación necesita dos factores");
  }

}