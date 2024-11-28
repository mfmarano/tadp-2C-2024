import scala.util.Try

sealed trait Figura

case class Punto(x: Int, y: Int)
case class Triangulo(v1: Punto, v2: Punto, v3: Punto) extends Figura
case class Rectangulo(vSupIzq: Punto, vInfDer: Punto) extends Figura
case class Circulo(centro: Punto, radio: Int) extends Figura
case class Grupo(figuras: List[Figura]) extends Figura


sealed trait Transformacion extends Figura {
  def esNula: Boolean
  val figuraTransformada: Figura
  def aplicarA(figura: Figura): Figura
}

trait Transformable[T] {
  type Params
  def parametros: Params
  def aplicarA(figura: Figura): Figura
}

case class Color(r: Int, g: Int, b: Int, figura: Figura) extends Figura with Transformable[Color] {
  override def aplicarA(figura: Figura): Figura = 
    Color(r,g,b, figura)
    
  type Params = (Int, Int, Int)
  def parametros: Params = (r, g, b)
}

case class Rotacion(grados: Double, override val figuraTransformada: Figura) extends Transformacion with Transformable[Rotacion] {
  override def esNula: Boolean = grados == 0

  override def aplicarA(figura: Figura): Figura =
    Rotacion(this.grados, figura)

  def combinar(otra: Rotacion): Rotacion =
    Rotacion((this.grados + otra.grados) % 360, otra.figuraTransformada)

  type Params = Double
  def parametros: Params = grados
}

case class Escala(factorX: Double, factorY: Double, override val figuraTransformada: Figura) extends Transformacion with Transformable[Escala] {
  override def esNula: Boolean = factorX == 1 && factorY == 1
  override def aplicarA(figura: Figura): Figura =
    Escala(this.factorX, this.factorY, figura)

  def combinar(otra: Escala): Escala =
    Escala(this.factorX * otra.factorX, this.factorY * otra.factorY, otra.figuraTransformada)

  type Params = (Double, Double)
  def parametros: Params = (factorX, factorY)
}

case class Traslacion(dX: Int, dY: Int, override val figuraTransformada: Figura) extends Transformacion with Transformable[Traslacion] {
  override def esNula: Boolean = dX == 0 && dY == 0

  override def aplicarA(figura: Figura): Figura =
    Traslacion(this.dX, this.dY, figura)

  def combinar(otra: Traslacion): Traslacion =
    Traslacion(this.dX + otra.dX, this.dY + otra.dY, otra.figuraTransformada)

  type Params = (Int, Int)
  def parametros: Params = (dX, dY)
}

object ParserImagenes {

  import Parsers._

  private lazy val figuraSimple: Parser[Figura] = triangulo <|> rectangulo <|> circulo

  private lazy val transformacion: Parser[Figura] = color <|> escala <|> rotacion <|> traslacion

  private lazy val figura: Parser[Figura] = figuraSimple <|> transformacion <|> grupo

  private val espacios: Parser[List[Char]] = (char(' ') <|> char('\r') <|> char('\n') <|> char('\t')).*

  private val rgb: Parser[Int] = integer.satisfies(n => n >= 0 && n <= 255)

  private def normalizarGrados(grados: Double) = ((grados % 360) + 360) % 360

  private def argumento[T](parser: Parser[T]) = for {
    _ <- espacios
    p <- parser
    _ <- espacios
  } yield p

  private def argumentos[T](tipo: Parser[T], principio: Char, fin: Char): Parser[List[T]] = for {
    _ <- char(principio)
    p <- argumento(tipo).sepBy(char(','))
    _ <- char(fin)
  } yield p

  private def argumentos[T](tipo: Parser[T], principio: Char, fin: Char, cantidad: Int): Parser[List[T]] =
    argumentos(tipo, principio, fin).satisfies(_.size == cantidad)

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
    puntos <- argumentos(punto, '[', ']', 3)
  } yield Triangulo(puntos.head, puntos(1), puntos(2))

  val rectangulo: Parser[Rectangulo] = for {
    _ <- string("rectangulo")
    puntos <- argumentos(punto, '[', ']', 2)
  } yield Rectangulo(puntos.head, puntos(1))

  val circulo: Parser[Circulo] = for {
    _ <- string("circulo")
    _ <- char('[')
    centro <- argumento(punto)
    _ <- char(',')
    radio <- argumento(integer)
    _ <- char(']')
  } yield Circulo(centro, radio)

  val grupo: Parser[Grupo] = for {
    _ <- string("grupo")
    figuras <- argumentos(figura, '(', ')')
  } yield Grupo(figuras)

  private val figuraTransformada = argumentos(figura, '(', ')', 1).map(_.head)

  val color: Parser[Color] = for {
    _ <- string("color")
    valores <- argumentos(rgb, '[', ']', 3)
    figura <- figuraTransformada
  } yield Color(valores.head, valores(1), valores(2), figura)

  val escala: Parser[Escala] = for {
    _ <- string("escala")
    factores <- argumentos(double, '[', ']', 2)
    figura <- figuraTransformada
  } yield Escala(factores.head, factores(1), figura)

  val rotacion: Parser[Rotacion] = for {
    _ <- string("rotacion")
    grados <- argumentos(double, '[', ']', 1).map(_.head)
    figura <- figuraTransformada
  } yield Rotacion(normalizarGrados(grados), figura)

  val traslacion: Parser[Traslacion] = for {
    _ <- string("traslacion")
    d <- argumentos(integer, '[', ']', 2)
    figura <- figuraTransformada
  } yield Traslacion(d.head, d(1), figura)
}