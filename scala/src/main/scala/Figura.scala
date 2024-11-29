case class Punto(x: Int, y: Int)

sealed trait Figura {
  val figuraInterna: Figura = this match {
    case Color(_, _, _, f) => f
    case Rotacion(_, f) => f
    case Escala(_, _, f) => f
    case Traslacion(_, _, f) => f
    case _ => this
  }
}

case class Triangulo(v1: Punto, v2: Punto, v3: Punto) extends Figura
case class Rectangulo(vSupIzq: Punto, vInfDer: Punto) extends Figura
case class Circulo(centro: Punto, radio: Int) extends Figura
case class Grupo(figuras: List[Figura]) extends Figura

sealed trait Transformacion extends Figura {
  def esNula: Boolean

  def aplicarA(figura: Figura): Figura

  def esIgualA(figuras: List[Figura]): Boolean = figuras.forall(esIgualA)

  def esIgualA(otra: Figura): Boolean
}

case class Color(red: Int, green: Int, blue: Int, figura: Figura) extends Transformacion {
  override def esNula: Boolean = false

  override def aplicarA(figura: Figura): Figura = Color(red, green, blue, figura)

  override def esIgualA(otra: Figura): Boolean = otra match {
    case Color(r, g, b, _) => red == r && green == g && blue == b
    case _ => false
  }

  def combinarCon(otra: Color): Color = otra
}

case class Rotacion(grados: Double, figura: Figura) extends Transformacion {
  override def esNula: Boolean = grados == 0

  override def aplicarA(figura: Figura): Figura = Rotacion(grados, figura)

  override def esIgualA(otra: Figura): Boolean = otra match {
    case Rotacion(g, _) => grados == g
    case _ => false
  }

  def combinarCon(otra: Rotacion): Rotacion =
    Rotacion((grados + otra.grados) % 360, otra.figura)
}

case class Escala(factorX: Double, factorY: Double, figura: Figura) extends Transformacion {
  override def esNula: Boolean = factorX == 1 && factorY == 1

  override def aplicarA(figura: Figura): Figura = Escala(factorX, factorY, figura)

  override def esIgualA(otra: Figura): Boolean = otra match {
    case Escala(fX, fY, _) => factorX == fX && factorY == fY
    case _ => false
  }

  def combinarCon(otra: Escala): Escala =
    Escala(factorX * otra.factorX, factorY * otra.factorY, otra.figura)
}

case class Traslacion(desplazamientoX: Int, desplazamientoY: Int, figura: Figura) extends Transformacion {
  override def esNula: Boolean = desplazamientoX == 0 && desplazamientoY == 0

  override def aplicarA(figura: Figura): Figura = Traslacion(desplazamientoX, desplazamientoY, figura)

  override def esIgualA(otra: Figura): Boolean = otra match {
    case Traslacion(dX, dY, _) => desplazamientoX == dX && desplazamientoY == dY
    case _ => false
  }

  def combinarCon(otra: Traslacion): Traslacion =
    Traslacion(desplazamientoX + otra.desplazamientoX, desplazamientoY + otra.desplazamientoY, otra.figura)
}
