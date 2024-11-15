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
