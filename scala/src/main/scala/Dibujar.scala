import scalafx.scene.paint.Color
import tadp.drawing.TADPDrawingAdapter

import scala.util.{Failure, Success}

object GUIDeTextoADibujo extends App {
  TADPDrawingAdapter.forInteractiveScreen { (texto, adapter) =>
    ParserImagenes.parsearFigura(texto) match {
      case Success(figura) => Interprete.dibujar(figura)(adapter)
      case Failure(error) => println(s"Error al parsear el texto: ${error.getMessage}")
    }
  }
}

object GenerarImagen extends App {
  TADPDrawingAdapter.forImage("imagen.png")(dibujarFiguraEjemplo)
}

object Dibujar extends App {
  TADPDrawingAdapter.forScreen(dibujarFiguraEjemplo)
}

def dibujarFiguraEjemplo(adapter: TADPDrawingAdapter): TADPDrawingAdapter = {
  adapter
    .circle((300, 100), 50) // un ojo
    .circle((500, 100), 50) // otro ojo
    .beginColor(Color.rgb(255, 0, 0))
    .triangle((400, 200), (350, 250), (450, 250)) // nariz
    .end()
    .rectangle((300, 350), (500, 370)) //
    .rectangle((300, 310), (320, 350)) // La boca
    .rectangle((480, 310), (500, 350))
}
