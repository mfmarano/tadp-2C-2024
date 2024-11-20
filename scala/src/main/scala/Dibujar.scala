import scalafx.scene.paint.Color
import tadp.drawing.TADPDrawingAdapter

import scala.util.{Failure, Success}

object Dibujar extends App {
  TADPDrawingAdapter.forScreen { adapter =>
    // Aca es donde iria su codigo que USA el adapter
    
    // Ejemplo de como dibujar usando directamente la API de dibujo
    adapter
      .circle((300, 100), 50) // un ojo
      .circle((500, 100), 50) // otro ojo
      .beginColor(Color.rgb(255, 0, 0))
      .triangle((400, 200), (350, 250), (450, 250)) // nariz
      .end()
      .rectangle((300, 350), (500, 370)) //
      .rectangle((300, 310), (320, 350)) // La boca
      .rectangle((480, 310), (500, 350)) //
  }
}

object GUIDeTextoADibujo extends App {
  TADPDrawingAdapter.forInteractiveScreen { (texto, adapter) =>
    ParserImagenes.parsearFigura(texto) match {
      case Success(figura) => Interprete.dibujar(figura)(adapter)
      case Failure(error) => println(s"Error al parsear el texto")
    }
  }
}

object GenerarImagen extends App {
  // La imagen se genera en la carpeta out/
  TADPDrawingAdapter.forImage("imagen.png") { adapter =>
    adapter
      .circle((300, 100), 50) // un ojo
      .circle((500, 100), 50) // otro ojo
      .beginColor(Color.rgb(255, 0, 0))
      .triangle((400, 200), (350, 250), (450, 250)) // nariz
      .end()
      .rectangle((300, 350), (500, 370)) //
      .rectangle((300, 310), (320, 350)) // La boca
      .rectangle((480, 310), (500, 350)) //
  }
}
