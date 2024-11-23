import tadp.drawing.TADPDrawingAdapter

object Interprete {
  private type Adapter = TADPDrawingAdapter

  def dibujar(figura: Figura): Adapter => Adapter = adapter => {
    SimplificadorAST.simplificar(figura) match {
      case Triangulo(v1, v2, v3) =>
        adapter.triangle((v1.x, v1.y), (v2.x, v2.y), (v3.x, v3.y))
      case Rectangulo(vSupIzq, vInfDer) =>
        adapter.rectangle((vSupIzq.x, vSupIzq.y), (vInfDer.x, vInfDer.y))
      case Circulo(centro, radio) =>
        adapter.circle((centro.x, centro.y), radio)
      case Grupo(figurasAgrupadas) =>
        figurasAgrupadas.foldLeft(adapter)((adapter, figuraAgrupada) => dibujar(figuraAgrupada)(adapter))
      case Color(r, g, b, figuraColoreada) =>
        transformar(figuraColoreada, _.beginColor(scalafx.scene.paint.Color.rgb(r, g, b)))(adapter)
      case Escala(factorX, factorY, figuraEscalada) =>
        transformar(figuraEscalada, _.beginScale(factorX, factorY))(adapter)
      case Rotacion(grados, figuraRotada) =>
        transformar(figuraRotada, _.beginRotate(grados))(adapter)
      case Traslacion(dX, dY, figuraTrasladada) =>
        transformar(figuraTrasladada, _.beginTranslate(dX, dY))(adapter)
    }
  }

  private def transformar(figura: Figura, transformacion: Adapter => Adapter): Adapter => Adapter =
    adapter => dibujar(figura)(transformacion(adapter)).end()
}