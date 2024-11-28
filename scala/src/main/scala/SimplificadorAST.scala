object SimplificadorAST {
  def simplificar(figura: Figura): Figura = figura match {
    case t: Transformacion => simplificarTransformacion(t)
    case g: Grupo => simplificarGrupo(g)
    case f => f
  }
  

  private def simplificarTransformacion(transformacion: Transformacion): Figura = {
    val figuraTransformadaSimplificada = simplificar(transformacion.figuraTransformada)

    (transformacion, figuraTransformadaSimplificada) match {
      case (t, _) if t.esNula => figuraTransformadaSimplificada
      case (r1: Rotacion, r2: Rotacion) => r1.combinar(r2)
      case (e1: Escala, e2: Escala) => e1.combinar(e2)
      case (t1: Traslacion, t2: Traslacion) => t1.combinar(t2)
      case (c1: Color, c2: Color) => c1.combinar(c2)
      case _ => transformacion.aplicarA(figuraTransformadaSimplificada)
    }
  }

  private def simplificarGrupo(grupo: Grupo): Figura = {
    val figurasSimplificadas = grupo.figuras.map(simplificar)
    detectarYAplicarTransformacionesComunes(Grupo(figurasSimplificadas))
  }

  private def detectarYAplicarTransformacionesComunes(grupo: Grupo): Figura = {
    val transformacionesUniformes = TransformacionesComunes.transformacionesUniformes(grupo.figuras)

    transformacionesUniformes match {
      case List() => grupo
      case _ =>
        val transformacion = grupo.figuras.collect { case t: Transformacion => t }.head
        transformacion.aplicarA(Grupo(grupo.figuras.map(getFiguraInterna)))
    }
  }
  

  private def getFiguraInterna(figura: Figura): Figura = figura match {
    case Color(_, _, _, f) => f
    case Rotacion(_, f) => f
    case Escala(_, _, f) => f
    case Traslacion(_, _, f) => f
    case f => f
  }

}

object TransformacionesComunes {
  def sonTransformacionesUniformes[T <: Transformacion](figuras: List[Figura]): Boolean = {
    val transformaciones = figuras.head match
      case t: Transformacion => t.filter(figuras)
      case _ => List()

    if (transformaciones.size != figuras.size || transformaciones.isEmpty) return false
    
    val primerParametro = transformaciones.head.parametros
    transformaciones.forall(_.parametros == primerParametro)
  }
  
  def transformacionesUniformes(figuras: List[Figura]): List[String] =
    List(
      "Color" -> sonTransformacionesUniformes[Color](figuras),
      "Rotación" -> sonTransformacionesUniformes[Rotacion](figuras),
      "Traslación" -> sonTransformacionesUniformes[Traslacion](figuras),
      "Escala" -> sonTransformacionesUniformes[Escala](figuras)
    ).collect { case (nombre, true) => nombre }
}