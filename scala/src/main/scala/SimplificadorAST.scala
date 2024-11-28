object SimplificadorAST {
  def simplificar(figura: Figura): Figura = figura match {
    case t: Transformacion => simplificarTransformacion(t)
    case g: Grupo => simplificarGrupo(g)
    case f => f
  }
  
  private def simplificarTransformacion(transformacion: Transformacion): Figura = {
    val figuraTransformadaSimplificada = simplificar(transformacion.getFiguraInterna)

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
    grupo.transformacionesComunes match {
      case List() => grupo
      case _ =>
        val transformacion = grupo.figuras.collect { case t: Transformacion => t }.head
        transformacion.aplicarA(Grupo(grupo.figuras.map(_.getFiguraInterna)))
    }
  }
}
