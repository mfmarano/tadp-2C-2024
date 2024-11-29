object SimplificadorAST {
  def simplificar(figura: Figura): Figura = figura match {
    case t: Transformacion => simplificarTransformacion(t)
    case g: Grupo => simplificarGrupo(g)
    case f => f
  }

  private def simplificarTransformacion(transformacion: Transformacion): Figura = {
    val figuraTransformadaSimplificada = simplificar(transformacion.figuraInterna)

    (transformacion, figuraTransformadaSimplificada) match {
      case (t, _) if t.esNula => figuraTransformadaSimplificada
      case (r1: Rotacion, r2: Rotacion) => r1.combinarCon(r2)
      case (e1: Escala, e2: Escala) => e1.combinarCon(e2)
      case (t1: Traslacion, t2: Traslacion) => t1.combinarCon(t2)
      case (c1: Color, c2: Color) => c1.combinarCon(c2)
      case _ => transformacion.aplicarA(figuraTransformadaSimplificada)
    }
  }

  private def simplificarGrupo(grupo: Grupo): Figura = {
    val figurasSimplificadas = grupo.figuras.map(simplificar)

    figurasSimplificadas match {
      case (t: Transformacion) :: figuras if t.esIgualA(figuras) =>
        t.aplicarA(Grupo(figurasSimplificadas.map(_.figuraInterna)))
      case _ => Grupo(figurasSimplificadas)
    }
  }
}
