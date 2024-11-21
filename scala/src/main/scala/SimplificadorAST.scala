object SimplificadorAST {
  def simplificar(figura: Figura): Figura = figura match {
    case c: Color => simplificarColor(c)
    case g: Grupo => simplificarGrupo(g)
    case t: Transformacion => simplificarTransformacion(t)
    case f => f
  }

  private def simplificarGrupo(grupo: Grupo): Figura = {
    val figurasSimplificadas = grupo.figuras.map(simplificar)
    detectarYAplicarTransformacionesComunes(Grupo(figurasSimplificadas))
  }

  private def detectarYAplicarTransformacionesComunes(grupo: Grupo): Figura = {
    def extraerColor(figura: Figura): Option[(Int, Int, Int)] = figura match {
      case Color(r, g, b, _) => Some((r, g, b))
      case _ => None
    }

    val colores = grupo.figuras.map(extraerColor)
    if (colores.forall(_.isDefined) && colores.map(_.get).distinct.size == 1) {
      val (r, g, b) = colores.head.get
      return Color(r, g, b, Grupo(grupo.figuras.map {
        case Color(_, _, _, f) => f
        case f => f
      }))
    }

    grupo
  }

  private def simplificarColor(color: Color): Figura = color match {
    case Color(_, _, _, Color(r, g, b, f)) => simplificar(Color(r, g, b, f))
    case _ => color
  }

  private def simplificarTransformacion(transformacion: Transformacion): Figura = {
    if (transformacion.esNula) return simplificar(getFiguraInterna(transformacion))

    val figuraSimplificada = simplificar(getFiguraInterna(transformacion))
    (transformacion, figuraSimplificada) match {
      case (r1: Rotacion, r2: Rotacion) =>
        r1.combinar(r2)
      case (e1: Escala, e2: Escala) =>
        e1.combinar(e2)
      case (t1: Traslacion, t2: Traslacion) =>
        t1.combinar(t2)
      case _ => transformacion.reconstruir(figuraSimplificada)
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