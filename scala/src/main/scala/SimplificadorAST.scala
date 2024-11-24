object SimplificadorAST {
  def simplificar(figura: Figura): Figura = figura match {
    case c: Color => simplificarColor(c)
    case t: Transformacion => simplificarTransformacion(t)
    case g: Grupo => simplificarGrupo(g)
    case f => f
  }

  private def simplificarColor(color: Color): Figura = color match {
    case Color(_, _, _, Color(r, g, b, f)) => Color(r, g, b, simplificar(f))
    case _ => color
  }

  private def simplificarTransformacion(transformacion: Transformacion): Figura = {
    val figuraTransformadaSimplificada = simplificar(transformacion.figuraTransformada)

    (transformacion, figuraTransformadaSimplificada) match {
      case (t, _) if t.esNula => figuraTransformadaSimplificada
      case (r1: Rotacion, r2: Rotacion) => r1.combinar(r2)
      case (e1: Escala, e2: Escala) => e1.combinar(e2)
      case (t1: Traslacion, t2: Traslacion) => t1.combinar(t2)
      case _ => transformacion.aplicarA(figuraTransformadaSimplificada)
    }
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

    def extraerGrados(figura: Figura): Option[Double] = figura match {
      case Rotacion(grados, _) => Some(grados)
      case _ => None
    }

    def extraerDesplazamiento(figura: Figura): Option[(Int, Int)] = figura match {
      case Traslacion(dx, dy, _) => Some((dx, dy))
      case _ => None
    }

    def extraerEscala(figura: Figura): Option[(Double, Double)] = figura match {
      case Escala(factorX, factorY, _) => Some((factorX, factorY))
      case _ => None
    }

    val rotaciones = grupo.figuras.map(extraerGrados)
    if (rotaciones.forall(_.isDefined) && rotaciones.map(_.get).distinct.size == 1) {
      val grados = rotaciones.head.get
      return Rotacion(grados, Grupo(grupo.figuras.map(x => getFiguraInterna(x))))
    }

    val desplazamientos = grupo.figuras.map(extraerDesplazamiento)
    if (desplazamientos.forall(_.isDefined) && desplazamientos.map(_.get).distinct.size == 1) {
      val (dx, dy) = desplazamientos.head.get
      return Traslacion(dx, dy, Grupo(grupo.figuras.map(x => getFiguraInterna(x))))
    }

    val escalas = grupo.figuras.map(extraerEscala)
    if (escalas.forall(_.isDefined) && escalas.map(_.get).distinct.size == 1) {
      val (factorX, factorY) = escalas.head.get
      return Escala(factorX, factorY, Grupo(grupo.figuras.map(x => getFiguraInterna(x))))
    }

    val colores = grupo.figuras.map(extraerColor)
    if (colores.forall(_.isDefined) && colores.map(_.get).distinct.size == 1) {
      val (r, g, b) = colores.head.get
      return Color(r, g, b, Grupo(grupo.figuras.map(x => getFiguraInterna(x))))
    }

    grupo
  }

  private def getFiguraInterna(figura: Figura): Figura = figura match {
    case Color(_, _, _, f) => f
    case Rotacion(_, f) => f
    case Escala(_, _, f) => f
    case Traslacion(_, _, f) => f
    case f => f
  }
}

// TODO: Revisar `simplificarGrupo`