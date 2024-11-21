object SimplificadorAST {
  def simplificar(figura: Figura): Figura = {
    val simplificada = figura match {
      case g: Grupo => simplificarGrupo(g)
      case c: Color => simplificarColor(c)
      case r: Rotacion => simplificarTransformacion(r)(
        esNula = _.grados == 0,
        combinar = (r1, r2) => Rotacion((r1.grados + r2.grados) % 360, r2.figura),
        reconstruir = (r, f) => Rotacion(r.grados, f)
      )
      case e: Escala => simplificarTransformacion(e)(
        esNula = e => e.factorX == 1 && e.factorY == 1,
        combinar = (e1, e2) => Escala(e1.factorX * e2.factorX, e1.factorY * e2.factorY, e2.figura),
        reconstruir = (e, f) => Escala(e.factorX, e.factorY, f)
      )
      case t: Traslacion => simplificarTransformacion(t)(
        esNula = t => t.dX == 0 && t.dY == 0,
        combinar = (t1, t2) => Traslacion(t1.dX + t2.dX, t1.dY + t2.dY, t2.figura),
        reconstruir = (t, f) => Traslacion(t.dX, t.dY, f)
      )
      case f => f
    }
    
    if (simplificada == figura) figura
    else simplificar(simplificada)
  }

  private def simplificarGrupo(grupo: Grupo): Figura = {
    val figurasSimplificadas = grupo.figuras.map(simplificar)
    
    // Buscar transformaciones comunes
    detectarYAplicarTransformacionesComunes(Grupo(figurasSimplificadas))
  }

  private def simplificarColor(color: Color): Figura = {
    val figuraSimplificada = simplificar(color.figura)
    figuraSimplificada match {
      case Color(r, g, b, f) => Color(r, g, b, f)
      case _ => Color(color.r, color.g, color.b, figuraSimplificada)
    }
  }

  private def simplificarTransformacion[T <: Figura](transformacion: T)(
    esNula: T => Boolean,
    combinar: (T, T) => T,
    reconstruir: (T, Figura) => T
  ): Figura = {
    if (esNula(transformacion)) return simplificar(getFiguraInterna(transformacion))
    
    val figuraSimplificada = simplificar(getFiguraInterna(transformacion))
    (transformacion, figuraSimplificada) match {
      case (r1: Rotacion, r2: Rotacion) => 
        combinar(r1.asInstanceOf[T], r2.asInstanceOf[T])
      case (e1: Escala, e2: Escala) => 
        combinar(e1.asInstanceOf[T], e2.asInstanceOf[T])
      case (t1: Traslacion, t2: Traslacion) => 
        combinar(t1.asInstanceOf[T], t2.asInstanceOf[T])
      case _ => reconstruir(transformacion, figuraSimplificada)
    }
  }

  private def getFiguraInterna(figura: Figura): Figura = figura match {
    case Color(_, _, _, f) => f
    case Rotacion(_, f) => f
    case Escala(_, _, f) => f
    case Traslacion(_, _, f) => f
    case _ => throw new IllegalArgumentException("La figura no tiene una figura interna")
  }

  private def detectarYAplicarTransformacionesComunes(grupo: Grupo): Figura = {
    // Verificar grupo vacío
    if (grupo.figuras.isEmpty) return grupo

    def extraerColor(figura: Figura): Option[(Int, Int, Int)] = figura match {
      case Color(r, g, b, _) => Some((r, g, b))
      case _ => None
    }

    // Aplicar color comun si existe
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
} 