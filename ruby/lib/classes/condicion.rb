class Condicion
  def initialize(&proposicion)
    @proposicion = proposicion
  end

  def verificar(objeto)
    @proposicion.call(objeto)
  end
end
