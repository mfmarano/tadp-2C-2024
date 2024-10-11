class Condicion
  def initialize(&proposicion)
    @proposicion = proposicion
  end

  def self.crear_condicion(mensaje, &proposicion)
    condicion = Condicion.new &proposicion
    condicion.instance_variable_set(:@mensaje, mensaje)
    condicion
  end

  def verificar(objeto)
    raise TadspecException.new(@mensaje.call(objeto)) unless @proposicion.call(objeto)
  end
end
