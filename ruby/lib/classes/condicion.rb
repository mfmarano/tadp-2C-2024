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
    true
  end
end

class CondicionEspia < Condicion

  def self.crear_condicion(mensaje, &proposicion)
    condicion = CondicionEspia.new &proposicion
    condicion.instance_variable_set(:@mensaje, mensaje)
    condicion
  end

  def verificar(objeto)
    raise TadspecException.new("#{objeto} no fue espiado") unless objeto.is_a? ObjetoEspia
    super
  end

  def veces(cantidad)
    # WIP
  end

  def con_argumentos(*argumentos)
    # WIP
  end
end
