class Condicion

  attr_reader :mensaje

  def initialize(&proposicion)
    @proposicion = proposicion
  end

  def self.crear_condicion(mensaje, &proposicion)
    condicion = Condicion.new &proposicion
    condicion.instance_variable_set(:@mensaje, mensaje)
    condicion
  end

  def verificar(objeto)
    @proposicion.call(objeto)
  end
end

class CondicionEspia < Condicion

  def self.crear_condicion(mensaje, metodo, &proposicion)
    condicion = CondicionEspia.new &proposicion
    condicion.instance_variable_set(:@mensaje, mensaje)
    condicion.instance_variable_set(:@metodo, metodo)
    condicion
  end

  def verificar(objeto)
    raise TadspecException.new("#{objeto} no fue espiado") unless objeto.is_a? ObjetoEspia
    super
  end

  def veces(cantidad)
    modificar_condicion(
      proc { |objeto| objeto.recibio_tantas_veces?(@metodo, cantidad) },
      proc { |objeto| "Esperaba recibir #{@metodo} #{cantidad} veces y se recibio #{objeto.veces_recibidas(@metodo)} veces" }
    )
    self
  end

  def con_argumentos(*argumentos)
    modificar_condicion(
      proc { |objeto| objeto.recibio_con_argumentos?(@metodo, *argumentos) },
      proc { "Se recibio el mensaje #{@metodo}, pero no con los argumentos #{argumentos}" }
    )
    self
  end

  private

  def modificar_condicion(proposicion_nueva, mensaje_nuevo)
    proposicion_anterior = @proposicion
    @proposicion = proc { |objeto| proposicion_anterior.call(objeto) && proposicion_nueva.call(objeto) }
    mensaje_anterior = @mensaje
    @mensaje = proc { |objeto| proposicion_anterior.call(objeto) ? mensaje_nuevo.call(objeto) : mensaje_anterior.call(objeto) }
  end
end