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
    proc_anterior = @proposicion
    @proposicion = proc { |objeto| proc_anterior.call(objeto) && objeto.recibio_tantas_veces?(@metodo, cantidad) }
    mensaje_anterior = @mensaje
    @mensaje = proc { |objeto| proc_anterior.call(objeto) ?
         "Esperaba recibir #{@metodo} #{cantidad} veces y se recibio #{objeto.veces_recibidas(@metodo)} veces"
         : mensaje_anterior.call(objeto)
    }
    self
  end

  def con_argumentos(*argumentos)
    proc_anterior = @proposicion
    @proposicion = proc { |objeto| proc_anterior.call(objeto) && objeto.recibio_con_argumentos?(@metodo, *argumentos) }
    mensaje_anterior = @mensaje
    @mensaje = proc { |objeto| proc_anterior.call(objeto) ?
                 "Se recibio el mensaje #{@metodo}, pero no con los argumentos #{argumentos}"
                 : mensaje_anterior.call(objeto)
    }
    self
  end
end
