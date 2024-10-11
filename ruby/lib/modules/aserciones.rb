require_relative '../classes/condicion'

module Aserciones
  def ser(condicion)
    return igual(condicion) unless condicion.is_a?(Condicion)
    condicion
  end

  def igual(valor)
    mensaje = proc { |objeto| "Esperaba #{valor}, encontré #{objeto}" }
    Condicion.crear_condicion(mensaje) { |objeto| objeto == valor }
  end

  def mayor_a(valor)
    mensaje = proc { |objeto| "Esperaba que #{valor} fuese mayor que #{objeto}" }
    Condicion.crear_condicion(mensaje) { |objeto| objeto > valor }
  end

  def menor_a(valor)
    mensaje = proc { |objeto| "Esperaba que #{valor} fuese menor que #{objeto}" }
    Condicion.crear_condicion(mensaje) { |objeto| objeto < valor }
  end

  def uno_de_estos(*valores)
    if valores.length > 1
      mensaje = proc { |objeto| "Esperaba que #{objeto} fuese uno de #{valores}" }
      Condicion.crear_condicion(mensaje) { |objeto| valores.include?(objeto) }
    else
      mensaje = proc { |objeto| "Esperaba que #{objeto} fuese uno de #{valores}" }
      Condicion.crear_condicion(mensaje) { |objeto| valores.first.include?(objeto) }
    end
  end

  def entender(symbol)
    mensaje = proc { |objeto| "Esperaba que #{objeto} entienda #{symbol}" }
    Condicion.crear_condicion(mensaje) { |objeto| objeto.respond_to? symbol }
  end

  def en(&proc)
    proc
  end

  def explotar_con(excepcion)
    mensaje = proc { "Esperaba que explote con #{excepcion}" }
    Condicion.crear_condicion(mensaje) { |objeto|
      begin
        objeto.call
        false
      rescue excepcion
        true
      end
    }
  end

  def respond_to_missing?(symbol)
    symbol.to_s.start_with?('ser_') || symbol.to_s.start_with?('tener_') || super
  end

  def method_missing(symbol, *args, &block)
    unless respond_to_missing? symbol
      super
    end

    if symbol.to_s.start_with?('ser_')
      return obtener_condicion_ser(symbol)
    end

    if symbol.to_s.start_with?('tener_')
      obtener_condicion_tener(symbol, *args)
    end
  end

  private

  def obtener_condicion_ser(symbol)
    metodo = "#{symbol.to_s.delete_prefix('ser_')}?".to_sym
    mensaje = proc { |objeto| "Esperaba que #{objeto} sea #{symbol}"}
    Condicion.crear_condicion(mensaje) { |objeto| objeto.send(metodo) }
  end

  def obtener_condicion_tener(symbol, *args)
    atributo = "@#{symbol.to_s.delete_prefix('tener_')}".to_sym
    mensaje = proc { |objeto| "Esperaba que #{atributo} de #{objeto} sea #{args[0]}, encontré #{objeto.instance_variable_get(atributo)}" }
    Condicion.crear_condicion(mensaje) { |objeto| ser(args[0]).verificar(objeto.instance_variable_get(atributo)) }
  end
end
