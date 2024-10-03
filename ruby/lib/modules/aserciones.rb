require_relative '../classes/condicion'

module Aserciones
  def ser(condicion)
    return igual(condicion) unless condicion.is_a?(Condicion)
    condicion
  end

  def igual(valor)
    Condicion.new { |objeto| raise TadspecException.new("Esperaba #{valor}, encontré #{objeto}") unless objeto == valor }
  end

  def mayor_a(valor)
    Condicion.new { |objeto| raise TadspecException.new("Esperaba que #{valor} fuese mayor que #{objeto}") unless objeto > valor }
  end

  def menor_a(valor)
    Condicion.new { |objeto| raise TadspecException.new("Esperaba que #{valor} fuese menor que #{objeto}") unless objeto < valor }
  end

  def uno_de_estos(*valores)
    if valores.length > 1
      Condicion.new { |objeto| raise TadspecException.new("Esperaba que #{objeto} fuese uno de #{valores}") unless valores.include?(objeto) }
    else
      Condicion.new { |objeto| raise TadspecException.new("Esperaba que #{objeto} fuese uno de #{valores}") unless valores.first.include?(objeto) }
    end
  end

  def entender(symbol)
    Condicion.new { |objeto| raise TadspecException.new("Esperaba que #{objeto} entienda #{symbol}") unless objeto.respond_to? symbol }
  end

  def en(&proc)
    proc
  end

  def explotar_con(excepcion)
    Condicion.new { |objeto|
      begin
        objeto.call
        raise TadspecException.new("Esperaba que explote con #{excepcion}")
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
    Condicion.new { |obj| raise TadspecException.new("Esperaba que #{obj} sea #{symbol}") unless obj.send(metodo) }
  end

  def obtener_condicion_tener(symbol, *args)
    atributo = "@#{symbol.to_s.delete_prefix('tener_')}".to_sym
    Condicion.new { |obj| ser(args[0]).verificar(obj.instance_variable_get(atributo)) }
  end
end
