require_relative '../classes/condicion'

module Aserciones
  def ser(condicion)
    return igual(condicion) unless condicion.is_a?(Condicion)
    condicion
  end

  def igual(valor)
    Condicion.new { |objeto| objeto == valor }
  end

  def mayor_a(valor)
    Condicion.new { |objeto| objeto > valor }
  end

  def menor_a(valor)
    Condicion.new { |objeto| objeto < valor }
  end

  def uno_de_estos(*valores)
    if valores.length > 1
      Condicion.new { |objeto| valores.include?(objeto) }
    else
      Condicion.new { |objeto| valores.first.include?(objeto) }
    end
  end

  def entender(symbol)
    Condicion.new { |objeto| objeto.respond_to? symbol }
  end

  def en(&proc)
    proc
  end

  def explotar_con(excepcion)
    Condicion.new { |objeto|
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
    Condicion.new { |obj| obj.respond_to?(metodo) && obj.send(metodo) }
  end

  def obtener_condicion_tener(symbol, *args)
    atributo = "@#{symbol.to_s.delete_prefix('tener_')}".to_sym
    Condicion.new { |obj| ser(args[0]).verificar(obj.instance_variable_get(atributo)) }
  end
end
