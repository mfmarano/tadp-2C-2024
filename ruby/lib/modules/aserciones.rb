require_relative '../classes/condicion'

module Aserciones
  def ser(condicion)
    return igual(condicion) unless condicion.is_a?(Condicion)
    condicion
  end

  def igual(valor)
    Condicion.new(Proc.new { |objeto| objeto == valor })
  end

  def mayor_a(valor)
    Condicion.new(Proc.new { |objeto| objeto > valor })
  end

  def menor_a(valor)
    Condicion.new(Proc.new { |objeto| objeto < valor })
  end

  def uno_de_estos(*valores)
    if valores.length > 1
      Condicion.new(Proc.new { |objeto| valores.include?(objeto) })
    else
      Condicion.new(Proc.new { |objeto| valores.first.include?(objeto) })
    end
  end

  def respond_to_missing?(symbol)
    symbol.to_s.start_with?('ser_') || super
  end

  def method_missing(symbol, *args, &block)
    unless respond_to_missing? symbol
      super
    end

    if symbol.to_s.start_with?('ser_')
      obtener_condicion_ser(symbol, *args, block)
    end
  end

  private

  def obtener_condicion_ser(symbol, *args, block)
    metodo = "#{symbol.to_s.delete_prefix("ser_")}?".to_sym
    Condicion.new(Proc.new {
      |obj| obj.respond_to?(metodo) && obj.send(metodo, *args, &block)
    })
  end
end
