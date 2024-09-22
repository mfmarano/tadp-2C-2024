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
end
