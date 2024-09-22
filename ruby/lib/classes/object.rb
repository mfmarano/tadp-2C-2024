# TODO: investigar para que `deberia` se entienda solo dentro de los tests
class Object
  def deberia(asercion)
    raise TadspecException.new("Test fallido") unless asercion.verificar(self)
  end
end