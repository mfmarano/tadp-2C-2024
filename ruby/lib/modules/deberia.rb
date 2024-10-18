module Deberia
  def deberia(condicion)
    raise TadspecException.new(condicion.mensaje.call(self)) unless condicion.verificar(self)
  end
end
