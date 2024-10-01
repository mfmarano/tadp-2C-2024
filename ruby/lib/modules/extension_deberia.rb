module ExtensionDeberia
  def deberia(asercion)
    raise TadspecException.new("Test fallido") unless asercion.verificar(self)
  end
end
