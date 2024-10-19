module Mockear
  def mockear(metodo, &comportamiento)
    Mockeador.guardar_metodo_original(self.instance_method(metodo), &comportamiento)
  end
end

module Mockeador

  attr_accessor :metodos_originales

  def self.metodos_originales
    @metodos_originales ||= []
  end

  def self.guardar_metodo_original(metodo, &comportamiento)
    metodos_originales << metodo
    metodo.owner.define_method metodo.name, comportamiento
  end

  def self.restaurar_metodos_originales
    metodos_originales.each do |metodo|
      metodo.owner.define_method(metodo.name, metodo)
    end
    metodos_originales.clear
  end

end