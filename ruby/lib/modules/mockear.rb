require_relative '../classes/metodo_original'

module Mockear
  def mockear(metodo, &comportamiento)
    Mockeador.guardar_metodo_original(self, metodo, &comportamiento)
  end
end

module Mockeador

  attr_accessor :metodos_originales

  def self.guardar_metodo_original(clase, simbolo, &comportamiento)
    metodos_originales ||= []
    metodos_originales << MetodoOriginal.new(clase, simbolo, clase.instance_method(simbolo))

    clase.define_method simbolo, comportamiento
  end

  def self.restaurar_metodos_originales
    metodos_originales.each do |metodo|
      metodo.clase.define_method(metodo.simbolo, metodo.comportamiento)
    end
  end

  def self.metodos_originales
    @metodos_originales ||= []
  end

end