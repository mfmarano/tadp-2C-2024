class Persona
  attr_accessor :edad

  def initialize(edad)
    @edad = edad
  end

  def adulto?
    self.edad > 17
  end

  def saludar
    "Hola humano"
  end
end
