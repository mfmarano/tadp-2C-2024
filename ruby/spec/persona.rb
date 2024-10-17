class Persona
  def initialize(edad)
    @edad = edad
  end

  def adulto?
    @edad > 17
  end

  def saludar
    "Hola humano"
  end
end
