require_relative 'spec_helper'

Object.include Deberia
describe 'TADsPec' do

  describe 'Aserciones' do

    self.class_eval do
      include Aserciones
    end

    it 'true es igual a true' do
      true.deberia ser true
    end

    it '7 es igual a 7' do
      7.deberia ser 7
    end

    it '30 es mayor a 20' do
      30.deberia ser mayor_a 20
    end

    it '20 es menor que 25' do
      20.deberia ser menor_a 25
    end

    it '7 esta dentro de la lista' do
      7.deberia ser uno_de_estos [7, 22, "hola"]
    end

    it 'hola esta dentro de la lista' do
      "hola".deberia ser uno_de_estos 7, 22, "hola"
    end

    it 'Persona es adulto' do
      Persona.new(30).deberia ser_adulto
      Persona.new(30).adulto?.deberia ser true
    end

    it 'Persona tiene 30' do
      Persona.new(30).deberia tener_edad 30
      Persona.new(30).deberia tener_nombre nil
      Persona.new(30).deberia tener_edad mayor_a 20
      Persona.new(30).deberia tener_edad menor_a 40
      Persona.new(30).deberia tener_edad uno_de_estos [7, 30, "hola"]
    end

    it 'Persona entiende' do
      Persona.new(30).deberia entender :adulto?
      Persona.new(30).deberia entender :class
    end

    it 'Explota' do
      en { 7 / 0 }.deberia explotar_con ZeroDivisionError
      en { Persona.new(30).nombre }.deberia explotar_con NoMethodError
      en { Persona.new(30).nombre }.deberia explotar_con StandardError
    end

  end

  describe 'Espiar' do

    self.class_eval do
      include Aserciones
      include Espiar
    end

    it 'el objeto original no se modifica' do
      objeto_original = [1, 2, 3]
      objeto_espia = espiar(objeto_original)

      objeto_espia.push(4)

      objeto_original.deberia ser [1, 2, 3]
      objeto_espia.objeto_espiado.deberia ser [1, 2, 3, 4]
    end

    it 'el objeto espiado funciona como debería' do
      objeto_original = [1, 2, 3]
      objeto_espia = espiar(objeto_original)

      objeto_espia.push(4)
      objeto_espia.pop

      objeto_espia.deberia haber_recibido(:push)
      objeto_espia.deberia haber_recibido(:push).con_argumentos(4)
      objeto_espia.deberia haber_recibido(:pop)
      objeto_espia.deberia haber_recibido(:push).veces(1)
      objeto_espia.deberia haber_recibido(:pop).veces(1)
    end
  end
end