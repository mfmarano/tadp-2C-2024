require_relative 'spec_helper'

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

  end
end