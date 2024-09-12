require_relative 'spec_helper'

class Prueba
  def materia
    :tadp
  end
end

describe 'Prueba' do
  let(:prueba) { Prueba.new }

  describe '#materia' do
    it 'debería pasar este test' do
      expect(prueba.materia).to be :tadp
    end
  end
end