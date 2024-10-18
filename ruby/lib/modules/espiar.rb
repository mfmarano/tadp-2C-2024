require_relative '../classes/objeto_espia'

module Espiar
   
  def espiar(objeto)
    ObjetoEspia.new(objeto)
  end
end