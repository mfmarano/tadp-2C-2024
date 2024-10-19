require_relative '../classes/objeto_espia'

module Espiar
  def espiar(objeto)
    objeto_espia = ObjetoEspia.new(objeto)
    objeto_espia.espiar_metodos
    objeto_espia
  end
end