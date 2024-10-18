class ObjetoEspia
  attr_reader :objeto_espiado, :llamadas

  def initialize(objeto_espiado)
    @objeto_espiado = objeto_espiado
    @llamadas = []
    espiar_metodos
  end

  def method_missing(method, *args, &block)
    @objeto_espiado.send(method, *args, &block)
  end

  def respond_to_missing?(method, include_private = false)
    @objeto_espiado.respond_to?(method, include_private) || super
  end

  private

  def espiar_metodos
    @objeto_espiado.class.instance_methods(false)
                   .map { |instance_method| @objeto_espiado.method(instance_method) }
                   .each { |method| espiar_metodo(method, self) }
  end

  def espiar_metodo(method, espia)
    @objeto_espiado.define_singleton_method(method.name) do |*argumentos, &bloque|
      espia.llamadas << { metodo: method.name, argumentos: argumentos }
      method.call(*argumentos, &bloque)
    end
  end

end
