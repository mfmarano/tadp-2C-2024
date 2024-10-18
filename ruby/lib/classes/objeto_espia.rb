class ObjetoEspia
  attr_accessor :objeto_espiado, :llamadas

  def initialize(objeto_espiado)
    @objeto_espiado = objeto_espiado
    @llamadas = []
    espiar_metodos
  end

  def espiar_metodos
    espia = self

    @objeto_espiado.class.instance_methods(false).map { |method| @objeto_espiado.method(method) }.each { |method|
      @objeto_espiado.define_singleton_method(method.name) do |*args, &bloque|
        espia.agregar_call(method.name, *args)
        method.call(*args, &bloque)
      end
    }
  end

  def agregar_call(simbolo, *args)
    @llamadas << { method: simbolo, args: args }
  end

  def method_missing(method, *args, &block)
    @objeto_espiado.send(method, *args, &block)
  end

  def respond_to_missing?(method, include_private = false)
    @objeto_espiado.respond_to?(method, include_private) || super
  end
end
