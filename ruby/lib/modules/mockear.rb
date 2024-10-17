module Mockear
  # WIP
end

# module Mockear
#   def mockear(metodo, &bloque)
#     self.define_singleton_method(metodo, &bloque)
#   end
# end

# module Mockear
#
#   attr_accessor :metodos_originales
#
#   def mockear(method, &block)
#     @metodos_originales ||= {}
#     @metodos_originales[method] = self.instance_method(method)
#     self.define_method(method) do
#       block.call
#     end
#   end
#
#   def desmockear(method)
#     puts @metodos_originales
#     self.define_method(method, @metodos_originales[method])
#   end
# end