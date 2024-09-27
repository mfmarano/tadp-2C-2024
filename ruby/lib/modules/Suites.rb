# frozen_string_literal: true
# El módulo principal del framework
require_relative './aserciones'
require_relative '../classes/object'
require_relative '../exceptions/tadspec_exception'



module TADsPec
  def self.testear(suite_class = nil, *tests)
    if suite_class.nil?
      testear_todas_las_suites
    elsif tests.empty?
      testear_todos_los_tests_de_suite(suite_class)
    else
      testear_tests_especificos(suite_class, tests)
    end
  end

  def self.testear_todas_las_suites
    suites.each do |suite|
      testear_todos_los_tests_de_suite(suite)
    end
  end

  def self.testear_todos_los_tests_de_suite(suite_class)
    suite_instance = suite_class.new
    test_methods = obtener_metodos_de_test(suite_class)
    test_methods.each do |method|
      ejecutar_test(suite_instance, method)
    end
  end

  def self.testear_tests_especificos(suite_class, tests)
    suite_instance = suite_class.new
    test_methods = tests.map { |test| "testear_que_#{test}".to_sym }
    test_methods.each do |method|
      if suite_class.instance_methods.include?(method)
        ejecutar_test(suite_instance, method)
      else
        puts "Test #{method} no encontrado en la suite #{suite_class}"
      end
    end
  end

  def self.ejecutar_test(suite_instance, method)
    #puts "Ejecutando #{method}..."
    suite_instance.instance_eval do
      suite_instance.send(method)
    end
    puts "#{method} PASS"
    rescue => e
      puts "Error al ejecutar el test #{method}: #{e.message}"
  end

  def self.suites
    clases = ObjectSpace.each_object(Class).select do |klass|
      !obtener_metodos_de_test(klass).empty?
    end
    clases.each do |klass|
      klass.include(Aserciones)
      #klass.class_eval do
      #  using Method_deberia
      #end
    end

    clases
  end

  def self.obtener_metodos_de_test(klass)
    klass.instance_methods.select { |method| method.to_s.start_with?('testear_que_') }
  end
end


# Ejecuciones
TADsPec.testear          # Corre todas las suites
#TADsPec.testear(MiSuiteDeTests)  # Corre todos los tests de MiSuiteDeTests
#TADsPec.testear(MiSuiteDeTests, :pasa_algo)  # Corre el test testear_que_pasa_algo