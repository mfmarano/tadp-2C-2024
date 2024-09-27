# frozen_string_literal: true
# El módulo principal del framework
require_relative './aserciones'
require_relative '../classes/object'
require_relative '../exceptions/tadspec_exception'
require_relative '../results/total_result'
require_relative '../results/suite_result'
require_relative '../results/test_result'

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
    total_result = TotalResult.new
    suites.each do |suite|
      suite_result = testear_todos_los_tests_de_suite(suite)
      total_result.add_suite_result(suite_result)
    end
    puts total_result
  end

  def self.testear_todos_los_tests_de_suite(suite_class)
    suite_instance = suite_class.new
    test_methods = obtener_metodos_de_test(suite_class)
    suite_result = SuiteResult.new(suite_class.name)
    test_methods.each do |method|
      result = ejecutar_test(suite_instance, method)
      suite_result.add_result(result)
    end
    suite_result
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
    begin
      suite_instance.instance_eval do
        suite_instance.send(method)
      end
      TestResult.new(method, :pasado)
    rescue TadspecException => e
      TestResult.new(method, :failed, e.message)
    rescue StandardError => e
      TestResult.new(method, :exploded, "#{e.class}: #{e.message}\n#{e.backtrace.join("\n")}")
    end
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

  private

  def self.actualizar_resultados(resultados, resultado)
    resultados[:total] += 1
    case resultado[:estado]
    when :pasado
      resultados[:pasados] << resultado[:nombre]
    when :fallado
      resultados[:fallados] << resultado
    when :explotado
      resultados[:explotados] << resultado
    end
  end

  def self.actualizar_resultados_totales(totales, resultados_suite)
    totales[:total] += resultados_suite[:total]
    totales[:pasados] += resultados_suite[:pasados].size
    totales[:fallados] += resultados_suite[:fallados].size
    totales[:explotados] += resultados_suite[:explotados].size
  end
end