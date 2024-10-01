require_relative './aserciones'
require_relative 'extension_deberia'
require_relative '../exceptions/tadspec_exception'
require_relative '../results/total_result'
require_relative '../results/suite_result'
require_relative '../results/test_result'

module TADsPec
  def self.testear(suite_class = nil, *tests)
    if suite_class.nil?
      total_results = testear_todas_las_suites
    elsif tests.empty?
      total_results = testear_todos_los_tests_de_suite(suite_class)
    else
      total_results = testear_tests_especificos(suite_class, tests)
    end
    puts total_results
  end

  private

  def self.testear_todas_las_suites
    total_result = TotalResult.new
    suites.each do |suite|
      suite_result = testear_todos_los_tests_de_suite(suite)
      total_result.add_suite_result(suite_result)
    end
    total_result
  end

  def self.testear_todos_los_tests_de_suite(suite_class)
    suite_instance = suite_class.new
    total_result = TotalResult.new
    test_methods = obtener_metodos_de_test(suite_class)
    suite_result = SuiteResult.new(suite_class.name)
    test_methods.each do |method|
      result = ejecutar_test(suite_instance, method)
      suite_result.add_result(result)
    end
    total_result.add_suite_result(suite_result)

    total_result
  end

  def self.testear_tests_especificos(suite_class, tests)
    suite_instance = suite_class.new
    test_methods = tests.map { |test| "testear_que_#{test}".to_sym }
    total_result = TotalResult.new
    suite_result = SuiteResult.new(suite_class.name)
    test_methods.each do |method|
      if suite_class.instance_methods.include?(method)
        result = ejecutar_test(suite_instance, method)
        suite_result.add_result(result)
      else
        puts "Test #{method} no encontrado en la suite #{suite_class}"
      end
    end
    total_result.add_suite_result(suite_result)
    total_result
  end

  def self.ejecutar_test(suite_instance, method)
    begin
      suite_instance.instance_eval do
        Object.include ExtensionDeberia
        suite_instance.send(method)
      end
      return TestResult.new(method, :pasado)
    rescue TadspecException => e
      return TestResult.new(method, :fallido, e.message)
    rescue StandardError => e
      return TestResult.new(method, :exploto, "#{e.class}: #{e.message}\n#{e.backtrace.join("\n")}")
    end
  end

  def self.suites
    clases = ObjectSpace.each_object(Class).select do |klass|
      !obtener_metodos_de_test(klass).empty?
    end
    clases.each do |klass|
      klass.include(Aserciones)
    end

    clases
  end

  def self.obtener_metodos_de_test(klass)
    klass.instance_methods.select { |method| method.to_s.start_with?('testear_que_') }
  end

end