require_relative './aserciones'
require_relative 'extension_deberia'
require_relative '../exceptions/tadspec_exception'
require_relative '../results/total_result'
require_relative '../results/suite_result'
require_relative '../results/test_result'

module TADsPec
  def self.testear(suite_class = nil, *tests)
    total_results = TotalResult.new
    if suite_class.nil?
      total_results = testear_todas_las_suites
    elsif tests.empty?
      total_results.add_suite_result(testear_todos_los_tests_de_suite(suite_class))
    else
      total_results.add_suite_result(testear_tests_especificos(suite_class, tests))
    end
    puts total_results
  end

  private

  def self.testear_todas_las_suites
    obtener_todas_las_suites.each_with_object(TotalResult.new) { |suite, total_result|
      total_result.add_suite_result(testear_todos_los_tests_de_suite(suite))
    }
  end

  def self.testear_todos_los_tests_de_suite(suite_class)
    testear_tests_especificos(suite_class, obtener_metodos_de_test(suite_class))
  end

  def self.testear_tests_especificos(suite_class, tests)
    suite_result = SuiteResult.new(suite_class.name)
    tests.map { |test| test.start_with?('testear_que_') ? test.to_sym : "testear_que_#{test}".to_sym }
         .each_with_object(suite_class.new) { |method, suite_instance| suite_result.add_result(ejecutar_test(suite_instance, method)) }
    suite_result
  end

  def self.ejecutar_test(suite_instance, method)
    unless suite_instance.methods.include?(method)
      return TestResult.new(method, :exploto, "Test #{method} no encontrado en la suite #{suite_instance.class}")
    end

    suite_instance.class.include(Aserciones)
    begin
      suite_instance.instance_eval {
        Object.include ExtensionDeberia
        suite_instance.send(method)
      }
      return TestResult.new(method, :pasado)
    rescue TadspecException => e
      return TestResult.new(method, :fallido, e.message)
    rescue StandardError => e
      return TestResult.new(method, :exploto, "#{e.class}: #{e.message}\n#{e.backtrace.join("\n")}")
    end
  end

  def self.obtener_todas_las_suites
    ObjectSpace.each_object(Class)
               .select { |klass| !obtener_metodos_de_test(klass).empty? }
  end

  def self.obtener_metodos_de_test(klass)
    klass.instance_methods
         .select { |method| method.to_s.start_with?('testear_que_') }
  end

end