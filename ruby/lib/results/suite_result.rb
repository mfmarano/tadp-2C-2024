class SuiteResult

  def initialize(suite_name)
    @suite_name = suite_name
    @test_results = []
  end

  def add_result(test_result)
    @test_results << test_result
  end

  def tests_totales
    @test_results.size
  end

  def tests_pasados
    @test_results.count(&:pasado?)
  end

  def tests_fallidos
    @test_results.count(&:fallido?)
  end

  def tests_explotados
    @test_results.count(&:exploto?)
  end

  def to_s
    separador = "-" * 40
    result = "Resultados para #{@suite_name}:\n"
    result += "#{separador}\n"
    result += "Total de tests: #{tests_totales}\n"
    result += "Tests pasados: #{tests_pasados}\n"
    result += "Tests fallados: #{tests_fallidos}\n"
    result += "Tests explotados: #{tests_explotados}\n"

    @test_results.group_by(&:status).each do |status, tests|
      result += "\n#{separador}\n"
      result += "Tests #{status}:\n"
      result += "#{separador}\n"
      result += tests.map(&:to_s).join("\n")
      result += "\n"
    end

    result + "#{separador}\n"
  end
end