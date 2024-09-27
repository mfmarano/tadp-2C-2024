class TotalResult

  def initialize
    @suite_results = []
  end

  def add_suite_result(suite_result)
    @suite_results << suite_result
  end

  def tests_totales
    @suite_results.sum(&:tests_totales)
  end

  def tests_pasados
    @suite_results.sum(&:tests_pasados)
  end

  def tests_fallidos
    @suite_results.sum(&:tests_fallidos)
  end

  def tests_explotados
    @suite_results.sum(&:tests_explotados)
  end

  def to_s
    separador = "-" * 40
    result = "#{separador}\n"

    @suite_results.each do |suite_result|
      result += "\n#{suite_result}\n"
    end

    result += "Resultados totales:\n"
    result += "#{separador}\n"
    result += "Total de tests: #{tests_totales}\n"
    result += "Tests pasados: #{tests_pasados}\n"
    result += "Tests fallados: #{tests_fallidos}\n"
    result += "Tests explotados: #{tests_explotados}\n"
    result += "#{separador}\n"

    result
  end
end