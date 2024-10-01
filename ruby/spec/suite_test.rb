# frozen_string_literal: true
require_relative '../lib/modules/Suites'

# class MiSuiteDeTests
#   def testear_que_se_ejecuta
#     puts "Test 1 ejecutado"
#   end
#
#   def testear_que_funciona_explotar
#     en { 7/ 0}.deberia explotar_con ZeroDivisionError
#   end
#
#   def metodo_que_no_es_un_test
#     en { 7/ 0}.deberia explotar_con ZeroDivisionError
#   end
# end

class OtraSuiteDeTests
  def testear_que_pasa
    1.deberia ser 1
  end

  def testear_que_falla
    2.deberia ser 1
  end

  def testear_que_funciona_explotar
    en { 7/ 0}.deberia explotar_con ZeroDivisionError
  end

  def testear_que_test_explota
    1.metodo_que_no_existe
  end
end

TADsPec.testear