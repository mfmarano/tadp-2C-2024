require_relative '../lib/modules/Suites'
require_relative 'persona'

class MiSuiteDeTests
  def testear_que_se_ejecuta
    puts "Test 1 ejecutado"
  end

  def testear_que_funciona_explotar
    en { 7/ 0}.deberia explotar_con ZeroDivisionError
  end

  def testear_que_no_funciona_explotar
    en { 7/ 1}.deberia explotar_con ZeroDivisionError
  end

  def metodo_que_no_es_un_test
    en { 7/ 0}.deberia explotar_con ZeroDivisionError
  end
end

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

  def testear_que_persona_es_adulto
    Persona.new(2).deberia ser_adulto
  end

  def testear_que_persona_tiene_30
    Persona.new(30).deberia tener_edad 30
  end

  def no_funciona_deberia
    2.deberia ser 1
  end

end

#TADsPec.testear
#TADsPec.testear(OtraSuiteDeTests)
#TADsPec.testear(OtraSuiteDeTests, :testear_que_falla)