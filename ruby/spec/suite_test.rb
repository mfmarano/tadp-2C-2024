require_relative '../lib/modules/tadspec'
require_relative 'persona'

class MiSuiteDeTests
  def testear_que_se_ejecuta
    puts "Test 1 ejecutado"
  end

  def testear_que_funciona_explotar
    en { 7 / 0 }.deberia explotar_con ZeroDivisionError
  end

  def testear_que_no_funciona_explotar
    en { 7 / 1 }.deberia explotar_con ZeroDivisionError
  end

  def metodo_que_no_es_un_test
    en { 7 / 0 }.deberia explotar_con ZeroDivisionError
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
    en { 7 / 0 }.deberia explotar_con ZeroDivisionError
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

class SuiteMocks
  def testear_que_hay_mock
    Persona.mockear(:saludar) { "Mock" }
    persona = Persona.new(20)
    persona.saludar.deberia ser "Mock"
  end

  def testear_que_no_hay_mock
    persona = Persona.new(20)
    persona.saludar.deberia ser "Hola humano"
  end
end

class SuiteEspias

  def testear_que_espia
    persona = Persona.new(20)
    persona = espiar(persona)

    persona.adulto?
    persona.deberia haber_recibido(:adulto?)
    persona.deberia haber_recibido(:edad)
  end

  def testear_que_falla_si_no_es_espiado
    persona = Persona.new(20)
    persona.deberia haber_recibido(:adulto?)
  end

  def testear_que_cuenta_veces
    persona = Persona.new(20)
    persona = espiar(persona)

    persona.adulto?
    persona.deberia haber_recibido(:adulto?).veces(1)
  end

  def testear_que_verifica_argumentos_vacio
    persona = Persona.new(20)
    persona = espiar(persona)

    persona.adulto?
    persona.deberia haber_recibido(:adulto?).con_argumentos
  end

  def testear_que_verifica_argumentos
    persona = Persona.new(20)
    persona = espiar(persona)

    persona.mayor_de?(20)
    persona.deberia haber_recibido(:mayor_de?).con_argumentos(20)
  end

  def testear_que_falla_por_veces
    persona = Persona.new(20)
    persona = espiar(persona)

    persona.adulto?
    persona.deberia haber_recibido(:adulto?).veces(2)
  end

  def testear_que_falla_por_argumentos
    persona = Persona.new(20)
    persona = espiar(persona)

    persona.mayor_de?(20)
    persona.deberia haber_recibido(:mayor_de?).con_argumentos(10)
  end

end

# TADsPec.testear
# TADsPec.testear(OtraSuiteDeTests)
# TADsPec.testear(OtraSuiteDeTests, :pasa, :falla)
# TADsPec.testear(SuiteMocks)
# TADsPec.testear(SuiteEspias)