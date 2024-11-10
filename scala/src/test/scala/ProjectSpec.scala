import Parsers.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.matchers.should.Matchers.*
import scala.util.{Failure, Success}

class ProjectSpec extends AnyFreeSpec {

  "Parsers" - {
    "anyChar" - {
      "debería parsear cualquier caracter" in {
        anyChar.parse("hola") shouldEqual Success(('h', "ola"))
        anyChar.parse("") shouldBe a[Failure[_]]
      }
    }

    "char" - {
      "debería parsear un caracter específico" in {
        char('c').parse("chau") shouldEqual Success(('c', "hau"))
        char('c').parse("hola") shouldBe a[Failure[_]]
      }
    }

    "digit" - {
      "debería parsear un dígito" in {
        digit.parse("1abc") shouldEqual Success(('1', "abc"))
        digit.parse("abc") shouldBe a[Failure[_]]
      }
    }

    "string" - {
      "debería parsear una cadena específica" in {
        string("hola").parse("hola mundo!") shouldEqual Success(("hola", " mundo!"))
        string("").parse("") shouldBe Success(("", ""))
        string("").parse("hola mundo!") shouldBe Success(("", "hola mundo!"))
        string("hola").parse("") shouldBe a[Failure[_]]
        string("hola").parse("holgado") shouldBe a[Failure[_]]
      }
    }

    "integer" - {
      "debería parsear un entero" in {
        integer.parse("123abc") shouldEqual Success((123, "abc"))
        integer.parse("-123abc") shouldEqual Success((-123, "abc"))
        integer.parse("abc") shouldBe a[Failure[_]]
      }
    }

    "double" - {
      "debería parsear un decimal" in {
        double.parse("123.45abc") shouldEqual Success((123.45, "abc"))
        double.parse("-123.45abc") shouldEqual Success((-123.45, "abc"))
        double.parse("123") shouldEqual Success((123, ""))
        double.parse("123.as") shouldEqual Success((123.0, "as"))
        double.parse("abc") shouldBe a[Failure[_]]
      }
    }
  }

  "Combinators" - {
    "<|>" - {
      "el resultado deberia ser del primer parser que lo retorne" in {
        val aob = char('a') <|> char('b')
        aob.parse("arbol") shouldEqual Success(('a', "rbol"))
        aob.parse("bort") shouldEqual Success(('b', "ort"))
      }
    }

    "<>" - {
      "debería concatenar los resultados de dos parsers" in {
        val holaMundo = string("hola") <> string("mundo")
        holaMundo.parse("holamundo") shouldEqual Success((("hola", "mundo"), ""))
        holaMundo.parse("holamundooo") shouldEqual Success((("hola", "mundo"), "oo"))
        holaMundo.parse("holachau") shouldBe a[Failure[_]]
      }
    }

    "~>" - {
      "debería retornar el resultado del segundo parser" in {
        val holaMundo = string("hola") ~> string("mundo")
        holaMundo.parse("holamundo") shouldEqual Success(("mundo", ""))
        holaMundo.parse("holamundooo") shouldEqual Success(("mundo", "oo"))
        holaMundo.parse("holachau") shouldBe a[Failure[_]]
      }
    }

    "<~" - {
      "debería retornar el resultado del primer parser" in {
        val holaMundo = string("hola") <~ string("mundo")
        holaMundo.parse("holamundo") shouldEqual Success(("hola", ""))
        holaMundo.parse("holamundooo") shouldEqual Success(("hola", "oo"))
        holaMundo.parse("holachau") shouldBe a[Failure[_]]
      }
    }

    "sepBy" - {
      "debería parsear una lista de elementos separados por un separador" in {
        val charInteger = char('-') ~> integer
        charInteger.parse("1234-5678") shouldBe a[Failure[_]]
        charInteger.parse("-1234-5678") shouldEqual Success(1234, "-5678")

        val charIntegerKleene = (char('-') ~> integer).*
        charIntegerKleene.parse("1234-5678") shouldEqual Success(List(), "1234-5678")
        charIntegerKleene.parse("-1234-5678") shouldEqual Success(List(1234, 5678), "")

        val integerCharIntegerKleene = integer <> (char('-') ~> integer).*
        integerCharIntegerKleene.parse("1234-5678") shouldEqual Success((1234, List(5678)), "")
        integerCharIntegerKleene.parse("1234-5678-9") shouldEqual Success((1234, List(5678, 9)), "")
        integerCharIntegerKleene.parse("-1234-5678-9") shouldBe Success(((-1234, List(5678, 9)), ""))
        integerCharIntegerKleene.parse("0--1234-5678-9") shouldBe Success(((0, List(-1234,5678, 9)), ""))

        val digitosSeparadosPorPunto = digit.sepBy(char('.'))
        digitosSeparadosPorPunto.parse("1.2.3.4") shouldEqual Success(List('1','2','3','4'), "")
        digitosSeparadosPorPunto.parse("1234") shouldEqual Success(List('1'), "234")

        val numeroDeTelefono = integer.sepBy(char('-'))
        numeroDeTelefono.parse("4356-1234") shouldEqual Success((List(4356, 1234), ""))
        numeroDeTelefono.parse("hola-chau") shouldBe a[Failure[_]]
        numeroDeTelefono.parse("1234 5678") shouldEqual Success((List(1234), " 5678"))
      }
    }

    "satisfies" - {
      "debería fallar si el resultado no satisface la condición" in {
        val digitoPar = digit.satisfies(_ % 2 == 0)
        digitoPar.parse("246") shouldEqual Success(('2', "46"))
        digitoPar.parse("135") shouldBe a[Failure[_]]
      }
    }

    "opt" - {
      "debería retornar un Some si el parser tiene éxito" in {
        val talVezIn = string("in").opt
        val precedencia = talVezIn <> string("fija")
        precedencia.parse("infija") shouldEqual Success((Some("in"), "fija"), "")
        precedencia.parse("fija") shouldEqual Success((None, "fija"), "")
      }
    }

    "*" - {
      "debería retornar una lista con los resultados de los parsers" in {
        val digitos = digit.*
        digitos.parse("1234") shouldEqual Success((List('1', '2', '3', '4'), ""))
        digitos.parse("1a234") shouldEqual Success((List('1'), "a234"))
        digitos.parse("hola") shouldEqual Success((List.empty, "hola"))
        digitos.parse("") shouldEqual Success((List.empty, ""))
      }
    }

    "+" - {
      "debería retornar una lista con los resultados de los parsers" in {
        val digitos = digit.+
        digitos.parse("1234") shouldEqual Success((List('1', '2', '3', '4'), ""))
        digitos.parse("hola") shouldBe a[Failure[_]]
        digitos.parse("") shouldBe a[Failure[_]]
      }
    }
  }
}