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
  }
}