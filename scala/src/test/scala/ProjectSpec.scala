import Parsers.*
import ParserImagenes.*
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
        integerCharIntegerKleene.parse("0--1234-5678-9") shouldBe Success(((0, List(-1234, 5678, 9)), ""))

        val digitosSeparadosPorPunto = digit.sepBy(char('.'))
        digitosSeparadosPorPunto.parse("1.2.3.4") shouldEqual Success(List('1', '2', '3', '4'), "")
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

  "Parsers de figuras" - {
    "punto" - {
      "debería parsear un punto" in {
        punto.parse("1 @ 2") shouldEqual Success((Punto(1, 2), ""))
        punto.parse("1@2") shouldEqual Success((Punto(1, 2), ""))
        punto.parse("1@ 2") shouldEqual Success((Punto(1, 2), ""))
        punto.parse("1 @2") shouldEqual Success((Punto(1, 2), ""))
        punto.parse("1@2 ") shouldEqual Success((Punto(1, 2), " "))
        punto.parse("1 @ 2 ") shouldEqual Success((Punto(1, 2), " "))
        punto.parse("1@2a") shouldBe Success((Punto(1, 2), "a"))
        punto.parse("1@") shouldBe a[Failure[_]]
        punto.parse("@2") shouldBe a[Failure[_]]
        punto.parse("1") shouldBe a[Failure[_]]
        punto.parse("") shouldBe a[Failure[_]]
      }
    }

    "triángulo" - {
      "debería parsear un triángulo" in {
        triangulo.parse("triangulo[1@2, 3@4, 5@6]") shouldEqual Success(Triangulo(Punto(1, 2), Punto(3, 4), Punto(5, 6)), "")
        triangulo.parse("triangulo[1@2, 3@4, 5@6] ") shouldEqual Success(Triangulo(Punto(1, 2), Punto(3, 4), Punto(5, 6)), " ")
        triangulo.parse("triangulo[1@2, 3@4, 5@6]a") shouldEqual Success(Triangulo(Punto(1, 2), Punto(3, 4), Punto(5, 6)), "a")
        triangulo.parse("triangulo[1@2, 3@4, 5@6") shouldBe a[Failure[_]]
        triangulo.parse("triangulo[1@2, 3@4, 5@6, 7@8]") shouldBe a[Failure[_]]
        triangulo.parse("triangulo[1@2, 3@4]") shouldBe a[Failure[_]]
        triangulo.parse("triangulo[1@2]") shouldBe a[Failure[_]]
      }
    }

    "rectángulo" - {
      "debería parsear un rectángulo" in {
        rectangulo.parse("rectangulo[1@2, 3@4]") shouldEqual Success(Rectangulo(Punto(1, 2), Punto(3, 4)), "")
        rectangulo.parse("rectangulo[1@2, 3@4] ") shouldEqual Success(Rectangulo(Punto(1, 2), Punto(3, 4)), " ")
        rectangulo.parse("rectangulo[1@2, 3@4]a") shouldEqual Success(Rectangulo(Punto(1, 2), Punto(3, 4)), "a")
        rectangulo.parse("rectangulo[1@2, 3@4") shouldBe a[Failure[_]]
        rectangulo.parse("rectangulo[1@2, 3@4, 5@6]") shouldBe a[Failure[_]]
        rectangulo.parse("rectangulo[1@2]") shouldBe a[Failure[_]]
      }
    }

    "círculo" - {
      "debería parsear un círculo" in {
        circulo.parse("circulo[1@2, 3]") shouldEqual Success(Circulo(Punto(1, 2), 3), "")
        circulo.parse("circulo[1@2, 3] ") shouldEqual Success(Circulo(Punto(1, 2), 3), " ")
        circulo.parse("circulo[1@2, 3]a") shouldEqual Success(Circulo(Punto(1, 2), 3), "a")
        circulo.parse("circulo[1@2, 3") shouldBe a[Failure[_]]
        circulo.parse("circulo[1@2, 3, 4]") shouldBe a[Failure[_]]
        circulo.parse("circulo[1@2]") shouldBe a[Failure[_]]
      }
    }

    "grupo" - {
      "debería parsear un grupo de figuras" in {
        val grupoSimple =
          """grupo(
                            triangulo[200 @ 50, 101 @ 335, 299 @ 335],
                            circulo[200 @ 350, 100]
                        )"""

        grupo.parse(grupoSimple) shouldEqual Success(Grupo(List(
          Triangulo(Punto(200, 50), Punto(101, 335), Punto(299, 335)),
          Circulo(Punto(200, 350), 100))), "")

        val grupoAnidado =
          """grupo(
                                  grupo(
                                      triangulo[250 @ 150, 150 @ 300, 350 @ 300],
                                      triangulo[150 @ 300, 50 @ 450, 250 @ 450],
                                      triangulo[350 @ 300, 250 @ 450, 450 @ 450]
                                  ),
                                  grupo(
                                      rectangulo[460 @ 90, 470 @ 100],
                                      rectangulo[430 @ 210, 500 @ 220],
                                      rectangulo[430 @ 210, 440 @ 230],
                                      rectangulo[490 @ 210, 500 @ 230],
                                      rectangulo[450 @ 100, 480 @ 260]
                                  )
                              )"""

        grupo.parse(grupoAnidado) shouldEqual Success(Grupo(List(
          Grupo(List(
            Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300)),
            Triangulo(Punto(150, 300), Punto(50, 450), Punto(250, 450)),
            Triangulo(Punto(350, 300), Punto(250, 450), Punto(450, 450))
          )),
          Grupo(List(
            Rectangulo(Punto(460, 90), Punto(470, 100)),
            Rectangulo(Punto(430, 210), Punto(500, 220)),
            Rectangulo(Punto(430, 210), Punto(440, 230)),
            Rectangulo(Punto(490, 210), Punto(500, 230)),
            Rectangulo(Punto(450, 100), Punto(480, 260))
          ))
        )), "")
      }
    }

    "color" - {
      "debería parsear un color " in {
        color.parse("color[2, 3, 50](triangulo[250 @ 150, 150 @ 300, 350 @ 300])") shouldEqual Success(Color(2, 3, 50, Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300))), "")
        color.parse("color[50, 60,70]()") shouldBe a[Failure[_]]
        color.parse("color[2, 3, 257](triangulo[250 @ 150, 150 @ 300, 350 @ 300])") shouldBe a[Failure[_]]


        val colorConMasDeUnaFigura =
          """color[50, 60, 70](
                triangulo[250 @ 150, 150 @ 300, 350 @ 300],
                triangulo[250 @ 150, 150 @ 300, 350 @ 300]
                )
              """

        color.parse(colorConMasDeUnaFigura) shouldBe a[Failure[_]]


        val colorConGrupo =
          """color[2, 3, 4](
                    grupo(
                      triangulo[250 @ 150, 150 @ 300, 350 @ 300],
                      triangulo[250 @ 150, 150 @ 300, 350 @ 300]
                    )
              )"""

        color.parse(colorConGrupo) shouldEqual Success(Color(2, 3, 4, Grupo(List(
          Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300)),
          Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300))
        ))), "")
      }
    }

    "escala" - {
      "debería parsear una escala" in {
        escala.parse("escala[2, 3](triangulo[250 @ 150, 150 @ 300, 350 @ 300])") shouldEqual Success(
          Escala(2, 3, Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300))), "")
        escala.parse("escala[2, 3]()") shouldBe a[Failure[_]]

        val escalaConMasDeUnaFigura =
          """escala[2, 3](
                triangulo[250 @ 150, 150 @ 300, 350 @ 300],
                triangulo[250 @ 150, 150 @ 300, 350 @ 300]
                )
              """

        escala.parse(escalaConMasDeUnaFigura) shouldBe a[Failure[_]]

        val escalaConGrupo =
          """escala[2, 3](
                        grupo(
                          triangulo[250 @ 150, 150 @ 300, 350 @ 300],
                          triangulo[250 @ 150, 150 @ 300, 350 @ 300]
                        )
                  )"""

        escala.parse(escalaConGrupo) shouldBe Success(Escala(2, 3, Grupo(List(
          Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300)),
          Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300))
        ))), "")
      }
    }

    "rotacion" - {
      "debería parsear una rotacion" in {
        rotacion.parse("rotacion[200](triangulo[250 @ 150, 150 @ 300, 350 @ 300])") shouldEqual Success(Rotacion(200, Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300))), "")
        rotacion.parse("rotacion[50]()") shouldBe a[Failure[_]]
        rotacion.parse("rotacion[200, 50](triangulo[250 @ 150, 150 @ 300, 350 @ 300])") shouldBe a[Failure[_]]

        val rotacionConMasDeUnaFigura =
          """rotacion[50](
                        triangulo[250 @ 150, 150 @ 300, 350 @ 300],
                        triangulo[250 @ 150, 150 @ 300, 350 @ 300]
                        )
                      """

        rotacion.parse(rotacionConMasDeUnaFigura) shouldBe a[Failure[_]]


        val rotacionConGrupo =
          """rotacion[200](
                            grupo(
                              triangulo[250 @ 150, 150 @ 300, 350 @ 300],
                              triangulo[250 @ 150, 150 @ 300, 350 @ 300]
                            )
                      )"""

        rotacion.parse(rotacionConGrupo) shouldEqual Success(Rotacion(200, Grupo(List(
          Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300)),
          Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300))
        ))), "")
      }
    }

    "traslacion" - {
      "debería parsear una traslacion" in {
        traslacion.parse("traslacion[200, 50](triangulo[250 @ 150, 150 @ 300, 350 @ 300])") shouldEqual Success(Traslacion(200, 50, Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300))), "")
        traslacion.parse("traslacion[50, 60]()") shouldBe a[Failure[_]]
        traslacion.parse("traslacion[200, 50, 60](triangulo[250 @ 150, 150 @ 300, 350 @ 300])") shouldBe a[Failure[_]]

        val traslacionConMasDeUnaFigura =
          """traslacion[50, 60](
                        triangulo[250 @ 150, 150 @ 300, 350 @ 300],
                        triangulo[250 @ 150, 150 @ 300, 350 @ 300]
                        )
                      """

        traslacion.parse(traslacionConMasDeUnaFigura) shouldBe a[Failure[_]]


        val traslacionConGrupo =
          """traslacion[200, 50](
                            grupo(
                              triangulo[250 @ 150, 150 @ 300, 350 @ 300],
                              triangulo[250 @ 150, 150 @ 300, 350 @ 300]
                            )
                      )"""

        traslacion.parse(traslacionConGrupo) shouldEqual Success(Traslacion(200, 50, Grupo(List(
          Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300)),
          Triangulo(Punto(250, 150), Punto(150, 300), Punto(350, 300))
        ))), "")
      }
    }
  }
}