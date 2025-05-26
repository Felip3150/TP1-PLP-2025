module Main (main) where

import Documento
import PPON
import Test.HUnit

main :: IO ()
main = runTestTTAndExit allTests

allTests :: Test
allTests =
  test
    [ "Ejercicio 2" ~: testsEj2,
      "Ejercicio 3" ~: testsEj3,
      "Ejercicio 4" ~: testsEj4,
      "Ejercicio 6" ~: testsEj6,
      "Ejercicio 7" ~: testsEj7,
      "Ejercicio 8" ~: testsEj8,
      "Ejercicio 9" ~: testsEj9
    ]

{-
Respecto a los tests del ejercicio 1, al ser el fold sobre la estructura Doc, no vemos necesario agregar test sobre el, ya que
todo el resto del trabajo practico fue resuelto en base a esa funcion. Entendemos que los otros ejercicios, como sus respectivos
test funcionan como propios test de la funcion foldDoc.
-}

testsEj2 :: Test
testsEj2 =
  test
    [ vacio <+> vacio ~?= vacio,
      texto "a" <+> texto "b" ~?= texto "ab",
      (texto "a" <+> linea) <+> texto "b" ~?= texto "a" <+> (linea <+> texto "b"),
      texto "a" <+> texto "b" <+> texto "c" ~?= texto "abc",
      texto "a" <+> linea <+> texto "b" <+> vacio ~?= texto "a" <+> linea <+> texto "b"
    ]

testsEj3 :: Test
testsEj3 =
  test
    [ indentar 2 vacio ~?= vacio,
      indentar 2 (texto "a") ~?= texto "a",
      indentar 2 (texto "a" <+> linea <+> texto "b") ~?= texto "a" <+> indentar 2 (linea <+> texto "b"),
      indentar 2 (linea <+> texto "a") ~?= indentar 1 (indentar 1 (linea <+> texto "a")),
      indentar 2 (indentar 2 (linea <+> texto "a")) ~?= indentar 4 (linea <+> texto "a"),
      indentar 3 (texto "a" <+> indentar 1 (linea <+> texto "b")) ~?= texto "a" <+> indentar 4 (linea <+> texto "b")
    ]

testsEj4 :: Test
testsEj4 =
  test
    [ mostrar vacio ~?= "",
      mostrar linea ~?= "\n",
      mostrar (texto "jaja") ~?= "jaja", 
      mostrar (indentar 2 (texto "a" <+> linea <+> texto "b")) ~?= "a\n  b",
      mostrar (vacio <+> texto "a") ~?= "a",
      mostrar (indentar 2 (linea <+> indentar 2 (linea <+> texto "a"))) ~?= "\n  \n    a"
    ]

testsEj5 :: Test
testsEj5 =
  test
    [ pponAtomico (TextoPP "juan") ~?= True,
      pponAtomico (IntPP 21) ~?= True,
      pponAtomico merlina ~?= False
    ]



pericles, merlina, addams, familias :: PPON
pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
addams = ObjetoPP [("0", pericles), ("1", merlina)]
familias = ObjetoPP [("Addams", addams)]

testsEj6 :: Test
testsEj6 =
  test
    [ pponObjetoSimple pericles ~?= True,
      pponObjetoSimple addams ~?= False,
      pponObjetoSimple (ObjetoPP [("x", TextoPP "hola"), ("y", IntPP 42)]) ~?= True,
      pponObjetoSimple (ObjetoPP [("clave", ObjetoPP [("z", IntPP 3)])]) ~?= False,
      pponObjetoSimple (ObjetoPP [("clave", TextoPP "a"), ("otra", ObjetoPP [])]) ~?= False
    ]

a, b, c :: Doc
a = texto "a"
b = texto "b"
c = texto "c"

testsEj7 :: Test
testsEj7 =
  test
    [ mostrar (intercalar (texto ", ") [vacio]) ~?= "",
      mostrar (intercalar (texto ", ") [texto "a", texto "b", texto "c"]) ~?= "a, b, c",
      mostrar (entreLlaves [vacio]) ~?= "{\n  \n}",
      mostrar (entreLlaves [texto "a", texto "b", texto "c"]) ~?= "{\n  a,\n  b,\n  c\n}",
      mostrar (intercalar (texto ", ") [texto "a"]) ~?= "a",
      mostrar (intercalar (texto " + ") [texto "a", texto "b"]) ~?= "a + b"
    ]

testsEj8 :: Test
testsEj8 =
  test
    [ mostrar (aplanar (texto "a" <+> linea <+> texto "b" <+> linea <+> texto "c")) ~?= "a b c",
      mostrar (aplanar (texto "x")) ~?= "x",
      mostrar (aplanar vacio) ~?= "",
      mostrar (aplanar (linea <+> texto "a")) ~?= " a",
      mostrar (aplanar (texto "a" <+> linea)) ~?= "a "
    ]

testsEj9 :: Test
testsEj9 =
  test
    [ mostrar (pponADoc pericles) ~?= "{ \"nombre\": \"Pericles\", \"edad\": 30 }",
      mostrar (pponADoc addams) ~?= "{\n  \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n  \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n}",
      mostrar (pponADoc familias) ~?= "{\n  \"Addams\": {\n    \"0\": { \"nombre\": \"Pericles\", \"edad\": 30 },\n    \"1\": { \"nombre\": \"Merlina\", \"edad\": 24 }\n  }\n}",
      mostrar (pponADoc (TextoPP "hola")) ~?= "\"hola\"",
      mostrar (pponADoc (IntPP 42)) ~?= "42",
      mostrar (pponADoc (ObjetoPP [])) ~?= "{ }"
    ]
