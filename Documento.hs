module Documento
  ( Doc, vacio, linea, texto, foldDoc, (<+>), indentar, mostrar,imprimir,)
where

data Doc = Vacio | Texto String Doc | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b)-> Doc -> b
foldDoc fVacio fTexto fLinea doc = case doc of
  Vacio -> fVacio
  Texto s d -> fTexto s (rec d)
  Linea i d -> fLinea i (rec d)
  where rec = foldDoc fVacio fTexto fLinea 

infixr 6 <+>

--INVARIANTE
{-
Asumimos que el invariante se cumple tanto para d1 como para d2, entonces queremos probar que vale para d1 <+> d2.
Hacemos induccion sobre d1, entonces por extencionalidad de Doc, vemos los casos para cada forma de d1.

Caso d1 = Vacio
  Vacio <+> d2
  foldDoc d2 (\s acc -> case acc of 
    Texto s2 acc2 -> Texto (s++s2) acc2 
    otherwise -> Texto s acc) Linea Vacio
  d2
El resultado es d2, y como estoy asumiendo que los invariantes se cumplen tanto para d1 como d2, para este caso tambien se cumple.

caso d1 = Texto s d
  Texto s d <+> d2
  foldDoc d2 (\s acc -> case acc of 
    Texto s2 acc2 -> Texto (s++s2) acc2 
    otherwise -> Texto s acc) Linea (Texto s d)

En este punto, lo que queda es un "Texto s" y como documento asociado el resultado de aplicar recursivamente el fold a, en este
caso d. Ahi podemos separar el flujo en dos casos, si el resultado del fold es otro Texto, concatena su string con el anterior, 
sino, simplemente junta el Texto anterior con el nuevo documento. Detallo porque se cumple cada invariante de Texto:
  
  -"s no debe ser el string vacio y s no debe contener saltos de linea": Esto es asi ya que, en el caso de que el resultado del fold 
    sobre d sea otro Texto, se devuelve un unico Texto cuyo string asociado es el del Texto 1 (el anterior) y el del fold, 
    concatenados, y podemos asegurar que no tienen el string vacio ni un salto de linea, ya que una parte viene de lo que seria d1, 
    que asumimos que cumple con los invariantes, y la segunda viene del documento que estaba asociado al mismo Texto d1, que, como 
    ya dijimos, estamos asumiendo que cumple con los invariantes.

  -"d debe ser Vacio o Linea i d’": Esto es asi ya que, si el resultado de la recursion es otro Texto, se devuelve un unico Texto
    con los strings concatenados y el documento asociado. Y en cualquier otro caso, simplemente un Texto con su string original y
    el resto de su documento asociado. En otras palabras, no puede pasar que el resultado tenga un Texto y como documento directamente
    asociado otro texto, ya que la funcion se encarga de unirlos concatenando sus strings, dejando como opciones unicamente una
    Linea con alguna "i" y algun "d", o directamente un Vacio.

  Luego, respecto a acc, como sabemos por la definicion de foldDoc que es el resultado de aplicar el propio foldDoc a d, el documento
  asociado en este caso al Texto. Desde el principio asumimos que cumple con los invariantes, y mientras se aplica el fold se encuentra
  con los mismos casos que ya detallamos que hacen que aun se cumplan los invariantes. Recursivamente va a recorrer el documento d hasta
  llegar a la "base", ahi deberia encontrar un vacio, que luego de aplicar la funcion sigue cumpliendo el invariante. Luego arrastra 
  el resutado al nivel superior, que puede ser un Texto (detallado antes) o una Linea (detallado abajo), seguidamente hasta completar 
  todo el documento.

caso d1 = Linea i d 
  Linea i d <+> d2
  foldDoc d2 (\s acc -> case acc of 
    Texto s2 acc2 -> Texto (s++s2) acc2 
    otherwise -> Texto s acc) Linea (Linea s d)

En este punto, lo que se va a devolver es una "Linea i d'" siendo d' el resultado de aplicarle recursivamente el fold a d, Detallo
porque se cumple el invariante:
  -"Sea Linea i d entonces i >= 0": Esto se cumple, ya que lo que hace la funcion es simplemente, para el caso de que se tope con 
    un doc "Linea", devolver otra Linea con el mismo valor de i que la que se encontro, que como estoy asumiendo que cumple con el 
    invariante, al devolver otra Linea sin alterar el valor de i, si el original cumple con el invariante, luego de la funcion 
    tambien se va a cumplir.
-}
foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b)-> Doc -> b
foldDoc fVacio fTexto fLinea doc = case doc of
  Vacio -> fVacio
  Texto s d -> fTexto s (rec d)
  Linea i d -> fLinea i (rec d)
  where rec = foldDoc fVacio fTexto fLinea 

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 (\s acc -> case acc of 
  Texto s2 acc2 -> Texto (s++s2) acc2 
  otherwise -> Texto s acc) Linea d1


{-INVARIANTE
Aca asumimos que los invariantes valen para todo d :: Doc, y queremos probar que vale para lo siguiente
para todo i :: int, para todo d :: Doc. indentar i d cumple con los invariantes

caso d = Vacio
  indentar i Vacio
  foldDoc Vacio Texto (\i2 acc -> Linea (i+i2) acc) Vacio
  Vacio

Este caso, como lo devuelto es Vacio, y no hay ningun invariente en particular que involucre a Vacio, decimos que esta probado.

caso d = Texto s d
  indentar i (Texto s d)
  foldDoc Vacio Texto (\i2 acc -> Linea (i+i2) acc) (Texto s d)
  Texto s (foldDoc Vacio Texto (\i2 acc -> Linea (i+i2) acc) d)

En este caso, desde este punto, se va a devolver un Texto con su string original y un documento asociado que es el resultado de 
aplicarle el fold al documento asociado original del Texto. Desarrollamos el porque cumple con los invariantes.
  
  -"s no debe ser el string vacio y s no debe contener saltos de linea": Se debe a que, partiendo de la base de que "Texto s d" 
  cumple con los invariantes, luego la funcion no realiza ninguna modificacion sobre el "s" del Texto, por ende, si los cumple
  antes de aplicarle la funcion, lo cumple despues.
  
  -"d debe ser Vacio o Linea i d’": Por lo mismo de antes, asumiendo que "Texto s d" cumple con este invariante, la funcion no crea
  y agrega un nuevo Texto al documento final, recorre el documento dado como parametro, haciendo modificaciones particulares cuando
  encuentra una Linea. Entonces, si el "Texto s d" original cumple con este invariante, luego de aplicarle la funcion va a hacerlo.


caso d = Linea j d
  indentar i (Linea j d)
  foldDoc Vacio Texto (\i2 acc -> Linea (i+i2) acc) (Linea j d)
  (\i2 acc -> Linea (i+i2) acc) j ((\i2 acc -> Linea (i+i2) acc) d)
  Linea (i+j) ((\i2 acc -> Linea (i+i2) acc) d)

En este punto, lo que va a devolver es una Linea con el valor de cantidad de espacios igual a la suma del valor que tenia la Linea
original que se encontro el fold, mas el valor que se paso como parametro en la funcion indentar. Detallamos porque se cumplen los 
invariantes:

  -"Sea Linea i d entonces i >= 0": Esto se cumple ya que estamos asumiendo que "Linea i d" cumple con el invariante, por lo que
  como la funcion lo que hace es, si se encuentra un Linea, devolver otro Linea con el valor de espacios siendo la suma entre
  el valor dado como parametro en la funcion indentar (que por consigna es mayor que 0), y el que tenia la Linea que se encontro
  el fold, que estamos asumiendo que cumple el invariante, por lo que es mayor o igual a 0. Luego la suma de dos naturales, natural-
  mente es mayor que 0, por lo que se cumple el invariante. 
  Luego, respecto a acc, como sabemos por la definicion de foldDoc que es el resultado de aplicar el propio foldDoc a d, el documento
  asociado en este caso a la Linea. Desde el principio asumimos que cumple con los invariantes, y mientras se aplica el fold se encuentra
  con los mismos casos que ya detallamos que hacen que aun se cumplan los invariantes. Recursivamente va a recorrer el documento d hasta
  llegar a la "base", ahi deberia encontrar un vacio, que luego de aplicar la funcion sigue cumpliendo el invariante. Luego arrastra 
  el resutado al nivel superior, que puede ser un Texto (detallado antes) o una Linea (detallado en el anterior parrafo), seguidamente
  hasta completar todo el documento.
-}

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio Texto (\i2 acc -> Linea (i+i2) acc)


mostrar :: Doc -> String
mostrar = foldDoc "" (++) (\i acc -> "\n" ++ replicate i ' ' ++ acc)


imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)

