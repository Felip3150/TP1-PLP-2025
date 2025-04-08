module Documento
  ( Doc, vacio, linea, texto, foldDoc, (<+>), indentar, mostrar,
    imprimir,
  )
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
Como tengo la funcion infixr, que asocia a derecha toda expresion de 3 o mas "<+>", siempre se va a ejecutar con dos "Doc". Ademas, como es un conector
que se usa sobre las funciones texto, linea y vacio como tal, y no sobre uno de los posibles tipos de dato "Doc", son las propias funciones las que 
aseguran que se cumplen los invariantes 
-}
(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 (\s acc -> case acc of 
  Texto s2 acc2 -> Texto (s++s2) acc2
  otherwise -> Texto s acc) Linea d1
--INVARIANTE
{-
Lo mismo que antes, la funcion se aplica sobre las funciones texto, linea y vacio, que por como son, aseguran que se cumpla en invariante de cada 
uno de los constructores, luego cada una hace el reemplazo por una estructura propia del data Doc, y con eso el fold identifica cada tipo de estructura 
del data Doc.
-}
indentar :: Int -> Doc -> Doc
indentar i = foldDoc (Vacio) (\s acc -> Texto s acc) (\i2 acc -> Linea (i+i2) acc)

mostrar :: Doc -> String
mostrar = foldDoc "" (\s acc -> s ++ acc) (\i acc -> "\n" ++ replicate i ' ' ++ acc)

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)

