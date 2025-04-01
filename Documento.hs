module Documento
  ( Doc,
    vacio,
    linea,
    texto,
    foldDoc,
    (<+>),
    indentar,
    mostrar,
    imprimir,
  )
where

data Doc
  = Vacio
  | Texto String Doc
  | Linea Int Doc
  deriving (Eq, Show)

vacio :: Doc
vacio = Vacio

linea :: Doc
linea = Linea 0 Vacio

texto :: String -> Doc
texto t | '\n' `elem` t = error "El texto no debe contener saltos de línea"
texto [] = Vacio
texto t = Texto t Vacio

foldDoc :: a -> (Int -> a -> a) -> (String -> a -> a) -> Doc -> a
foldDoc fVacio fLinea fTexto doc = case doc of
    Vacio     -> fVacio
    Linea n d -> fLinea n (rec d)
    Texto t d -> fTexto t (rec d)
  where rec = foldDoc fVacio fLinea fTexto

-- NOTA: Se declara `infixr 6 <+>` para que `d1 <+> d2 <+> d3` sea equivalente a `d1 <+> (d2 <+> d3)`
-- También permite que expresiones como `texto "a" <+> linea <+> texto "c"` sean válidas sin la necesidad de usar paréntesis.
infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 Linea casoTexto d1
  where casoTexto t1 (Texto t2 d) = Texto (t1++t2) d
        casoTexto t1 d = Texto t1 d

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio casoLinea Texto
  where casoLinea i (Linea n d) = Linea (n+i) d
        casoTexto _ d = d

mostrar :: Doc -> String
mostrar = foldDoc "" (\i d -> '\n' : (repetir i) ++ d) (\t d -> t ++ d)
  where repetir 0 = ""
        repetir i = foldr (\_ acc -> " " ++ acc) "" [1..i]
        -- repetir devuelve un String con la cantidad de espacios que se le pasan como parámetro

-- | Función dada que imprime un documento en pantalla

-- ghci> imprimir (Texto "abc" (Linea 2 (Texto "def" Vacio)))
-- abc
--   def

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)
