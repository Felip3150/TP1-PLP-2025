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
--CORRECCION 1 - INVARIANTE 

-- Asumimos que los documentos d1 y d2 cumplen el invariante de representación de Doc.
-- Necesitamos ver que se preservan los invariantes propuestos durante la función <+> y al finalizar.
-- 
-- Dado un valor de tipo Doc, sea (Texto s d):
--
-- 1. s no debe ser el string vacío.
--      Esto se cumple porque en la definición de <+>:

--        casoTexto s rec = case rec of
--          Texto s2 rec2 -> Texto (s ++ s2) rec2
--          _             -> Texto s rec

--      solo se construye un Texto si s ya proviene de un constructor válido (de d1),
--      y por invariante, nunca puede ser vacío.
--      el caso Texto "" d no puede ocurrir porque s nunca es vacío por invariante.
--
-- 2. s no debe contener saltos de línea.
--      Tanto s como s2 (si rec es Texto) provienen de d1 o d2 que cumplen el invariante =>   
--      ambos strings no contienen \n => su concatenación tampoco.
--
-- 3. d debe ser Vacio o Linea i d'
--      En la fusión Texto (s ++ s2) rec2, el rec2 proviene del Texto s2 rec2 que está en d2.
--      Por invariante, su rec2 ya cumple esta condición: es Vacio o Linea i d'.
--      En el caso Texto s rec, rec viene del fold, y por hipótesis (estructura de d2), 
--      también cumple que rec es Vacio o Linea i d'.
--      Entonces los d generados en Texto s d despues de aplicar <+> preservan esta propiedad.
--
-- Dado un valor de tipo Doc, sea Linea i d:
--
-- 4. i >= 0
--      La definición de <+> deja los constructores Linea intactos (solo los reconstruye durante el fold).
--      Como d1 y d2 cumplen el invariante, todos los i ya son >= 0 => los Linea i d reconstruidos --      también lo cumplen.

(<+>) :: Doc -> Doc -> Doc
d1 <+> d2 = foldDoc d2 casoTexto Linea d1
  where
    casoTexto s rec = case rec of 
      Texto s2 rec2 -> Texto (s ++ s2) rec2
      _             -> Texto s rec

--CORRECCION 2 - INVARIANTE
-- Asumimos que el documento d cumple el invariante de representación de Doc.
-- Qvq indentar i d también lo cumple.
-- Recordamos que el invariante impone lo siguiente:

-- La función indentar transforma el documento usando:
--   foldDoc Vacio Texto casoLinea
--   donde:
--     Texto s d → se reconstruye exactamente igual usando el constructor Texto como funcion, por lo --     tanto, si el invariante se cumplía en d, se sigue cumpliendo => los puntos 1, 2 y 3 se mantienen.
--
--     Linea n d → se transforma en Linea (i + n) d
--     Como por invariante original, n >= 0, y como i es un Int fijo que no se modifica dentro del fold --     i >= 0   &    i + n >= i     =>      i + n >= 0.

-- Por lo tanto, indentar i d preserva el invariante.

indentar :: Int -> Doc -> Doc
indentar i = foldDoc Vacio Texto casoLinea
  where 
    casoLinea n = Linea (i+n) 

--CORRECCION 3
mostrar :: Doc -> String
mostrar = foldDoc "" (++) (\i recDoc -> "\n" ++ replicate i ' ' ++ recDoc)

imprimir :: Doc -> IO ()
imprimir d = putStrLn (mostrar d)