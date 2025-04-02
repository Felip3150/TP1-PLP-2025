--foldDoc :: b -> (String -> b -> b) -> (Int -> b -> b)-> Doc -> b
--foldDoc fVacio fTexto fLinea doc = case doc of
--  Vacio -> fVacio
--  Texto s d -> fTexto s (rec d)
--  Linea i d -> fLinea i (rec d)
--  where rec = foldDoc fVacio fTexto fLinea 

--vacio :: Doc
--vacio = Vacio

--(<+>) :: Doc -> Doc -> Doc
--d1 <+> d2 = foldDoc d2 (\s acc -> case acc of 
--  Texto s2 acc2 -> Texto (s++s2) acc2
--  otherwise -> Texto s acc) Linea d1

--indentar :: Int -> Doc -> Doc
--indentar i = foldDoc (Vacio) (\s acc -> Texto s acc) (\i2 acc -> Linea (i+i2) acc)

--mostrar :: Doc -> String
--mostrar = foldDoc "" (\s acc -> s ++ acc) (\i acc -> "\n" ++ replicate i ' ' ++ acc)

--imprimir :: Doc -> IO ()
--imprimir d = putStrLn (mostrar d)

module PPON where

import Documento

data PPON = TextoPP String | IntPP Int | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)



pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  TextoPP s -> True
  IntPP i -> True
  otherwise -> False

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple p = case p of
  ObjetoPP l -> foldr (\(a, b) acc -> (pponAtomico b) && acc) True l
  otherwise -> False


intercalar :: Doc -> [Doc] -> Doc
intercalar d = foldr1 (\d1 acc -> d1 <+> d <+> acc)


entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds = texto "{" <+> indentar 2 (linea <+> intercalar (texto "," <+> linea) ds) <+> linea <+> texto "}"


aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\s acc -> if acc == vacio then texto s else texto s <+> texto " " <+> acc) (\_ acc -> acc)


pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
juan = ObjetoPP [("nombre", TextoPP "Juan"), ("edad", IntPP 20)]
addams = ObjetoPP [("0", pericles), ("1", merlina), ("2", juan)]


pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s) 
pponADoc (IntPP i) = texto (show i)
pponADoc (ObjetoPP l) = (entreLlaves (fAux l))
  where fAux = map (\(a,b) -> if pponObjetoSimple b then texto (show a ++ ": ") <+> aplanar (pponADoc b) else texto (show a ++ ": ") <+> (pponADoc b))


--ObjetoPP [("0", ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]), ("1", ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]), ("2", ObjetoPP [("nombre", TextoPP "Juan"), ("edad", IntPP 20)])]
--["a: " <+> "nombre: "]
