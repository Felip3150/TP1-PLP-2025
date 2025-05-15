module PPON where

import Documento

data PPON = TextoPP String | IntPP Int | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)

--CORRECCION 4

pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  TextoPP _ -> True
  IntPP _ -> True
  _ -> False

--CORRECCION 5 

pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP l) = all (pponAtomico . snd) l
pponObjetoSimple _ = False

--CORRECCION 6
intercalar :: Doc -> [Doc] -> Doc
intercalar _ [] = vacio
intercalar sep docs = foldr1 (\x acc -> x <+> sep <+> acc) docs


entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds = texto "{" <+> indentar 2 (linea <+> intercalar (texto "," <+> linea) ds) <+> linea <+> texto "}"


aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\s acc -> texto s <+> acc) (\_ acc -> texto " " <+> acc)

--CORRECCION 7

-- Recursión estructural 
pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s)
pponADoc (IntPP i) = texto (show i)
pponADoc (ObjetoPP l) = resultado
  where
    docs = map parADoc l
    docsConLlaves = entreLlaves docs

    resultado = if pponObjetoSimple (ObjetoPP l) 
                  then aplanar docsConLlaves 
                  else docsConLlaves

    parADoc (a, b) = texto (show a ++ ": ") <+> pponADoc b
