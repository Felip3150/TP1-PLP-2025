module PPON where

import Documento

data PPON = TextoPP String | IntPP Int | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)


pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  TextoPP _ -> True
  IntPP _ -> True
  otherwise -> False


pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP x) = all (pponAtomico . snd) x


intercalar :: Doc -> [Doc] -> Doc
intercalar d = foldr1 (\d1 rec -> d1 <+> d <+> rec)


entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds = texto "{" <+> indentar 2 (linea <+> intercalar (texto "," <+> linea) ds) <+> linea <+> texto "}"


aplanar :: Doc -> Doc
aplanar = foldDoc vacio (\s rec -> texto s <+> rec) (\_ rec -> texto " " <+> rec)


--esquema de recursion estructural
pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s) 
pponADoc (IntPP i) = texto (show i)
pponADoc (ObjetoPP l) = documentar l
  where documentar = entreLlaves . map (\(a,b) -> if pponObjetoSimple b then texto (show a ++ ": ") <+> aplanar (pponADoc b) else texto (show a ++ ": ") <+> (pponADoc b))

