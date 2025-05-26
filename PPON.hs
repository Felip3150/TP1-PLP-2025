module PPON where

import Documento

data PPON = TextoPP String | IntPP Int | ObjetoPP [(String, PPON)]
  deriving (Eq, Show)


pponAtomico :: PPON -> Bool
pponAtomico p = case p of
  TextoPP _ -> True
  IntPP _ -> True
  otherwise -> False

{-
pponObjetoSimple :: PPON -> Bool
pponObjetoSimple p = case p of
  ObjetoPP l -> foldr (\(a, b) acc -> (pponAtomico b) && acc) True l
  otherwise -> False
-}
pponObjetoSimple :: PPON -> Bool
pponObjetoSimple (ObjetoPP X) = all (pponAtomico . snd) X
pponObjetoSimple _ = False



intercalar :: Doc -> [Doc] -> Doc
intercalar d = foldr1 (\d1 acc -> d1 <+> d <+> acc)
--la que va es esta con foldr1

entreLlaves :: [Doc] -> Doc
entreLlaves [] = texto "{ }"
entreLlaves ds = texto "{" <+> indentar 2 (linea <+> intercalar (texto "," <+> linea) ds) <+> linea <+> texto "}"


aplanar :: Doc -> Doc
aplanar = foldDoc vacio 
  (\s acc -> if acc == vacio then texto s else texto s <+> texto " " <+> acc) 
  (\_ acc -> acc)


pericles = ObjetoPP [("nombre", TextoPP "Pericles"), ("edad", IntPP 30)]
merlina = ObjetoPP [("nombre", TextoPP "Merlina"), ("edad", IntPP 24)]
juan = ObjetoPP [("nombre", TextoPP "Juan"), ("edad", IntPP 20)]
addams = ObjetoPP [("0", pericles), ("1", merlina), ("2", juan)]


pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s) 
pponADoc (IntPP i) = texto (show i)
pponADoc (ObjetoPP l) = documentar l
  where documentar = entreLlaves . map (\(a,b) -> if pponObjetoSimple b then texto (show a ++ ": ") <+> aplanar (pponADoc b) else texto (show a ++ ": ") <+> (pponADoc b))


