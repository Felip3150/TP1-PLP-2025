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
aplanar = foldDoc vacio (\s acc -> texto s <+> acc) (\_ acc -> texto " " <+> acc)


pponADoc :: PPON -> Doc
pponADoc (TextoPP s) = texto (show s) 
pponADoc (IntPP i) = texto (show i)
pponADoc (ObjetoPP l) | pponObjetoSimple (ObjetoPP l) = aplanar (entreLlaves (fAux l))
                      | otherwise = entreLlaves (fAux l)
  where fAux = map (\(a,b) -> if pponObjetoSimple b then texto (show a ++ ": ") <+> aplanar (pponADoc b) else texto (show a ++ ": ") <+> (pponADoc b))