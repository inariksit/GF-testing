module Lib
    ( someFunc
    ) where

import Grammar
import PGF
import Paths_GF_testing

someFunc :: IO ()
someFunc = do
	grName <- getDataFileName "MiniLang.pgf" 
	gr <- readGrammar grName
	mapM_ print $ map (featIth gr (mkCat "Utt") 5 ) [1..10]

-- Level 1: 

--foo :: PGF -> CId -> [[Expr]]
--foo gr fun = case cat of 
--  "Utt" -> 
--  x     -> 

-- where
--  Just ("cat "++cat,p,c) = browse gr fun :: Maybe (String, [CId], [CId])



--args :: [Hypo] -> [CId]
--args [] = []
--args (():hyps)

--arg :: Hypo -> CId
--arg (Explicit, _, typ) = let (_,x,_) = unType typ in x
--arg (Implicit, _, typ) = let (_,x,_) = unType typ in x
