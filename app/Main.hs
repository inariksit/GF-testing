module Main where

import GrammarC
import Lib
import Paths_GF_testing
import System.Environment

import Data.List

main :: IO ()
main = do
  grName <- getDataFileName "TestLang.pgf" 
  gr <- readGrammar grName  
--  mapM_ print $ concrCats gr
  let n = 5
  mapM_ (print.bestExamples gr.map foo) $ funsByConcrCat gr (mkCat "N")
  mapM_ (print.bestExamples gr.map foo) $ funsByConcrCat gr (mkCat "V")
  --mapM_ (print.take n) $ funsByConcrCat gr (mkCat "V2")
  --mapM_ (print.take n) $ funsByConcrCat gr (mkCat "VP")

 where
  foo :: Symbol -> Tree
  foo noun = App noun []   

  --mapM_ print $ nub $ filter (not.null) $ sort $ funsByConcrCat gr (mkCat "VP")

  --args <- getArgs
  --case args of 
  --  ("all":_) -> mapM_ (assertLin gr) (filter hasArg $ symbols gr)
  --  (funNm:_) -> assertLin gr (lookupSymbol gr funNm)