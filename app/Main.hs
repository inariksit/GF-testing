module Main where

import GrammarC
import Lib
import Paths_GF_testing
import System.Environment

import Data.List
import Data.Maybe

--import qualified PGF2

main :: IO ()
main = do
--  grName <- getDataFileName "TestLang.pgf" 
  grName <- getDataFileName "Phrasebook.pgf" 
  gr <- readGrammar grName

--  mapM_ print (productions gr `map` [100..120])
--  print $ coercions gr

  let trees = treesByCCat gr
  mapM_ print $ sort trees         -- with foldl
--  mapM_ (mapM_ print.sort) trees   -- with scanl
  print $ length trees


  putStrLn "---"
  args <- getArgs
  case args of 
    ("all":_) -> mapM_ (assertLin gr) (filter hasArg $ symbols gr)
    (funNm:_) -> assertLin gr (lookupSymbol gr funNm)
