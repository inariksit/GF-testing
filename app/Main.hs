module Main where

import GrammarC
import Lib
import Paths_GF_testing
import System.Environment

main :: IO ()
main = do
  grName <- getDataFileName "TestLang.pgf" 
  gr <- readGrammar grName  
  args <- getArgs
  case args of 
    ("all":_) -> mapM_ (assertLin gr) (filter hasArg $ symbols gr)
    (funNm:_) -> assertLin gr (lookupSymbol gr funNm)
