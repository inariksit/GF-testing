module Main where

import GrammarC
import Lib
import Paths_GF_testing
import System.Environment

import Data.List
import Data.Maybe


main :: IO ()
main = do
  grName <- getDataFileName "TestLang.pgf" 
--  grName <- getDataFileName "Phrasebook.pgf" 
  gr <- readGrammar grName
  args <- getArgs
  case args of 
    ("all":_) -> mapM_ (assertLin gr) (map show $ filter hasArg $ symbols gr)
    (detCN:_) -> do
      --mostly just testing output, functions are not working properly
      assertLin gr detCN
      putStrLn "---"

      let detCNs = lookupSymbols gr detCN
      mapM_ print [ (cArgs,res) 
                    | (args,res)<- map ctyp detCNs
                    , let cArgs = map (coerce gr) args ]
      putStrLn "---"

      let trees_cats = treesUsingFun gr detCN
      mapM_ (putStrLn . 
            (\(t,c) -> show t ++ " : " ++
                   show c ++ "\n" ++ 
                   intercalate ", " (tabularLin gr t))) 
            trees_cats
      putStrLn "---"
      mapM_ print $ foldl nextLevel trees_cats (replicate 3 gr)

    _ -> putStrLn "---"
