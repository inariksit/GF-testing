module Main where

import GrammarC
import Lib
import Paths_GF_testing
import System.Environment

import Data.List
import Data.Maybe

import System.IO


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering

--  grName <- getDataFileName "TestLang.pgf" 
  grName <- getDataFileName "MiniLang.pgf" 
--  grName <- getDataFileName "Test.pgf" 
  gr <- readGrammar grName
  args <- getArgs
  let debug = "d" `elem` args
  let hole = "h" `elem` args
  case args of 

    (detCN:_) -> do
      let trees_cats = treesUsingFun gr detCN

      mapM_ (assertLin debug gr) trees_cats

      if hole
        then do putStrLn ""
                sequence_ [ testHole gr c | (_,c,_) <- trees_cats ] 
        else return ()


    _ -> sequence_ 
        [ do putStrLn (showConcrFun gr symb)
             sequence_
              [ testHole gr ccat 
                | ccat <- nub $ ((\(as,b) -> b:as) . ctyp) symb ]
             putStrLn "\n\n"
          | symb <- symbols gr ]

