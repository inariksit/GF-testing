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

  grName <- getDataFileName "TestLang.pgf" 
--  grName <- getDataFileName "MiniLang.pgf" 
--  grName <- getDataFileName "Phrasebook.pgf" 
  gr <- readGrammar grName
  args <- getArgs
  let debug = "d" `elem` args
  let hole = "h" `elem` args

  case args of 

    (detCN:_) -> do
      let trees_cats = treesUsingFun gr detCN
      --mapM_ print trees_cats

      mapM_ (assertLin debug gr) trees_cats

      --sequence_ 
      --  [ testWord gr symb
      --  | symb <- lookupSymbols gr detCN
      --  ]

     where
      wordCats = nub [ snd (ctyp f) | f <- symbols gr, arity f == 0 ]


    _ -> sequence_ 
        [ do putStrLn (showConcrFun gr symb)
             --sequence_
             -- [ testHole gr ccat 
             --   | ccat <- nub $ ((\(as,b) -> b:as) . ctyp) symb ]
--             putStrLn "\n\n"
          | symb <- symbols gr ]


    --_ -> sequence_ 
    --    [ testWord gr symb
    --    | c <- wordCats
    --    , let symb = head [ f | f <- symbols gr, arity f == 0, snd (ctyp f) == c ]
    --    ]
    -- where
    --  wordCats = nub [ snd (ctyp f) | f <- symbols gr, arity f == 0 ]


