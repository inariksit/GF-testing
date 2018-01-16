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
  oldGrammarName <- getDataFileName "TestLangOld.pgf"
--  grName <- getDataFileName "MiniLang.pgf" 
--  grName <- getDataFileName "Phrasebook.pgf" 
  gr <- readGrammar grName
  grOld <- readGrammar oldGrammarName
  args <- getArgs
  let debug = "d" `elem` args
  let hole = "h" `elem` args
  let compareWithOld = "cwo" `elem` args


  case args of 

    ("all":_) -> do
      let defaults = words "DetNP"
      let trees_cats = map (treesUsingFun gr) defaults
      mapM_ (mapM_ (assertLin debug gr)) trees_cats



    (detCN:_) -> do
      let trees_cats = treesUsingFun gr detCN
      --mapM_ print trees_cats

      mapM_ (assertLin debug gr) trees_cats

      sequence_ 
        [ testWord gr symb
        | symb <- lookupSymbols gr detCN
        ]




    _ -> sequence_ 
        [ do putStrLn (showConcrFun gr symb)
             --sequence_
             -- [ testHole gr ccat 
             --   | ccat <- nub $ ((\(as,b) -> b:as) . ctyp) symb ]
--             putStrLn "\n\n"
          | symb <- symbols gr ]

  if compareWithOld
   then sequence_ [ do putStrLn $ "### " ++ acat
                       putStr "Compiles to "
                       putStr (show nOld)
                       putStr " concrete categories in old grammar, "
                       putStr (show nNew)
                       putStrLn " in the new grammar.  "
                       putStr "* Labels only in old: "
                       putStrLn $ intercalate ", " labelsOld
                       putStr "* Labels only in new: "
                       putStrLn $ intercalate ", " labelsNew
                       putStrLn ""

                    | (acat, [nOld,nNew], labelsOld, labelsNew) <- diffCats grOld gr ]
     else return ()

    --_ -> sequence_ 
    --    [ testWord gr symb
    --    | c <- wordCats
    --    , let symb = head [ f | f <- symbols gr, arity f == 0, snd (ctyp f) == c ]
    --    ]
    -- where
    --  wordCats = nub [ snd (ctyp f) | f <- symbols gr, arity f == 0 ]


