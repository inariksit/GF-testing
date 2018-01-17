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
  args <- getArgs

  let debug = "d" `elem` args
  let compareWithOld = "cwo" `elem` args

  grName <- getDataFileName "TestLang.pgf" 
  gr <- readGrammar grName
  grOld <- if compareWithOld
             then readGrammar =<< getDataFileName "TestLangOld.pgf"
             else return gr



  case args of 

    ("all":_) -> sequence_ $ take 5
      [ testFun debug gr (show symb)
         | symb <- symbols gr ]



    (detCN:_) -> testFun debug gr detCN

    _ -> sequence_ 
        [ do putStrLn (showConcrFun gr symb)
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


