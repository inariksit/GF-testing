module Main where

import GrammarC
import Lib
import Paths_GF_testing
import System.Environment

import Control.Monad ( when )
import Data.List ( intercalate )

import System.IO


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs

  let debug = "d" `elem` args
  let compareWithOld = "cwo" `elem` args
  let useTreebank = "tb" `elem` args

  let concName = "TestLangDut" --TODO: user input
  let concTrans = "TestLangEng" 

  grName <- getDataFileName "TestLang.pgf" 
  gr     <- readGrammar concName grName
  grTrans <- readGrammar concTrans grName


  case args of 

    ("parse":_) -> mapM_ print $ parse gr "ik ben een hond"

    ("all":_) -> sequence_ $ take 5
      [ testFun debug gr [] (show symb)
        | symb <- symbols gr ]

    (detCN:_) -> testFun debug gr [grTrans] detCN

    _ -> sequence_ 
        [ do putStrLn (showConcrFun gr symb)
          | symb <- symbols gr ]

-------------------------------------------------------------------------------
-- secondary operations: read trees from treebank, compare with old grammar

  when useTreebank $ do 
    treebank <- lines `fmap` (readFile =<< getDataFileName "treebank.txt")
    sequence_ [ do let tree = readTree gr str
                   mapM_ putStrLn $ tabularPrint gr tree
                | str <- treebank ]


  when compareWithOld $ do
    grOld <- readGrammar concName =<< getDataFileName "TestLangOld.pgf"
    sequence_
        [ putStrLn $ unlines $ 
           [ "### " ++ acat
           , (show nOld) ++ " concrete categories in the old grammar, "
           , (show nNew) ++ " concrete categories in the new grammar.  "
           , "* Labels only in old: " ++ intercalate ", " labelsOld
           , "* Labels only in new: " ++ intercalate ", " labelsNew ]
        | (acat, [nOld,nNew], labelsOld, labelsNew) <- diffCats grOld gr ]


