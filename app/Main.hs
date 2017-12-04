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
--  grName <- getDataFileName "Phrasebook.pgf" 
  gr <- readGrammar grName
  args <- getArgs
  case args of 
    ("prons":_) -> do
      let (hePron:_) = lookupSymbols gr "he_Pron"
      let (shePron:_) = lookupSymbols gr "she_Pron"
      print $ tabularLin gr (App hePron [])
      print $ tabularLin gr (App shePron [])

    ("things":_) -> do
      let (x:_) = lookupSymbols gr "something_NP"
      let (y:_) = lookupSymbols gr "everybody_NP"
      print $ tabularLin gr (App x [])
      print $ tabularLin gr (App y [])
      print (x,ctyp x)
      print (y,ctyp y)

      mapM_ print $ concrFuns gr

    ("all":_) -> mapM_ (print . treesUsingFun gr . show) (filter hasArg $ symbols gr)
    (detCN:_) -> do
      --mostly just testing output, functions are not working properly
--      assertLin gr detCN
      putStrLn "---"


      let trees_cats = treesUsingFun gr detCN
      --mapM_ (putStrLn . 
      --      (\(t,c,f) ->  "\n" ++ f ++ "\n" ++
      --             show t ++ " : " ++
      --             show c ++ "\n" ++ -- linearize gr t ++
      --             intercalate "\n" (tabularPrint gr t)))

      --      trees_cats

      let cls = [ (map (coerce gr) args, coerce gr res) 
                  | symb <- lookupSymbols gr detCN
                  , let (args,res) = ctyp symb ]

      let bestCtxs = [ map (bestContexts gr cl) as
                       | (as,cl) <- cls ] 
      print bestCtxs

--      putStrLn "---"
--      mapM_ print $ foldl nextLevel trees_cats (replicate 6 gr)

    _ -> putStrLn "---"

 where
  lookupCat :: Grammar -> String -> ConcrCat 
  lookupCat gr str = snd $ ctyp $ head $ lookupSymbols gr str

  tabularPrint :: Grammar -> Tree -> [String]
  tabularPrint gr t = 
    let cseqs = [ concatMap showCSeq cseq | cseq <- map (concrSeqs gr) (seqs $ top t) ]
        tablins = tabularLin gr t :: [(String,String)]
     in [ fieldname ++ ":\t" ++ lin ++ "\t" ++ s | ((fieldname,lin),s) <- zip tablins cseqs ]

  showCSeq (Left tok) = " " ++ show tok ++ " "
  showCSeq (Right (i,j)) = " <" ++ show i ++ "," ++ show j ++ "> "

