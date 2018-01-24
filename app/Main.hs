{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import GrammarC
import Lib
import Paths_GF_testing

import Data.List ( intercalate, groupBy )

import System.Console.CmdArgs hiding ( name )
import qualified System.Console.CmdArgs as A
import System.IO ( stdout, hSetBuffering, BufferMode(..) )


data GfTest 
  = GfTest 
  { source       :: Lang
  , translations :: Lang
  , function     :: Name
  , debug        :: Bool
  , treebank     :: Maybe FilePath
  , old_grammar  :: Maybe FilePath
  } deriving (Data,Typeable,Show,Eq)

gftest = GfTest 
  { source       = def &= A.typ "Eng"  &= help "Pick the language you want to test."
  , translations = def &= A.typ "\"Eng Swe\"" 
                       &= A.name "t"   &= help "Optional languages to show translations in."
  , function     = def &= A.typ "UseN" &= help "Function to test"
  , debug        = def                 &= help "Show debug output"
  , treebank     = def &= typFile
                       &= A.name "b"   &= help "Path to a treebank"
  , old_grammar  = def &= typFile      &= help "Compare with an old grammar"
  }


main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering

  args <- cmdArgs gftest

  let langName = "TestLang" ++ source args
  let langTrans = [ "TestLang" ++ t | t <- words (translations args) ]

  grName <- getDataFileName "TestLang.pgf" 
  gr     <- readGrammar langName grName
  grTrans <- sequence [ readGrammar lt grName | lt <- langTrans ]


  -- Testing a function
  case function args of
    []    -> return ()
    "all" -> do let randomlyCuratedFuns = 
                            [ head funs | funs <- groupBy (\x y -> name x == name y) (symbols gr) ]
                print (length randomlyCuratedFuns)
                let concrcats = sum
                     [ sum [ 1 | x <- [st..end] ]
                       | (cat,st,end,_) <- concrCats gr ]
                print concrcats
                sequence_ [ testTree False gr [] t 
                           | t <- treesUsingFun gr randomlyCuratedFuns ]
    fname -> testFun (debug args) gr grTrans fname

-------------------------------------------------------------------------------
-- secondary operations: read trees from treebank, compare with old grammar

  case treebank args of
    Nothing -> return ()
    Just fp -> do
      tb <- readFile =<< getDataFileName fp
      sequence_ [ do let tree = readTree gr str
                     let (_args,ty) = ctyp (top tree)
                     putStrLn $ unlines [ "", show tree ++ " : " ++ show ty]
                     putStrLn $ linearize gr tree
                     --mapM_ putStrLn $ tabularPrint gr tree
                | str <- lines tb ]


  case old_grammar args of
    Nothing -> return ()
    Just fp -> do
      ogr <- readGrammar langName =<< getDataFileName fp
      let difcats = diffCats ogr gr

      -- print out tests for all functions in the changed cats
      let changedFuns = [ (cat,functionsByCat gr cat) | (cat,_,_,_) <- difcats ]
      sequence_ [ do putStrLn $ "Testing functions that produce a " ++ cat
                     sequence_ [ testTree False gr [] t | t <- treesUsingFun gr funs ]
                | (cat,funs) <- changedFuns ]

      -- generate statistics of the changes into a file 
      let resultFile = langName ++ "-ccat-changes.md"
      sequence_
        [ appendFile resultFile $ unlines
           [ "### " ++ acat
           , show o ++ " concrete categories in the old grammar, "
           , show n ++ " concrete categories in the new grammar.  "
           , "* Labels only in old: " ++ intercalate ", " ol
           , "* Labels only in new: " ++ intercalate ", " nl ]
        | (acat, [o,n], ol, nl) <- difcats ]

