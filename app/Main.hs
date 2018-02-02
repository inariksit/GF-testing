{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import GrammarC
import Lib
import Paths_GF_testing

import Data.List ( intercalate, groupBy, sortBy )
import qualified Data.Set as S


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
--    ('s':'c':' ':str) -> mapM_ print $ hasConcrString gr str
    "all" -> mapM_ putStrLn [ testTree False gr [] t 
                            | t <- treesUsingFun gr (symbols gr) ]
    fnames -> 
      sequence_
        [ putStrLn $ testFun (debug args) gr grTrans fname
         | fname <- words fnames ]

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
      let difcats = diffCats ogr gr -- (acat, [#o, #n], olabels, nlabels)

      ---------------------------------------------------------------------------
      -- Print statistics about the functions: e.g., in the old grammar,
      -- all these 5 functions used to be in the same category:
      -- [DefArt,PossPron,no_Quant,this_Quant,that_Quant]
      -- but in the new grammar, they are split into two:
      -- [DefArt,PossPron,no_Quant] and [this_Quant,that_Quant].
      let groupFuns grammar = -- :: Grammar -> [[Symbol]]
            concat [ (groupBy sameCCat $ sortBy compareCCat existingFuns)
                   | (cat,_,_,_) <- difcats
                   , let funs = functionsByCat grammar cat
                   , let existingFuns = filter (\s -> snd (ctyp s) `S.member` nonEmptyCats grammar) funs ]


      let sortByName = sortBy (\s t -> name s `compare` name t)
      let writeFunFile groupedFuns file grammar = do
           writeFile file ""
           sequence_ [ do appendFile file "---\n"
                          appendFile file $ unlines
                            [ showConcrFun gr fun
                            | fun <- sortByName funs ]
                      | funs <- groupedFuns ]

      writeFunFile (groupFuns ogr) (langName ++ "-old-funs.md") ogr
      writeFunFile (groupFuns gr)  (langName ++ "-new-funs.md") gr

      putStrLn $ "Created files " ++ langName ++ "-(old|new)-funs.md"
 
      --------------------------------------------------------------------------
      -- generate statistics of the changes in the concrete categories
      let ccatChangeFile = langName ++ "-ccat-changes.md"
      writeFile ccatChangeFile ""
      sequence_
        [ appendFile ccatChangeFile $ unlines
           [ "### " ++ acat
           , show o ++ " concrete categories in the old grammar, "
           , show n ++ " concrete categories in the new grammar.  "
           , "* Labels only in old: " ++ intercalate ", " ol
           , " (" ++ show (length ol) ++ ")"
           , "* Labels only in new: " ++ intercalate ", " nl 
           , " (" ++ show (length nl) ++ ")" ]
        | (acat, [o,n], ol, nl) <- difcats ] 
      putStrLn $ "Created file " ++ ccatChangeFile

      --------------------------------------------------------------------------
      -- print out tests for all functions in the changed cats
      let changedFuns = [ (cat,functionsByCat gr cat) | (cat,_,_,_) <- difcats ]
      let writeLinFile file grammar otherGrammar = do
           writeFile file ""
           sequence_ [ do print cat
                          appendFile file $ unlines
                            [ show comp
                            | t <- treesUsingFun grammar funs
                            , let comp = compareTree grammar otherGrammar t
                            , not $ null $ linTree comp ]
                     | (cat,funs) <- changedFuns ]

      --writeLinFile (langName ++ "-new-lins.md") gr ogr
      --writeLinFile (langName ++ "-old-lins.md") ogr gr
      mapM_ putStrLn $ nub [ show fun | (_,funs) <- changedFuns, fun <- funs ]




 where
  nub = S.toList . S.fromList
  sameCCat :: Symbol -> Symbol -> Bool
  sameCCat s1 s2 = snd (ctyp s1) == snd (ctyp s2)

  compareCCat :: Symbol -> Symbol -> Ordering
  compareCCat s1 s2 = snd (ctyp s1) `compare` snd (ctyp s2)

