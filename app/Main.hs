{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import GrammarC
import Lib
import EqRel
import Paths_GF_testing

import Data.List ( intercalate, groupBy, sortBy )
import qualified Data.Set as S
import qualified Data.Map as M


import System.Console.CmdArgs hiding ( name )
import qualified System.Console.CmdArgs as A
import System.IO ( stdout, hSetBuffering, BufferMode(..) )

data GfTest 
  = GfTest 
  { grammar      :: Maybe FilePath
  , source       :: Lang
  , translations :: Lang
  , function     :: Name
  , category     :: Cat
  , show_cats    :: Bool
  , debug        :: Bool
  , eq_fields    :: Bool
  , treebank     :: Maybe FilePath
  , old_grammar  :: Maybe FilePath
  } deriving (Data,Typeable,Show,Eq)

gftest = GfTest 
  { grammar      = def &= typFile      &= help "Path to the grammar (PGF) you want to test."
  , source       = def &= A.typ "Eng"  
                       &= A.name "s"   &= help "Concrete language for the chosen grammar."
  , translations = def &= A.typ "\"Eng Swe\"" 
                       &= A.name "t"   &= help "Optional languages to show translations in."
  , function     = def &= A.typ "UseN" &= help "Function to test"
  , category     = def &= A.typ "NP"   &= help "Test all functions that return the given category"
  , show_cats    = def                 &= help "Show all available categories." 
  , debug        = def                 &= help "Show debug output"
  , eq_fields    = def                 &= help "Show fields whose strings are always identical"
  , treebank     = def &= typFile
                       &= A.name "b"   &= help "Path to a treebank."
  , old_grammar  = def &= typFile      &= help "Path to an earlier version of the grammar."

  }


main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering

  args <- cmdArgs gftest

  let absName = case grammar args of 
                  Just fp -> stripPGF fp --doesn't matter if the name is given with or without ".pgf"
                  Nothing -> "TestLang"
  let langName = absName ++ source args
  let langTrans = [ absName ++ t | t <- words (translations args) ]

  grName <- getDataFileName (absName ++ ".pgf")
  gr     <- readGrammar langName grName
  grTrans <- sequence [ readGrammar lt grName | lt <- langTrans ]


  -- Show equal fields
  let tab = M.fromListWith (/\)
            [ (c, eqr)
            | (CC (Just c) _,eqr) <- equalFields gr
            ]
  if eq_fields args
   then sequence_
    [ putStrLn ("==> " ++ c ++ ":\n" ++ cl)
    | (c,eqr) <- M.toList tab
    , let fs = fieldNames gr c
    , cl <- case eqr of
              Top -> ["TOP"]
              Classes xss -> [ unlines (map (fs!!) xs)
                             | xs@(_:_:_) <- xss ]
    ]
   else return ()

  sequence_
    [ putStrLn ("==> " ++ show c ++ ": notUsed = " ++ show notUsed)
    | let top:_ = ccats gr "Utt"
    , (c,is) <- reachableFieldsFromTop gr top
    , let ar     = head $
                    [ length (seqs f) | f <- symbols gr, snd (ctyp f) == c ] ++
                    [ length (seqs f) | (b,a) <- coercions gr, a == c, f <- symbols gr, snd (ctyp f) == b ]
          notUsed = [ i | i <- [0..ar-1], i `notElem` is ]
    , not (null notUsed)
    ]

  -- Show available categories
  if show_cats args 
   then putStrLn $ unlines [ cat | (cat,_,_,_) <- concrCats gr ]
   else return ()

  -- Testing a function or all functions in a category
  case function args of
    [] -> case category args of
            []  -> return ()
            cat -> putStrLn $ unlines 
                    [ testTree False gr [] t 
                    | t <- treesUsingFun gr (functionsByCat gr cat) ]
--    ('s':'c':' ':str) -> mapM_ print $ hasConcrString gr str
    "all" -> putStrLn $ unlines 
              [ testTree False gr [] t 
              | t <- treesUsingFun gr (symbols gr) ]
    fnames -> putStrLn $ unlines
                [ testFun (debug args) gr grTrans fname
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
      oldgr <- readGrammar langName =<< getDataFileName fp
      let ogr = oldgr { concrLang = concrLang oldgr ++ "-OLD" }
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
      let changedFuns = if null difcats 
                         then [ (cat,functionsByCat gr cat) | (cat,_,_,_) <- concrCats gr ]
                          else [ (cat,functionsByCat gr cat) | (cat,_,_,_) <- difcats ]
      let writeLinFile file grammar otherGrammar = do
           writeFile file ""
           sequence_ [ do putStrLn cat
                          appendFile file $ unlines
                            [ show comp
                            | t <- treesUsingFun grammar funs
                            , let comp = compareTree grammar otherGrammar grTrans t
                            , not $ null $ linTree comp ]
                     | (cat,funs) <- changedFuns ]

      writeLinFile (langName ++ "-new-lins.md") gr ogr
      writeLinFile (langName ++ "-old-lins.md") ogr gr
      mapM_ putStr $ nub [ show fun | (_,funs) <- changedFuns, fun <- funs ]




 where
  nub = S.toList . S.fromList
  sameCCat :: Symbol -> Symbol -> Bool
  sameCCat s1 s2 = snd (ctyp s1) == snd (ctyp s2)

  compareCCat :: Symbol -> Symbol -> Ordering
  compareCCat s1 s2 = snd (ctyp s1) `compare` snd (ctyp s2)

  stripPGF :: String -> String
  stripPGF s = case reverse s of
                'f':'g':'p':'.':name -> reverse name
                name                 -> s

