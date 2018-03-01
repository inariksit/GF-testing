{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import GrammarC
import Lib
import EqRel
import Paths_GF_testing

import Control.Monad ( when )
import Data.List ( intercalate, groupBy, sortBy )
import Data.Maybe ( fromMaybe )
import qualified Data.Set as S
import qualified Data.Map as M


import System.Console.CmdArgs hiding ( name )
import qualified System.Console.CmdArgs as A
import System.IO ( stdout, hSetBuffering, BufferMode(..) )

data GfTest 
  = GfTest 
  { grammar       :: Maybe FilePath
  -- Languages
  , lang          :: Lang

  -- Functions and cats
  , function      :: Name
  , category      :: Cat
  , tree          :: String
  , start_cat     :: Maybe Cat
  , show_cats     :: Bool
  , concr_string  :: String

  -- Information about fields
  , equal_fields  :: Bool
  , empty_fields  :: Bool
  , unused_fields :: Bool
  , nullable      :: Bool 

  -- Compare to old grammar
  , old_grammar   :: Maybe FilePath
  , only_changed_cats :: Bool
  , treebank      :: Maybe FilePath
  , debug         :: Bool

  } deriving (Data,Typeable,Show,Eq)

gftest = GfTest 
  { grammar       = def &= typFile      &= help "Path to the grammar (PGF) you want to test"
  , lang          = def &= A.typ "\"Eng Swe\""  
                                        &= help "Concrete syntax + optional translations"
  , tree          = def &= A.typ "\"UseN tree_N\"" 
                        &= A.name "t"   &= help "Test the given tree"
  , function      = def &= A.typ "UseN" &= help "Test the given function(s)"
  , category      = def &= A.typ "NP"
                        &= A.name "c"   &= help "Test all functions with given goal category"
  , start_cat     = def &= A.typ "Utt"
                        &= A.name "s"   &= help "Use the given category as start category"
  , concr_string  = def &= A.typ "the"  &= help "Show all functions that include given string"
  , show_cats     = def                 &= help "Show all available categories" 
  , debug         = def                 &= help "Show debug output"
  , equal_fields  = def &= A.name "q"   &= help "Show fields whose strings are always identical"
  , empty_fields  = def &= A.name "e"   &= help "Show fields whose strings are always empty"
  , unused_fields = def                 &= help "Show fields that never make it into the top category"
  , nullable      = def                 &= help "Show trees that are erased"
  , treebank      = def &= typFile
                        &= A.name "b"   &= help "Path to a treebank"
  , old_grammar   = def &= typFile
                        &= A.name "o"   &= help "Path to an earlier version of the grammar"
  , only_changed_cats = def             &= help "When comparing against an earlier version of a grammar, only test functions in categories that have changed between versions"
  }


main :: IO ()
main = do 
  hSetBuffering stdout NoBuffering

  args <- cmdArgs gftest

  let absName = case grammar args of 
                  Just fp -> stripPGF fp --doesn't matter if the name is given with or without ".pgf"
                  Nothing -> "TestLang"
  let (langName:langTrans) = [ absName ++ t | t <- words (lang args) ]

  grName <- getDataFileName (absName ++ ".pgf")
  gr     <- readGrammar langName grName
  grTrans <- sequence [ readGrammar lt grName | lt <- langTrans ]

  let startcat = startCat gr `fromMaybe` start_cat args

      testTree' t = testTree False gr grTrans t ctxs
       where
        w    = top t
        c    = snd (ctyp w)
        ctxs = concat [ contextsFor gr sc c
                      | sc <- ccats gr startcat ]
  -----------------------------------------------------------------------------
  -- Statistics about the grammar

  let intersectConcrCats cats_fields intersection =
        M.fromListWith intersection
              ([ (c,fields)
              | (CC (Just c) _,fields) <- cats_fields 
              ] ++
              [ (cat,fields)
              | (c@(CC Nothing _),fields) <- cats_fields
              , (CC (Just cat) _,coe) <- coercions gr
              , c == coe
              ])

      printStats tab = 
        sequence_ [ do putStrLn $ "==> " ++ c ++ ": " 
                       putStrLn $ unlines (map (fs!!) xs)
                  | (c,vs) <- M.toList tab
                  , let fs = fieldNames gr c
                  , xs@(_:_) <- [ S.toList vs ]
                  ]

  -- Show empty fields
  when (empty_fields args) $ do
    putStrLn "### Empty fields:"
    printStats $ intersectConcrCats (emptyFields gr) S.intersection
    putStrLn ""

  -- Show question marks
  when (nullable args) $ do
    putStrLn "### Question marks:"
    sequence_
      [ do putStrLn ("==> " ++ show c ++ ":")
           sequence_
             [ do putStrLn ("- Tree:  " ++ show t)
                  putStrLn ("- Lin:   " ++ s)
                 -- putStrLn ("- Parse: " ++ show (take 1 $ parse gr s))
             | t <- ts
             , let s = linearize gr t
             ]
      | top <- take 1 $ ccats gr startcat
      , (c,ts) <- forgets gr top
      ]
    putStrLn ""

  -- Show unused fields
  when (unused_fields args) $ do

    let unused = 
         [ (c,S.fromList notUsed)
         | tp <- ccats gr startcat
         , (c,is) <- reachableFieldsFromTop gr tp
         , let ar = head $
                [ length (seqs f) | f <- symbols gr, snd (ctyp f) == c ] ++
                [ length (seqs f) | (b,a) <- coercions gr, a == c, f <- symbols gr, snd (ctyp f) == b ]
               notUsed = [ i | i <- [0..ar-1], i `notElem` is ]
         , not (null notUsed)
         ]
    putStrLn "### Unused fields:"
    printStats $ intersectConcrCats unused S.intersection
    putStrLn ""

  -- Show equal fields
  let tab = intersectConcrCats (equalFields gr) (/\)
  when (equal_fields args) $ do
    putStrLn "### Equal fields:"
    sequence_
     [ putStrLn ("==> " ++ c ++ ":\n" ++ cl)
     | (c,eqr) <- M.toList tab
     , let fs = fieldNames gr c
     , cl <- case eqr of
               Top -> ["TOP"]
               Classes xss -> [ unlines (map (fs!!) xs)
                              | xs@(_:_:_) <- xss ]
     ]
    putStrLn ""

  -----------------------------------------------------------------------------
  -- Show available categories
  when (show_cats args) $
   putStrLn $ unlines [ cat | (cat,_,_,_) <- concrCats gr ]

  -- Show all functions that contain the given string 
  -- (e.g. English "it" appears in DefArt, ImpersCl, it_Pron, …)
  case concr_string args of
    []  -> return ()
    str -> do putStrLn $ "### The following functions contain the syncategorematic string '" ++ str ++ "':"
              putStr "==> "
              putStrLn $ (intercalate ", ") $ nub [ name s | s <- hasConcrString gr str]

  -- Testing a tree
  case tree args of 
    [] -> return ()
    t  -> putStrLn $ testTree' (readTree gr t)
  -- Testing a function or all functions in a category
  case function args of
    [] -> case category args of
            []  -> return ()
            cat -> putStrLn $ unlines 
                    [ testTree' t
                    | t <- treesUsingFun gr (functionsByCat gr cat) ]
    "all" -> putStrLn $ unlines 
              [ testTree' t
              | t <- treesUsingFun gr (symbols gr) ]
    fnames -> putStrLn $ unlines
                [ testFun (debug args) gr grTrans startcat fname
                | fname <- words fnames ]
{-
              do --sequence_ 
              --    [ do print symb
              --         mapM_ (putStr.intercalate " ".map showSeq.concrSeqs gr) sqs
              --         putStrLn ""
              --    | symb <- lookupSymbol gr fnames
              --    , let sqs = seqs symb ]
                 putStrLn $ unlines
                  [ testFun (debug args) gr grTrans startcat fname
                  | fname <- words fnames ]
                 mapM_ print (coercions gr)
                 let (startccat:_) = ccats gr startcat
                 mapM_ print [ (c, treeWithHole, topOf hl treeWithHole) 
                             | (c,tts) <- contexts gr startccat 
                             , tt <- tts
                             , let hl = hole c
                             , let treeWithHole = tt (App hl [])]
-}
  -------------------------------------------------------------------------------
  -- Comparison with old grammar

  case old_grammar args of
    Nothing -> return ()
    Just fp -> do
      oldgr <- readGrammar langName =<< getDataFileName (stripPGF fp ++ ".pgf")
      let ogr = oldgr { concrLang = concrLang oldgr ++ "-OLD" }
          difcats = diffCats ogr gr -- (acat, [#o, #n], olabels, nlabels)
 
      --------------------------------------------------------------------------
      -- generate statistics of the changes in the concrete categories
      let ccatChangeFile = langName ++ "-ccat-diff.md"
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
      let changedFuns =
           if only_changed_cats args
            then [ (cat,functionsByCat gr cat) | (cat,_,_,_) <- difcats ]
            else
              case function args of
                [] -> [ (cat,functionsByCat gr cat) | (cat,_,_,_) <- concrCats gr ]
                fn -> [ (snd $ GrammarC.typ f, [f]) | f <- lookupSymbol gr fn ]
          writeLinFile file grammar otherGrammar = do
            writeFile file ""
            putStrLn "Testing functions in… "
            sequence_ [ do putStr $ cat ++ "                \r"
                           appendFile file $ unlines
                             [ show comp
                             | t <- treesUsingFun grammar funs
                             , let comp = compareTree grammar otherGrammar grTrans t
                             , not $ null $ linTree comp ]
                      | (cat,funs) <- changedFuns ]

      writeLinFile (langName ++ "-lin-diff.md") gr ogr
      putStrLn $ "Created file " ++ (langName ++ "-lin-diff.md")

      ---------------------------------------------------------------------------
      -- Print statistics about the functions: e.g., in the old grammar,
      -- all these 5 functions used to be in the same category:
      -- [DefArt,PossPron,no_Quant,this_Quant,that_Quant]
      -- but in the new grammar, they are split into two:
      -- [DefArt,PossPron,no_Quant] and [this_Quant,that_Quant].
      let groupFuns grammar = -- :: Grammar -> [[Symbol]]
            concat [ groupBy sameCCat $ sortBy compareCCat funs
                   | (cat,_,_,_) <- difcats
                   , let funs = functionsByCat grammar cat ]


          sortByName = sortBy (\s t -> name s `compare` name t)
          writeFunFile groupedFuns file grammar = do
            writeFile file ""
            sequence_ [ do appendFile file "---\n"
                           appendFile file $ unlines
                             [ showConcrFun gr fun
                             | fun <- sortByName funs ]
                      | funs <- groupedFuns ]

      writeFunFile (groupFuns ogr) (langName ++ "-old-funs.md") ogr
      writeFunFile (groupFuns gr)  (langName ++ "-new-funs.md") gr

      putStrLn $ "Created files " ++ langName ++ "-(old|new)-funs.md"
  -------------------------------------------------------------------------------
  -- Read trees from treebank. No fancier functionality yet.

  case treebank args of
    Nothing -> return ()
    Just fp -> do
      tb <- readFile =<< getDataFileName fp
      sequence_ [ do let tree = readTree gr str
                         (_args,ty) = ctyp (top tree)
                     putStrLn $ unlines [ "", show tree ++ " : " ++ show ty]
                     putStrLn $ linearize gr tree
                     --mapM_ putStrLn $ tabularPrint gr tree
                | str <- lines tb ]


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

  showSeq s = case s of
    Left syncat -> syncat
    Right (i,j) -> "<" ++ show i ++ "," ++ show j ++ ">"
