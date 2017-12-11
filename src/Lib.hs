module Lib
    ( assertLin
    , nextLevel
    , hasArg
    , lookupSymbols
    , treesUsingFun
    , coerce
    , uncoerce
    , concrFuns
    ) where

import GrammarC
import Paths_GF_testing
import Data.List
import Data.Either
import qualified Data.Set as S
import Data.Maybe
import Data.Tuple ( swap )
import Debug.Trace


assertLin :: Grammar -> String -> IO ()
assertLin gr fun = do
  let trees = [ tree | (tree,_,_) <- treesUsingFun gr fun ]
  print fun
--  print (length trees)
  pr (trees, map (linearize gr) trees)
 where
  pr ([],[]) = putStrLn ""
  pr (t:ts,l:ls) = do let typT = snd $ ctyp $ top t
                      putStrLn (show t ++ " : " ++ show typT)
                      print l 
                      putStrLn ""
                      pr (ts,ls)

lookupSymbols :: Grammar -> String -> [Symbol]
lookupSymbols gr str = 
  lookupAll (symb2table <$> symbols gr) (mkName str)
 where
  symb2table s@(Symbol nm _ _ _) = (nm,s)


concrFuns :: Grammar -> [(ConcrCat,(Symbol,[ConcrCat]))]
concrFuns gr = [ (resCcat,(s,argCcats))  
                  | s@(Symbol _fun _seqs _args (argCcats,resCcat)) <- symbols gr
               ]


--------------------------------------------------------------------------------

treesUsingFun :: Grammar -> String -> [(Tree,ConcrCat,String)] --last 2 for debug
treesUsingFun gr funname = 
  [ (tree,np_209,concrFunStr)
    | detCN <- lookupSymbols gr funname
    , let (dets_cns,np_209) = ctyp detCN -- :: ([ConcrCat],ConcrCat)
    , tree <- App detCN <$> bestTrees detCN gr dets_cns
    , let concrFunStr = show detCN ++ " : " ++ show (coerce gr <$> dets_cns) ++ " -> " ++ show np_209 ]

  

nextLevel :: [(Tree,ConcrCat)] -> Grammar -> [(Tree,ConcrCat)]
nextLevel trees_cats gr = trees_cats ++ [ undefined | False ] --TODO

 -- [ 
 --   | (tree,np_209) <- trees_cats 
 --   , (resCCat, (symb, argCCats)) <- contexts (tree,np_209) -- :: (ConcrCat,(Symbol,[ConcrCat]))

 --    --Replace whatever bestTrees gives by the tree from 
 --   , let argTrees = map (map (replace (tree,np_209)) (bestTrees gr argCCats)

 --   , let ... = App symb <$> argTrees 
 -- ]

 --where
 -- replace :: (Tree,ConcrCat) -> Tree -> Tree
 -- replace (detCNtree,np_209) tree = 
 --   let (_,ccat) = ctyp $ top tree
 --    in if ccat==np_209 then detCNtree else tree


 -- contexts (tree,np_209) =
 --    [ (res, (symb, coerce gr <$> argCCats))
 --      | (res,(symb,argCCats)) <- concrFuns gr
 --      , let crcs = np_209 : uncoerce gr np_209
 --      , any (`elem` argCCats) crcs ]

 -- argTrees ctx = [ bestTrees gr [] | False ]



  {- TODO: filter the list of contexts to best ones. Magic functions in PGF2? 
     Like these things: S3 := <0,0> "är" <1,0>
     Then, for the contexts we chose, find best argument trees.
     Insert tree of (tree,np_209) into the slot, and return.
     Repeat for trees_cats.
  -}

--------------------------------------------------------------------------------

bestTrees :: Symbol -> Grammar -> [ConcrCat] -> [[Tree]]
bestTrees fun gr cs = bestExamples fun gr $ take 10000
  [ featIthVec gr cats size i
    | size <- [1..5] 
    , let card = featCardVec gr cats size 
    , i <- [0..card-1]
   ]

 where
  cats = map (coerce gr) cs 

--------------------------------------------------------------------------------

testsAsWellAs :: (Eq a, Eq b) => [a] -> [b] -> Bool
xs `testsAsWellAs` ys = go (xs `zip` ys)
 where
  go [] =
    True
    
  go ((x,y):xys) =
    and [ y' == y | (x',y') <- xys, x == x' ] &&
    go [ xy | xy@(x',_) <- xys, x /= x' ]


bestExamples :: Symbol -> Grammar -> [[Tree]] -> [[Tree]]
bestExamples fun gr vtrees = go [] vtrees_lins

 where
  syncategorematics = concatMap (lefts . concrSeqs gr) (seqs fun)
  vtrees_lins = [ (vtree, syncategorematics ++ concatMap (map snd . tabularLin gr) vtree) --this linearises all trees at once, no interplay with 
                 | vtree <- vtrees ] :: [([Tree],[String])]

  go cur []  = map fst cur
  go cur (vt@(ts,lins):vts) 
    | any (`testsAsWellAs` lins) (map snd cur) = go cur vts
    | otherwise = go' (vt:[ c | c@(_,clins) <- cur
                              , not (lins `testsAsWellAs` clins) ])
                      vts

  go' cur vts | enough cur = map fst cur
              | otherwise  = go cur vts

  enough :: [([Tree],[String])] -> Bool
  enough [(_,lins)] = all len1 (group $ sort lins) -- can stop earlier but let's not do that
  enough _          = False 

  len1 [x] = True
  len1 _   = False

--------------------------------------------------------------------------------

hasArg :: Symbol -> Bool
hasArg s = case s of
  Symbol _ _ ([], _) _ -> False
  _                  -> True

lookupAll :: (Eq a) => [(a,b)] -> a -> [b]
lookupAll kvs key = [ v | (k,v) <- kvs, k==key ]

coerce :: Grammar -> ConcrCat -> ConcrCat
coerce gr ccat = fromMaybe ccat (lookup ccat (coercions gr))

uncoerce :: Grammar -> ConcrCat -> [ConcrCat]
uncoerce gr = lookupAll (map swap $ coercions gr)

isSubtree :: Tree -> Tree -> Bool
isSubtree t1 t2 = t1 `elem` args t2

norepeat :: Tree -> Bool
norepeat t = let tops = collapse t in nub tops == tops

collapse :: Tree -> [Symbol]
collapse (App tp as) = tp : concatMap collapse as


