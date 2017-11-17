module Lib
    ( assertLin
    , nextLevel
    , hasArg
    , lookupSymbols
    , treesUsingFun
    , coerce
    , uncoerce
    ) where

import GrammarC
import Paths_GF_testing
--import Control.Applicative
import Data.List
import Data.Maybe
import Data.Tuple ( swap )
import Debug.Trace


assertLin :: Grammar -> String -> IO ()
assertLin gr fun = do
  let trees = map fst $ treesUsingFun gr fun
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
  symb2table s@(Symbol nm _ _) = (nm,s)
  funsWithArgs = filter hasArg $ symbols gr

concrFuns :: Grammar -> [(ConcrCat,(Symbol,[ConcrCat]))]
concrFuns gr = [ (resCcat,(s,argCcats))  
                  | s@(Symbol _fun _args (argCcats,resCcat)) <- symbols gr
               ]


--------------------------------------------------------------------------------

treesUsingFun :: Grammar -> String -> [(Tree,ConcrCat)]
treesUsingFun gr detCN = [ (tree,np_209)
                 | detCN <- lookupSymbols gr detCN
                 , let (dets_cns,np_209) = ctyp detCN -- :: ([ConcrCat],ConcrCat)
                 , tree <- App detCN <$> bestTrees gr dets_cns ]

  

nextLevel :: [(Tree,ConcrCat)] -> Grammar -> [(Tree,ConcrCat)]
nextLevel trees_cats gr = trees_cats ++ [ undefined | False ] --TODO

 where
  contexts (tree,np_209) =
     [ (res, (name, coerce gr <$> argCCats))
       | (res,(name,argCCats)) <- concrFuns gr
       , let crcs = np_209 : uncoerce gr np_209
       , any (`elem` argCCats) crcs ]
  argTrees ctx = [ bestTrees gr [] | False ]

  {- TODO: filter the list of contexts to best ones. Magic functions in PGF2? 
     Like these things: S3 := <0,0> "är" <1,0>
     Then, for the contexts we chose, find best argument trees.
     Insert tree of (tree,np_209) into the slot, and return.
     Repeat for trees_cats.
  -}




--oldNextLevel :: Grammar -> Symbol -> [[Tree]] -- a list for each concrete category
--oldNextLevel gr origF = concat trees

-- where
----  (argCats,resCat) = typ origF -- e.g. ([Adj,CN],CN) for AdjCN
--  (argCats,resCat) = head $ ctyp origF -- e.g. ([Adj_12,CN_535],CN_560) for AdjCN
--                 -- ? TODO

--  -- gives default tree that uses the function we are testing
--  defTree x | x == resCat = App origF <$> bestTrees gr argCats
--            | otherwise   = map head $ bestTrees gr [x]

--  -- 1) Get all functions in the grammar that use the result category
--  -- 2) Get smallest argument trees to the functions; apply the functions to them 
--  trees = [ [ filter norepeat $ App f <$> sequence argTrees -- 
--              | (argCcats,resCcat) <- ccats
--              , resCat `elem` argCcats
--              , f /= origF -- don't apply another AdjCN if the original is AdjCN
--              , let argTrees = map defTree argCcats  ]

--            | f@(Symbol _ (args, _) ccats) <- symbols gr ]

--------------------------------------------------------------------------------


-- Just a dummy function for getting a quick input of nextLevel
--defaultTrees :: Grammar -> ConcrCat -> [Tree]


bestTrees :: Grammar -> [ConcrCat] -> [[Tree]]
bestTrees gr cs = bestExamples gr $ take 10000
  [ featIthVec gr cats size i
    | size <- [1..5] 
    , let card = featCardVec gr cats size 
    , i <- [0..card-1]
   ]

 where
  cats = map (coerce gr) cs 
  --[ case lookup c (coercions gr) of
  --            Nothing -> c
  --            Just c' -> trace (show c ++ " is a coercion to " ++ show c') $ c'
  --         | c <- cs ]


--------------------------------------------------------------------------------


testsAsWellAs :: (Eq a, Eq b) => [a] -> [b] -> Bool
xs `testsAsWellAs` ys = go (xs `zip` ys)
 where
  go [] =
    True
    
  go ((x,y):xys) =
    and [ y' == y | (x',y') <- xys, x == x' ] &&
    go [ xy | xy@(x',_) <- xys, x /= x' ]


bestExamples :: Grammar -> [[Tree]] -> [[Tree]]
bestExamples gr vtrees = go [] vtrees_lins

 where
  vtrees_lins = [ (vtree, concatMap (tabularLin gr) vtree) --this linearises all trees at once, no interplay with 
                 | vtree <- vtrees ] :: [([Tree],[String])]

  go cur []  = map fst cur
  go cur (vt@(ts,lins):vts) 
    | any (`testsAsWellAs` lins) (map snd cur) = go cur vts
    | otherwise = go' (vt:[ c | c@(ts,clins) <- cur
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
  Symbol _ ([], _) _ -> False
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


