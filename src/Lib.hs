module Lib
    ( assertLin
    , hasArg
    , lookupSymbols
    , bestExamples
    , bestContext
    , treesByCCat
    , defaultTrees
    , concrFuns
    ) where

import GrammarC
import Paths_GF_testing
import Control.Applicative
import Data.List
import Data.Maybe
import Debug.Trace


assertLin :: Grammar -> Symbol -> IO ()
assertLin gr fun = do
  let treesUsingFun = concat $ nextLevel gr fun
  print fun
  pr (treesUsingFun, map (linearize gr) treesUsingFun)
 where
  pr ([],[]) = putStrLn ""
  pr (t:ts,l:ls) = do putStrLn (show t ++ " : " ++ concatMap (show.snd) (ctyp $ top t))
                      print l 
                      putStrLn ""
                      pr (ts,ls)

lookupSymbols :: Grammar -> String -> [Symbol]
lookupSymbols gr str = 
  lookupAll (symb2table <$> symbols gr) (mkName str)
 where
  symb2table s@(Symbol nm _ _) = (nm,s)
  funsWithArgs = filter hasArg $ symbols gr

--------------------------------------------------------------------------------
-- Method 1, without FEAT. 
-- Slow but gives at least some trees for more complicated grammars.

--treesByCCat :: Grammar -> [(ConcrCat,Tree)]
treesByCCat gr = bestTrees gr $ foldl findTrees zeroArgTrees (replicate 10 gr)
  --use scanl to find out how many trees you get at each step

 where
  zeroArgTrees = bestTrees gr
    [ (ccat, App fun []) 
      | ( ccat@(CC (Just cat) _)     -- Assuming no coercions -- TODO is this correct?
        , (fun,[]) ) <- concrFuns gr -- Match only trees with 0 arguments
    ]

findTrees :: [(ConcrCat,Tree)] -> Grammar -> [(ConcrCat,Tree)]
findTrees nArgTrees gr
    = [ ( resCcat, App s bestArgs) 
         | s@(Symbol f t ccats) <- symbols gr
         , (argCcats,resCcat) <- ccats
         , let argsByCcat = map (lookupAll nArgTrees) argCcats :: [[Tree]]
         , all (not.null) argsByCcat -- if ≤1 of the argCcats is not in nArgTrees, break
         , let bestArgs = map (bestExample gr) argsByCcat
      ] ++ nArgTrees 

concrFuns :: Grammar -> [(ConcrCat,(Symbol,[ConcrCat]))]
concrFuns gr = [ (resCcat,(s,argCcats))  
                  | s@(Symbol _fun _args ccats) <- symbols gr
                  , (argCcats,resCcat) <- ccats ]

bestTrees :: Grammar -> [(ConcrCat,Tree)] -> [(ConcrCat,Tree)]
bestTrees gr = map (bestTree gr) . groupBy (\(a,_) (b,_) -> a==b) . sort
 
bestTree :: Grammar -> [(ConcrCat,Tree)] -> (ConcrCat,Tree)
bestTree gr ccats_trees = (head ccats, bestExample gr trees)
 where (ccats,trees) = unzip ccats_trees

-- out of (name,[args]) that all return something of the same type.
-- i.e. group the results of concrFuns
bestContext :: Grammar -> [(Symbol,[ConcrCat])] -> (Tree,ConcrCat) -> Tree
bestContext gr funs_args (argtree,argccat) = trace (show trees) $ bestExample gr trees
 where 
  c2t :: ConcrCat -> Maybe Tree
  c2t c | c == argccat = Just argtree
        | otherwise = case defaultTrees gr c of 
                       []    -> trace ("bestContext: empty list " ++ show c) $ Nothing 
                       (t:_) -> Just t

  trees = [ App fun (catMaybes argtrees)
            | (fun,argcats) <- funs_args 
            , let argtrees = map c2t argcats
            , all isJust argtrees ]



--------------------------------------------------------------------------------
-- Method 2, with FEAT. Doesn't run at all for e.g. Basque or Estonian.


nextLevel :: Grammar -> Symbol -> [[Tree]] -- a list for each concrete category
nextLevel gr origF = concat trees

 where
--  (argCats,resCat) = typ origF -- e.g. ([Adj,CN],CN) for AdjCN
  (argCats,resCat) = head $ ctyp origF -- e.g. ([Adj_12,CN_535],CN_560) for AdjCN
                 -- ? TODO

  -- gives default tree that uses the function we are testing
  defTree x | x == resCat = App origF <$> sequence (defaultTrees gr <$> argCats)
            | otherwise   = defaultTrees gr x

  -- 1) Get all functions in the grammar that use the result category
  -- 2) Get smallest argument trees to the functions; apply the functions to them 
  trees = [ [ filter norepeat $ App f <$> sequence argTrees -- 
              | (argCcats,resCcat) <- ccats
              , resCat `elem` argCcats
              , f /= origF -- don't apply another AdjCN if the original is AdjCN
              , let argTrees = map defTree argCcats  ]

            | f@(Symbol _ (args, _) ccats) <- symbols gr ]

--------------------------------------------------------------------------------

norepeat :: Tree -> Bool
norepeat t = let tops = collapse t in nub tops == tops

collapse :: Tree -> [Symbol]
collapse (App tp as) = tp : concatMap collapse as


-- Just a dummy function for getting a quick input of nextLevel
defaultTrees :: Grammar -> ConcrCat -> [Tree]
defaultTrees gr c = case nonEmptySizes of
  [] -> trace ("defaultTrees: no trees for " ++ show cat) $ []
  cs -> bestExamples gr
          [ featIth gr cat size card 
            | (size,cd) <- cs
            , card <- [0..cd-1] ]
 where
  cat = case lookup c (coercions gr) of
              Nothing -> c
              Just c' -> trace (show c ++ " is a coercion to " ++ show c') $ c'
  nonEmptySizes = [ (size,card) | size <- [1..3]
                                , let card = featCard gr cat size 
                                , card > 0 ]


--------------------------------------------------------------------------------

hasArg :: Symbol -> Bool
hasArg s = case s of
  Symbol _ ([], _) _ -> False
  _                  -> True

lookupAll :: (Eq a) => [(a,b)] -> a -> [b]
lookupAll kvs key = [ v | (k,v) <- kvs, k==key ]

testsAsWellAs :: (Eq a, Eq b) => [a] -> [b] -> Bool
xs `testsAsWellAs` ys = go (xs `zip` ys)
 where
  go [] =
    True
    
  go ((x,y):xys) =
    and [ y' == y | (x',y') <- xys, x == x' ] &&
    go [ xy | xy@(x',_) <- xys, x /= x' ]

bestExample :: Grammar -> [Tree] -> Tree
bestExample gr ts = head $ bestExamples gr ts

bestExamples :: Grammar -> [Tree] -> [Tree]
bestExamples gr [] = []
bestExamples gr trees = take 1 $ map fst $ sortBy (goodTest `mappend` shorterTree) trees_lins

 where
  trees_lins = [ (tree,map snd (tabularLin gr tree)) | tree <- trees ]
  goodTest :: (Eq a) => (b,[a]) -> (b,[a]) -> Ordering
  goodTest x y = if testsAsWellAs (snd x) (snd y) then LT else GT

  shorterTree :: (Tree,[a]) -> (Tree,[a]) -> Ordering
  shorterTree x y = length (collapse $ fst x) `compare` length (collapse $ fst y)


isSubtree :: Tree -> Tree -> Bool
isSubtree t1 t2 = t1 `elem` args t2


