module Lib
    ( assertLin
    , hasArg
    , lookupSymbol
    , bestExamples
    , treesByCCat
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
  pr (t:ts,l:ls) = do putStrLn (show t ++ " : " ++ concatMap (show.snd) (concrTypes $ top t))
                      print l 
                      putStrLn ""
                      pr (ts,ls)

lookupSymbol :: Grammar -> String -> Symbol
lookupSymbol gr str = 
  head funsWithArgs `fromMaybe`
       lookup (mkName str) (symb2table <$> symbols gr)
 where
  symb2table s@(Symbol nm _ _) = (nm,s)
  funsWithArgs = filter hasArg $ symbols gr

--------------------------------------------------------------------------------
-- Method 1, without FEAT. 
-- Slow but gives at least some trees for more complicated grammars.

--treesByCCat :: Grammar -> [(ConcrCat,Tree)]
treesByCCat gr = bestTrees $ foldl findTrees zeroArgTrees (replicate 10 gr)
  --use scanl to find out how many trees you get at each step

 where

  findTrees :: [(ConcrCat,Tree)] -> Grammar -> [(ConcrCat,Tree)]
  findTrees nArgTrees gr
    = nArgTrees ++ [ ( resCcat, App s bestArgs ) 
         | s@(Symbol f t ccats) <- symbols gr
         , (argCcats,resCcat) <- ccats
         , let lookup' ccat = [ (cat,tree) | (cat,tree) <- nArgTrees, cat==ccat ]
         , let argsOfCcat = map lookup' argCcats
         , all (not.null) argsOfCcat -- if >1 of the argCcats is not in nArgTrees, break
--         , let bestArgs = map (snd.head) argsOfCcat ]
         , let bestArgs = map (snd.bestTree) argsOfCcat ]

  concrFuns :: Grammar -> [(ConcrCat,(Name,[ConcrCat]))]
  concrFuns gr = [ (resCcat,(fun,argCcats))  
                  | (Symbol fun _args ccats) <- symbols gr
                  , (argCcats,resCcat) <- ccats ]

  zeroArgTrees = bestTrees
    [ (ccat, App symbol []) 
      | ( ccat@(CC (Just cat) _)     -- Assuming no coercions -- TODO is this correct?
        , (fun,[]) ) <- concrFuns gr -- Match only trees with 0 arguments
      , let symbol = Symbol fun ([],cat) [([],ccat)] ]

  bestTrees :: [(ConcrCat,Tree)] -> [(ConcrCat,Tree)]
  bestTrees = map bestTree . groupBy (\(a,_) (b,_) -> a==b) . sort

  bestTree :: [(ConcrCat,Tree)] -> (ConcrCat,Tree)
  bestTree ccats_trees = (head ccats, head $ bestExamples gr trees)
   where (ccats,trees) = unzip ccats_trees


--------------------------------------------------------------------------------
-- Method 2, with FEAT. Doesn't run at all for e.g. Basque or Estonian.


nextLevel :: Grammar -> Symbol -> [[Tree]] -- a list for each concrete category
nextLevel gr origF = concat trees

 where
--  (argCats,resCat) = typ origF -- e.g. ([Adj,CN],CN) for AdjCN
  (argCats,resCat) = head $ concrTypes origF -- e.g. ([Adj,CN],CN) for AdjCN


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
--            , resCat `elem` args



--------------------------------------------------------------------------------

norepeat :: Tree -> Bool
norepeat t = let tops = collapse t in nub tops == tops

collapse :: Tree -> [Symbol]
collapse (App tp as) = tp : concatMap collapse as


-- Just a dummy function for getting a quick input of nextLevel
defaultTrees :: Grammar -> ConcrCat -> [Tree]
defaultTrees gr c = case nonEmptySizes of
  [] -> []
  cs -> bestExamples gr
          [ featIth gr c size card 
            | (size,cd) <- cs
            , card <- [0..cd-1] ]
 where
  nonEmptySizes = [ (size,card) | size <- [1..5]
                                , let card = featCard gr c size 
                                , card > 0 ]


--------------------------------------------------------------------------------

hasArg :: Symbol -> Bool
hasArg s = case s of
  Symbol _ ([], _) _ -> False
  _                  -> True


testsAsWellAs :: (Eq a, Eq b) => [a] -> [b] -> Bool
xs `testsAsWellAs` ys = go (xs `zip` ys)
 where
  go [] =
    True
    
  go ((x,y):xys) =
    and [ y' == y | (x',y') <- xys, x == x' ] &&
    go [ xy | xy@(x',_) <- xys, x /= x' ]


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


