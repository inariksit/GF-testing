module Lib
    ( assertLin
    , hasArg
    , lookupSymbol
    , bestExamples
    ) where

import GrammarC
import Paths_GF_testing
import Control.Applicative
import Data.List
import Data.Maybe
import Debug.Trace


assertLin :: Grammar -> Symbol -> IO ()
assertLin gr fun = do
  let treesUsingFun = nextLevel gr fun
  print fun
  pr (treesUsingFun, map (linearize gr) treesUsingFun)
 where
  pr ([],[]) = putStrLn ""
  pr (t:ts,l:ls) = do putStrLn (show t ++ " : " ++ snd (typ $ top t))
                      print l 
                      putStrLn ""
                      pr (ts,ls)

lookupSymbol :: Grammar -> String -> Symbol
lookupSymbol gr str = 
  head funsWithArgs `fromMaybe`
       lookup (mkName str) (symb2table <$> symbols gr)
 where
  symb2table s@(Symbol nm tp) = (nm,s)
  funsWithArgs = filter hasArg $ symbols gr


nextLevel :: Grammar -> Symbol -> [Tree]
nextLevel gr origF = concat trees

 where
  (argCats,resCat) = typ origF -- e.g. ([Adj,CN],CN) for AdjCN

  -- gives default tree that uses the function we are testing
  defTree x | x == resCat = App origF <$> sequence (defaultTrees gr <$> argCats)
            | otherwise   = defaultTrees gr x

  -- gives smallest trees that use the function we are testing
  smTree x | x == resCat = App origF <$> sequence (smallestTrees gr <$> argCats)
           | otherwise   = smallestTrees gr x

  -- first tree of every size up to ith size, that use the function we are testing
  repTree i x | x == resCat = App origF <$> sequence (representativeTrees i gr <$> argCats)
              | otherwise   = representativeTrees i gr x

  -- 1) Get all functions in the grammar that use the result category
  -- 2) Get smallest argument trees to the functions; apply the functions to them 
--  trees = [ concat interestingTrees
  trees = [ filter norepeat $ App f <$> sequence argTrees -- 
            | f@(Symbol _ (args, _)) <- symbols gr
            , resCat `elem` args
            , f /= origF -- don't apply another AdjCN if the original is AdjCN

--            , let argTrees = repTree 3 <$> args :: [[Tree]]
--            , let argTrees = map smTree args 
            , let argTrees = map defTree args 

--            , let bestTrees = ... argTres
--            , let allTrees = [App f bestTrees]

            , let resTrees = repTree 3 resCat --all representative trees of the interesting category
            , let allTrees = filter norepeat $ App f <$> sequence argTrees 

            -- This is just testing output, no significance whatsoever
            , let true = and $ changesInLin gr f <$> sequence argTrees
            -- TODO: check out bracketed strings ???

            , let interestingTrees = if not true then [] else
                                      [ treesUsingResTree 
                                       | resTree <- resTrees
                                       , let treesUsingResTree = filter (isSubtree resTree) allTrees
                                       , let rtLin = trace (linearize gr resTree) $ linearize gr resTree
                                        ]
            ]
--}

--------------------------------------------------------------------------------

representativeTrees :: Int -> Grammar -> Cat -> [Tree]
representativeTrees i gr cat = trace ("repTrees: " ++ show trees) $ concat trees
 where
  trees = [ [ tree | am <- [0..amount-1] 
                   , let tree = featIth gr cat size am
                   , norepeat tree ] 
            | (size,amount) <- take i sz_am ] -- Take i first sizes that exist

  -- Try all sizes from 1 to 100, keep ones that exist & how many there are
  sz_am = [ (size,amount) | size <- [1..100]
                          , let amount = featCard gr cat size 
                          , amount > 0 ]

norepeat :: Tree -> Bool
norepeat t = let tops = collapse t in nub tops == tops

collapse :: Tree -> [Symbol]
collapse (App tp as) = tp : concatMap collapse as

-- All trees of the smallest size
smallestTrees :: Grammar -> Cat -> [Tree]
smallestTrees gr c = map (featIth gr c size) [0..amount-1]
 where
  (size,amount) = head [ (size,amount) | size <- [1..100]
                                       , let amount = featCard gr c size
                                       , amount > 0 ]

-- Just a dummy function for getting a quick input of nextLevel
defaultTrees :: Grammar -> Cat -> [Tree]
defaultTrees gr c = case nonEmptyCards of
  []   -> []
  x:xs -> [featIth gr c x 0]

 where
  nonEmptyCards = [ card | card <- [1..100]
                        , featCard gr c card > 0 ]


--------------------------------------------------------------------------------

hasArg :: Symbol -> Bool
hasArg s = case s of
  Symbol _ ([], _) -> False
  _                -> True



bestExamples :: Grammar -> [Tree] -> [Tree]
bestExamples gr [] = []
bestExamples gr trees = 
  [ t | (t,tl) <- zip trees indexedTabLins :: [(Tree, [((String,String),Int)] )]
      , diff tl == maxDiff ]
 where
  tabLins = map (tabularLin gr) trees :: [[(String,String)]] -- one list for one tree

  indexedTabLins = [ go tl [] | tl <- tabLins ] :: [[((String,String),Int)]]

  diff tabLins = maximum (map snd tabLins)
  maxDiff = maximum (map diff indexedTabLins)

  go :: [(String,String)] -> [((String,String),Int)] -> [((String,String),Int)]
  go []           res = res -- :: [((String,String),Int)]
  go (tl:tabLins) []  = go tabLins [(tl,1)]
  go (tl:tabLins) res = 
    case lookup (snd tl) [ (s,i) | ((f,s),i) <- res ] of
      Just ind -> go tabLins $ (tl,ind):res
      Nothing -> let largestInd = maximum (map snd res)
                  in go tabLins $ (tl,largestInd+1):res


-- Tells whether the tree to be tested, e.g. VP "drink beer", 
-- changes when combined with a given argument
-- Not yet functional, just testing outputs and figuring out what to do
changesInLin :: Grammar -> Symbol -> [Tree] -> Bool
changesInLin gr predVP args@[you,drinkBeer] =
    --trace ("=== changesInLin:\n" ++ show youLin ++ "\n" 
    --  ++ show drinkBeerLin ++ "\n" 
    --  ++ show youDrinkBeerLin ++ "\n"
    --  ++ show onlyDrinkBeer ++ "===") $ 
    drinkBeerLin /= onlyDrinkBeer
 where
  lin = linearize -- tabularLin
  drinkBeerLin = lin gr drinkBeer 
  youLin = lin gr you 
  youDrinkBeer = App predVP args :: Tree
  youDrinkBeerLin = lin gr youDrinkBeer
  onlyDrinkBeer = unwords (words youDrinkBeerLin \\ words youLin)
changesInLin gr predVP args = True --trace ("changesInLin: hit _ with " ++ show (length args, args)) $ True


isSubtree :: Tree -> Tree -> Bool
isSubtree t1 t2 = t1 `elem` args t2

