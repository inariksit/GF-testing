module Lib
    ( someFunc
    ) where

import Grammar
import Paths_GF_testing
import Control.Applicative
import Data.List
import Debug.Trace

someFunc :: IO ()
someFunc = do
  grName <- getDataFileName "MiniTest.pgf" 
  gr <- readGrammar grName  
  let funsWithArgs = filter hasArg $ symbols gr
  
  let nextLevels = take 3 $ map (nextLevel gr) funsWithArgs


  mapM_ pr $ (zip3 funsWithArgs nextLevels (map (map (linearize gr)) nextLevels))
--  mapM_ print $ take 2 (zip3 funsWithArgs nextLevels (map (map (tabularLin gr)) nextLevels))

  --let upToFive = levels gr (funsWithArgs !! 6)
  --mapM_ (print . \(i,trs) -> (i, map (linearize gr) trs)) $ upToFive

 where 
  pr (funName,trees,lins) =
    do print funName
       pr' (trees,lins)

  pr' ([],[]) = putStrLn ""
  pr' (x:xs,y:ys) = do print x 
                       print y 
                       putStrLn ""
                       pr' (xs,ys)


--TODO: this doesn't work because the new calls to nextLevel
--don't keep the original symbol in mind.
--levels :: Grammar -> Symbol -> [(Int,[Tree])]
--levels gr s = take 5 $ iterate go firstLevel
-- where
--  firstLevel = (1, nextLevel gr s)
--  go (n,xs) = (n+1, concatMap (nextLevel gr . top) xs)


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
  trees = [ concat interestingTrees
--  trees = [ filter norepeat $ App f <$> sequence argTrees -- 
            | f@(Symbol _ (args, _)) <- symbols gr
            , resCat `elem` args
            , f /= origF -- don't apply another AdjCN if the original is AdjCN

            , let argTrees = repTree 3 <$> args :: [[Tree]]

--            , let bestTrees = foldl1 (betterExample gr) <$> argTrees
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
--            , let argTrees = map smTree args ]
--            , let argTrees = map defTree args ]
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
collapse (App tp as) = tp : (concat $ map collapse as)

-- All trees of the smallest size
smallestTrees :: Grammar -> Cat -> [Tree]
smallestTrees gr c = map (featIth gr c size) [0..amount-1]
 where
  (size,amount) = head $ [ (size,amount) | size <- [1..100]
                                         , let amount = featCard gr c size
                                         , amount > 0 ]

-- Just a dummy function for getting a quick input of nextLevel
defaultTrees :: Grammar -> Cat -> [Tree]
defaultTrees gr c = [featIth gr c nonEmptyCard 0]

 where
  nonEmptyCard = head $ [ card | card <- [1..100]
                               , featCard gr c card > 0 ]


--------------------------------------------------------------------------------

hasArg :: Symbol -> Bool
hasArg s = case s of
  Symbol _ ([], _) -> False
  _                -> True

betterExample :: Grammar -> Tree -> Tree -> Tree
betterExample gr t1 t2 = 
  if length tabLinT2 >= length tabLinT1 then t2 else t1
 where
  tabLinT1 = nub $ map snd $ tabularLin gr t1 
  tabLinT2 = nub $ map snd $ tabularLin gr t2


-- Tells whether the tree to be tested, e.g. VP "drink beer", 
-- changes when combined with a given argument
-- Not yet functional, just testing outputs and figuring out what to do
changesInLin :: Grammar -> Symbol -> [Tree] -> Bool
changesInLin gr predVP args@[you,drinkBeer] =
    trace ("=== changesInLin:\n" ++ show youLin ++ "\n" ++ show drinkBeerLin ++ "\n" ++ show youDrinkBeerLin ++ "===") $ 
    True
 where
  drinkBeerLin = tabularLin gr drinkBeer :: [(String,String)]
  youLin = tabularLin gr you :: [(String,String)]
  youDrinkBeer = App predVP args :: Tree
  youDrinkBeerLin = tabularLin gr youDrinkBeer
changesInLin gr predVP args = trace ("changesInLin: hit _ with " ++ show (length args, args)) $ True


isSubtree :: Tree -> Tree -> Bool
isSubtree t1 t2 = t1 `elem` args t2


--TODO:
{-
Testing the function (Symbol) UsePron:

defaultTrees: [break_V2]
(ComplV2(break_V2,UsePron(i_Pron)),"break me")

defaultTrees: [UseV(travel_V)]
(PredVP(UsePron(i_Pron),UseV(travel_V)),"I do not travel")

defaultTrees: [in_Prep]
(PrepNP(in_Prep,UsePron(i_Pron)),"in me")

(UttNP(UsePron(i_Pron)),"me")


Testing the function ComplV2
* The testable trees are: 
x = ComplV2(break_V2,UsePN(paris_PN))
y = ComplV2(break_V2,MassNP(UseN(animal_N))
z = ComplV2(break_V2,DetCN(aPl_Det,UseN(animal_N)))

defaultTrees for AdvVP: [already_Adv]
(AdvVP(x,already_Adv),"break Paris already")
(AdvVP(y,already_Adv),"break animal already")
(AdvVP(z,already_Adv),"break animals already")

Here we don't have to show the user all 3 forms: the linearisation of x, y and z
is always the same.

defaultTrees for PredVP: [UsePN(paris_PN)]
(PredVP(UsePN(paris_PN),x)
(PredVP(UsePN(paris_PN),y)
(PredVP(UsePN(paris_PN),z)

Here we would want more defaultTrees: "I break Paris/animals" vs. "he breaks Paris/animals":
the trees x, y and z have different linearization depending on the first arg to PredVP.
--}
