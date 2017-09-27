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
  let nextLevels = take 2 $ map (nextLevel gr) funsWithArgs
  mapM_ pr $ (zip3 funsWithArgs nextLevels (map (map (linearize gr)) nextLevels))

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
levels :: Grammar -> Symbol -> [(Int,[Tree])]
levels gr s = take 5 $ iterate go firstLevel
 where
  firstLevel = (1, nextLevel gr s)
  go (n,xs) = (n+1, concatMap (nextLevel gr . top) xs)


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

  -- gives smallest trees that use the function we are testing
  repTree x | x == resCat = App origF <$> sequence (representativeTrees gr <$> argCats)
            | otherwise   = trace ("defaultTrees: " ++ show (defaultTrees gr x)) $ representativeTrees gr x

  -- 1) Get all functions in the grammar that use the result category
  -- 2) Get smallest argument trees to the functions; apply the functions to them 
  trees = [ App f <$> sequence argTrees 
            | f@(Symbol _ (args, _)) <- symbols gr
            , resCat `elem` args
            , f /= origF -- don't apply another AdjCN if the original is AdjCN
            , let argTrees = map repTree args ]
--            , let argTrees = map smTree args ]
--            , let argTrees = map defTree args ]


--------------------------------------------------------------------------------

hasArg :: Symbol -> Bool
hasArg s = case s of
  Symbol _ ([], _) -> False
  _                -> True

-- First tree of each size
representativeTrees :: Grammar -> Cat -> [Tree]
representativeTrees gr c = trace ("repTrees: " ++ show (filter norepeat $ featIth gr c <$> take 4 cards <*> [0]))
 $ filter norepeat $ featIth gr c <$> take 4 cards <*> [0]
 where
  cards = [ size | size <- [1..100]
                 , let amount = featCard gr c size
                 , amount > 0 ]
  norepeat :: Tree -> Bool
  norepeat t = let tops = ["TODO"] in nub tops == tops


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
