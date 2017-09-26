module Lib
    ( someFunc
    ) where

import Grammar
import Paths_GF_testing
import Control.Applicative
import Debug.Trace

someFunc :: IO ()
someFunc = do
  grName <- getDataFileName "MiniTest.pgf" 
  gr <- readGrammar grName  
  let funsWithArgs = filter hasArg $ symbols gr
  let nextLevels = map (nextLevel gr) funsWithArgs
  mapM_ pr $ (zip3 funsWithArgs nextLevels (map (map $ linearize gr) nextLevels))

  --let upToFive = levels gr (funsWithArgs !! 6)
  --mapM_ (print . \(i,trs) -> (i, map (linearize gr) trs)) $ upToFive


--TODO: this doesn't work because the new calls to nextLevel
--don't keep the original symbol in mind.
levels :: Grammar -> Symbol -> [(Int,[Tree])]
levels gr s = take 5 $ iterate go firstLevel
 where
  firstLevel = (1, nextLevel gr s)
  go (n,xs) = (n+1, concatMap (nextLevel gr . top) xs)


nextLevel :: Grammar -> Symbol -> [Tree]
nextLevel gr origS = concat trees

 where
  (argCats,resCat) = typ origS -- e.g. ([Adj,CN],CN) for AdjCN

  -- gives default tree that uses the function we are testing
  defTree x | x == resCat = App origS (defaultTree gr <$> argCats)
            | otherwise   = defaultTree gr x


  -- All functions in the grammar that use the result category
  funs = [ s | s@(Symbol _ (args, _)) <- symbols gr
             , resCat `elem` args
             , s /= origS ]  :: [Symbol]

  -- Apply the previously found functions, get trees.
  trees = [ App f <$> sequence dts | f <- funs 
                      , let (args,_) = typ f
                      , let dts = map smTree args ]
--                      , let dts = [map defTree args] ]

  -- gives default tree that uses the function we are testing
  smTree :: Cat -> [Tree]
  smTree x | x == resCat = App origS <$> sequence (smallestTrees gr <$> argCats)
           | otherwise   = smallestTrees gr x

--------------------------------------------------------------------------------
pr (funName,trees,lins) =
  do print funName
     mapM_ print (zip trees lins)


hasArg :: Symbol -> Bool
hasArg s = case s of
  Symbol _ ([], _) -> False
  _                -> True

smallestTrees :: Grammar -> Cat -> [Tree]
smallestTrees gr c = map (featIth gr c size) [0..amount-1]
 where
  (size,amount) = head $ [ (size,amount) | size <- [1..100]
                                         , let amount = featCard gr c size
                                         , amount > 0 ]

-- Just a dummy function for getting a quick input of nextLevel
defaultTree :: Grammar -> Cat -> Tree
defaultTree gr c = featIth gr c nonEmptyCard 0

 where
  nonEmptyCard = head $ [ card | card <- [1..100]
                               , featCard gr c card > 0 ]

