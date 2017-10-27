module Main where

import GrammarC
import Lib
import Paths_GF_testing
import System.Environment

import Data.List
import Data.Maybe

--import qualified PGF2

main :: IO ()
main = do
  grName <- getDataFileName "TestLang.pgf" 

--  grName <- getDataFileName "Phrasebook.pgf" 
  gr <- readGrammar grName  
  mapM_ (print.(\(x,y,z,_) -> (x,y,z))) $ concrCats gr

--  mapM_ print (sort $ swapLinFuns gr) -- Not a good idea for languages with 10k+ categories >_>
--  mapM_ print (productions gr `map` [100..120])
  putStrLn "---"
  let zeroArgTrees = same2 
       [ (ccat, App symbol []) 
        | (ccat,(fun,[])) <- swapLinFuns gr
        , let cat = getCat ccat
        , let symbol = Symbol fun ([],cat) [([],ccat)] ] --TODO 

  --use scanl to find out how many trees you get at each step
  let trees = nub $ foldl findTrees zeroArgTrees (take 10 $ repeat gr)
  --mapM_ print $ sort trees
  --print $ length $ swapLinFuns gr
  --print $ length trees
  --print $ coercions gr


  --putStrLn "END"

  args <- getArgs
  case args of 
    ("all":_) -> mapM_ (assertLin gr) (filter hasArg $ symbols gr)
    (funNm:_) -> assertLin gr (lookupSymbol gr funNm)

 where
  same2 :: (Eq a) => [(a,b)] -> [(a,b)] 
  same2 = nubBy (\(a,_) (b,_) -> a==b) 

  same3 :: (Eq a) => [(a,b,c)] -> [(a,b,c)] 
  same3 = nubBy (\(a,_,_) (b,_,_) -> a==b)

  findFuns :: (Eq a) => [a] -> [(a,b)] -> [b]
  findFuns args examples = 
   let funs = map (`lookup` examples) args 
   in if all isJust funs then map fromJust funs else []

  mkTree :: Symbol -> Tree
  mkTree s = App s []

  getCat :: ConcrCat -> Cat
  getCat (CC (Just c) _) = c
  getCat (CC Nothing _) = "TODO" --TODO: coercions

  swapLinFuns :: Grammar -> [(ConcrCat,(Name,[ConcrCat]))]
  swapLinFuns gr = [ (resCcat,(fun,argCcats))  
                    | (Symbol fun _args ccats) <- symbols gr
                    , (argCcats,resCcat) <- ccats ]

  findTrees :: [(ConcrCat,Tree)] -> Grammar -> [(ConcrCat,Tree)]
  findTrees nArgTrees gr
    = nArgTrees ++ [ ( resCcat, App s foundTrees ) 
         | s@(Symbol f t ccats) <- symbols gr
         , (argCcats,resCcat) <- ccats
         , let foundTrees = findFuns argCcats nArgTrees :: [Tree]
         , not (null foundTrees) 
      ] 
