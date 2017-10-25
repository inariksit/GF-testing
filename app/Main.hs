module Main where

import GrammarC
import Lib
import Paths_GF_testing
import System.Environment

import Data.List
import Data.Maybe

main :: IO ()
main = do
  grName <- getDataFileName "TestLang.pgf" 

--  grName <- getDataFileName "Phrasebook.pgf" 
  gr <- readGrammar grName  
  mapM_ (print.(\(x,y,z,_) -> (x,y,z))) $ concrCats gr

  mapM_ print (sort $ swapLinFuns gr)
--  mapM_ print (productions gr `map` [100..120])
  putStrLn "---"
  let zeroAgrTrees = same2 
       [ (ccat, App symbol []) 
        | (ccat,(fun,[])) <- swapLinFuns gr
        , let cat = getCat ccat
        , let symbol = Symbol fun ([],cat) ]

  --let b = findTrees gr a
  --let c = findTrees gr (a++b)
  --let d = findTrees gr (a++b++c)
  --let e = findTrees gr (a++b++c++d)
  --mapM_ (print.length) [a,b,c,d,e]

  --TODO: make it actually behave the same way as the manual a++b++c… example!
  --this uses only b to get c, only c to get d etc.
  let trees = scanr findTrees zeroAgrTrees (take 10 $ repeat gr)
  print $ (map length trees, map (length.nub) trees)
  mapM_ (\xs -> mapM_ print (same2 xs) >> putStrLn "---") trees
--  mapM_ print (sort $ nub $ concat trees)


  putStrLn "END"

  --args <- getArgs
  --case args of 
  --  ("all":_) -> mapM_ (assertLin gr) (filter hasArg $ symbols gr)
  --  (funNm:_) -> assertLin gr (lookupSymbol gr funNm)

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

  swapLinFuns gr = [ (cc,(fun,args))  | (fun,args,cc) <- linFunctions gr ]

  findTrees :: Grammar -> [(ConcrCat,Tree)] -> [(ConcrCat,Tree)]
  findTrees gr nArgTrees 
    = [ ( ccat, App symbol foundTrees ) 
         | (ccat,(fun,args)) <- swapLinFuns gr
         , let symbol = Symbol fun (map getCat args,getCat ccat)
         , let foundTrees = findFuns args nArgTrees :: [Tree]
         , not (null foundTrees) 
      ] 
