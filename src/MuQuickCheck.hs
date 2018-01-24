module MuQuickCheck where

import Data.Map( Map, (!) )
import qualified Data.Map as M
import Data.Set( Set )
import qualified Data.Set as S

import Test.QuickCheck
import qualified Data.List as L
import Data.List ((\\))

import Mu

--------------------------------------------------------------------------------

data Problem = P [ (Char, [Char], Mono) ] [ Char ] deriving ( Eq, Show )

data Mono
  = Arg Int
  | Union Mono Mono
  | Inter Mono Mono
  | Map [(Int,[Int])] [Int] Mono
  | This [Int]
 deriving ( Eq, Ord, Show )

instance Arbitrary Mono where
  arbitrary = sized (arbMono 0 5)

  shrink (Arg i) = [ Arg j | j <- [0..i-1] ]
  shrink (Union p q) = [ p, q ] ++ [ Union p' q | p' <- shrink p ] ++ [ Union p q' | q' <- shrink q ]
  shrink (Inter p q) = [ p, q ] ++ [ Inter p' q | p' <- shrink p ] ++ [ Inter p q' | q' <- shrink q ]
  shrink (Map f d p) = [ p, This d ]
                    ++ [ Map f' d p | f' <- shrink f ]
                    ++ [ Map f d' p | d' <- shrink d ]
                    ++ [ Map f d p' | p' <- shrink p ]
  shrink (This xs)   = [ This (L.nub (L.sort xs')) | xs' <- shrink xs ]

eval :: Mono -> [[Int]] -> [Int]
eval (Arg _) []      = []
eval (Arg 0) (xs:_)  = xs
eval (Arg i) (_:xss) = eval (Arg (i-1)) xss
eval (Union p q) xss = L.nub (L.sort (eval p xss `L.union` eval q xss))
eval (Inter p q) xss = eval p xss `L.intersect` eval q xss
eval (Map f d p) xss = L.nub (L.sort (concatMap (h f d) (eval p xss)))
 where
  h []        d x             = d
  h ((y,a):f) d x | x == y    = a
                  | otherwise = h f d x
eval (This xs) xss   = xs

arbMono :: Int -> Int -> Int -> Gen Mono
arbMono k c n =
  frequency
  [ (2, Arg   <$> choose (0,k-1))
  , (1, This  <$> arbSet c)
  , (n, Union <$> arbMono k c n2 <*> arbMono k c n2)
  -- , (n, Inter <$> arbMono k c n2 <*> arbMono k c n2)
  , (n, Map   <$> arbFun c <*> arbSet c <*> arbMono k c n2)
  ]
 where
  n2 = n `div` 2

  arbSet c =
    do xs <- listOf (choose (1,c))
       return (L.nub (L.sort xs))

  arbFun c =
    sequence
    [ do ys <- arbSet c
         return (x,ys)
    | x <- [1..c]
    ]

instance Arbitrary Problem where
  arbitrary =
    do v <- choose (1,26)
       c <- choose (1,10)
       arbProblem v c
  
  shrink (P defs xs) =
    [ P defs (xs \\ [x])
    | x <- xs
    ] ++
    [ P [ (y,filter (x/=) ys,del (count x ys) f) | (y,ys,f) <- defs, y /= x ] (filter (x/=) xs)
    | (x, _, _) <- defs
    ] ++
    [ P (take i defs ++ [(y,take j ys ++ drop (j+1) ys, del j f)] ++ drop (i+1) defs) xs
    | ((y,ys,f),i) <- defs `zip` [0..]
    , j <- [0..length ys-1]
    ] ++
    [ P (take i defs ++ [(y,ys,f')] ++ drop (i+1) defs) xs
    | ((y,ys,f),i) <- defs `zip` [0..]
    , f' <- shrink f
    ]
   where
    count x (y:ys)
      | x == y    = 0
      | otherwise = 1 + count x ys
    count x []    = 1
    
    del i (Arg j)
      | j < i         = Arg j
      | j == i        = This []
      | otherwise     = Arg (j-1)
    del i (Union p q) = Union (del i p) (del i q)
    del i (Inter p q) = Inter (del i p) (del i q)
    del i (Map f d p) = Map f d (del i p)
    del i p           = p

arbProblem v c =
  do ds <- sequence
            [ do ys <- args
                 f  <- sized (arbMono (length ys) c)
                 return (x,ys,f)
            | x <- xs
            ]
     xs <- args
     return (P ds xs)
 where
  xs   = take v ['a'..]
  args = listOf (elements xs)

defs :: Problem -> [(Char,[Char],[[Int]]-> [Int])]
defs (P ents _) =
  [ (x,xs,eval f)
  | (x,xs,f) <- ents
  ]

defs' :: Problem -> [(Char,[Char],[([Int],[Int])]-> [Int])]
defs' (P ents _) =
  [ (x,xs,eval f . map fst)
  | (x,xs,f) <- ents
  ]

prop_Mu :: Problem -> Property
prop_Mu p@(P _ xs) =
  let ds  = defs p
      gr  = M.fromList [ (x,xs) | (x,xs,_) <- ds ]
      new = mu  [] ds xs
      old = mu0 [] ds xs
      
      report =
        do putStrLn ("mu0: " ++ show old)
           putStrLn ("mu:  " ++ show new)
           
   in within 5000000 $ whenFail report (new == old)

prop_MuDiff :: Problem -> Property
prop_MuDiff p@(P _ xs) =
  let ds  = defs p
      gr  = M.fromList [ (x,xs) | (x,xs,_) <- ds ]
      new = muDiff [] [] null id difference union' (defs' p) xs
      old = mu0 [] ds xs
      
      report =
        do putStrLn ("mu0: " ++ show old)
           putStrLn ("mu:  " ++ show new)
           
   in within 5000000 $ whenFail report (new == old)

union'     xs ys = L.nub $ L.sort $ L.union xs ys
difference xs ys = L.nub $ L.sort $ filter (`notElem` ys) xs

