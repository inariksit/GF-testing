module FTree where

import qualified Data.Set as S
import Data.List( group, sort, insertBy )

{-
newtype FTree a b = F [(S.Set a,b)]

nil :: FTree a b
nil = F []

singleton :: Ord a => [a] -> b -> FTree a b
singleton xs y = F [(S.fromList xs,y)]

table :: Ord a => FTree a b -> [([a],b)]
table (F t) = [ (S.toList s,b) | (s,b) <- t ]

merge :: Ord a => (b->b->b) -> FTree a b -> FTree a b -> FTree a b
merge op (F p) (F q) = F (foldr comb q p)
 where
  comb (s,b) p
    | any ((s `S.isSubsetOf`) . fst) p =
        [ if s == t then (t,b `op` b') else (t,b') | (t,b') <- p ]
    | otherwise                        =
        insertBy cmp (s,b) [ tb | tb@(t,_) <- p, not (t `S.isSubsetOf` s) ]
   where
    (s,_) `cmp` (t,_) = s `compare` t

-- this is a bit of a hack... :-P
instance Eq a => Eq (FTree a b) where
  F p == F q = map fst p == map fst q
-}

-- represents a partial function : Set a -> b
data FTree a b
  = Choice [(a,FTree a b)]
  | Done b
 deriving ( Show )

nil :: FTree a b
nil = Choice []

instance Functor (FTree a) where
  f `fmap` Choice xts = Choice [ (x, f `fmap` t) | (x,t) <- xts ]
  f `fmap` Done y     = Done (f y)

singleton :: Ord a => [a] -> b -> FTree a b
singleton xs y =
    foldr (\x t -> Choice [(x,t)]) (Done y)
  . map head
  . group
  . sort
  $ xs

table :: FTree a b -> [([a],b)]
table (Choice xts) = [ (x:xs,y) | (x,t) <- xts, (xs,y) <- table t ]
table (Done y)     = [ ([],y) ]

merge :: Ord a => (b->b->b) -> FTree a b -> FTree a b -> FTree a b
merge op (Choice []) q           = q
merge op p           (Choice []) = p

merge op (Done y) (Done z) = Done (y `op` z)
merge op (Done _) q        = q
merge op p        (Done _) = p

merge op (Choice xts) (Choice yts) = Choice (go xts yts)
 where
  go [] yts = yts
  go xts [] = xts
  go xts@(xt@(x,t):xts') yts@(yt@(y,u):yts') =
    case x `compare` y of
      LT -> xt : go xts' [ (y,u `diff` t) | (y,u) <- yts ]
      EQ -> (x, merge op t u) : go xts' yts'
      GT -> yt : go [ (x,t `diff` u) | (x,t) <- xts ] yts'

diff :: Ord a => FTree a b -> FTree a b -> FTree a b
diff p (Choice []) = p
diff p (Done _)    = p
diff (Choice xts) (Choice yts) = Choice (go xts yts)
 where
  go [] yts = []
  go xts [] = xts
  go xts@(xt@(x,t):xts') yts@(yt@(y,u):yts') =
    case x `compare` y of
      LT -> xt : go xts' yts
      EQ -> (x, t `diff` u) : go xts' yts'
      GT -> go xts yts'

-- this is a bit of a hack... :-P
instance Eq a => Eq (FTree a b) where
  Choice xts == Choice yts = xts == yts
  Done _     == Done _     = True


