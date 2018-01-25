{-# LANGUAGE TemplateHaskell #-}
module FMapQuickCheck where

import FMap
import Data.List( nub, sort, insert )
import Test.QuickCheck
import Test.QuickCheck.All

--------------------------------------------------------------------------------
-- spec

spec :: FMap a b -> [([a],b)]
spec = toList

inv :: Ord a => FMap a b -> Bool
inv Nil            = True
inv (Answer _)     = True
inv (Ask x yes no) = go x yes && go x no && not (nil yes && nil no)
 where
  go q (Ask x yes no) = q < x && go x yes && go x no && not (nil yes && nil no)
  go q _              = True
  
  nil Nil            = True
  nil (Ask _ yes no) = nil yes && nil no
  nil _              = False

-- generators

newtype Str = Str [Int] deriving ( Eq, Show )

instance Arbitrary Str where
  arbitrary =
    do xs <- arbitrary
       return (Str (nub (sort (map abs xs))))

  shrink (Str xs) =
    nub [ Str (nub (sort xs')) | xs' <- shrink xs ]

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (FMap a b) where
  arbitrary =
    frequency
    [ (9, do x <- arbitrary
             sized (arb x))
    , (1, do z <- arbitrary
             return (Answer z))
    , (1, do return Nil)
    ]
   where
    arb q n =
      frequency
      [ (n, do x <- arbitrary `suchThat` (>q)
               (do l <- arb x n2
                   r <- arb x n2
                   return (Ask x l r)) `suchThat` inv)
      , (1, return Nil)
      , (1, do z <- arbitrary
               return (Answer z))
      ]
     where
      n2 = n `div` 2

  shrink t = [ t' | t' <- shr t, inv t' ]
   where
    shr (Ask x yes no) = [ yes, no ]
                      ++ [ Ask x yes' no | yes' <- shr yes ]
                      ++ [ Ask x yes no' | no'  <- shr no ]
    shr (Answer x)     = [ Nil ] ++ [ Answer x' | x' <- shrink x ]
    shr Nil            = []
                      

-- properties

prop_Inv fm =
  inv fm
 where
  types = fm :: FMap Int Int

prop_Covers (Str xs) fm =
  (fm `covers` xs) ==
    or [ all (`elem` ys) xs
       | (ys,_) <- spec fm
       ]
 where
  types = fm :: FMap Int Int

prop_Del (Str xs) fm =
  let fm' = del xs fm in
    whenFail (do putStrLn ("fm  = " ++ show fm  ++ "  ~ " ++ show (spec fm))
                 putStrLn ("fm' = " ++ show fm' ++ "  ~ " ++ show (spec fm')
                         ++ if inv fm' then "" else " (BAD)")) $
    inv fm' &&
    spec fm' == [ (ys,z) | (ys,z) <- spec fm, any (`notElem` xs) ys ]
 where
  types = fm :: FMap Int Int

prop_Add (Str xs, y) fm =
  not (fm `covers` xs) ==>
  let fm' = add xs y fm in
    whenFail (do putStrLn ("fm  = " ++ show fm  ++ "  ~ " ++ show (spec fm))
                 putStrLn ("fm' = " ++ show fm' ++ "  ~ " ++ show (spec fm') 
                         ++ if inv fm' then "" else " (BAD)")) $
                 
    inv fm' &&
    spec fm' ==
      insert (xs,y)
      [ (ys,z) | (ys,z) <- spec fm, any (`notElem` xs) ys ]
 where
  types = fm :: FMap Int Int

return []
testAll = $(quickCheckAll)
  
