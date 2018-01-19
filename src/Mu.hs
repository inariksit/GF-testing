module Mu where

import Data.Map( Map, (!) )
import qualified Data.Map as M
import Data.Set( Set )
import qualified Data.Set as S
import Graph

--------------------------------------------------------------------------------

-- naive implementation of fixpoint computation
mu0 :: (Ord x, Eq a) => a -> [(x, [x], [a] -> a)] -> [x] -> [a]
mu0 bot defs zs = [ done!z | z <- zs ]
 where
  xs    = [ x | (x, _, _) <- defs ]
  done  = iter [ bot | _ <- xs ]

  iter as
    | as == as' = tab
    | otherwise = iter as'
   where
    tab = M.fromList (xs `zip` as)
    as' = [ f [ tab!y | y <- ys ]
          | (_,(_, ys, f)) <- as `zip` defs
          ]

--------------------------------------------------------------------------------

-- scc-based implementation of fixpoint computation
mu :: (Ord x, Eq a) => a -> [(x, [x], [a] -> a)] -> [x] -> [a]
mu bot defs zs = [ vtab?z | z <- zs ]
 where
  ftab  = M.fromList [ (x,f)  | (x,_,f) <- defs ]
  graph = reach (M.fromList [ (x,xs) | (x,xs,_) <- defs ]) zs
  vtab  = foldl compute M.empty (scc graph)

  compute vtab t = fix vtab (map (vtab ?) xs)
   where
    xs = S.toList (backs t)

    fix vtab as
      | as' == as = vtab'
      | otherwise = fix vtab' as'
     where
      (_,vtab') = eval t vtab
      as'       = map (vtab' ?) xs

  eval (Cut x)     vtab = (vtab?x, vtab)
  eval (Node x ts) vtab = (a, M.insert x a vtab')
   where
    (as, vtab') = evalList ts vtab
    a           = (ftab!x) as

  evalList []     vtab = ([], vtab)
  evalList (t:ts) vtab = (a:as, vtab'')
   where
    (a, vtab')  = eval t vtab
    (as,vtab'') = evalList ts vtab'

  vtab ? x = case M.lookup x vtab of
               Nothing -> bot
               Just a  -> a

--------------------------------------------------------------------------------

