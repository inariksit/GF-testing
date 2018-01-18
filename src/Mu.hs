module Mu where

import Data.Map( Map, (!) )
import qualified Data.Map as M
import Data.Set( Set )
import qualified Data.Set as S

--------------------------------------------------------------------------------

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

{-
simulate :: (Ord x, Eq a) => Map x ([x], [a] -> a)
                          -> (Map x a, [(x,a)]) -> (Map x a, [x])
simulate defs (state, upds) = (state', changed')
 where
  used = M.fromListWith (++)
         [ (x,[y])
         | (y,(xs,_)) <- M.toList defs
         , x <- xs
         ]

  levels = go 0 M.empty x0s xks
   where
    xns = [ (x,length vs) | (x,(vs,_)) <- M.toList defs ]
    x0s = [ x | (x,0) <- xns ]
    xks = M.fromList [ xn | xn@(_,n) <- xns, n > 0 ]

    go i seen []     _   = seen
    go i seen (x:xs) xks = i' `seq` go i' (M.insert x i seen) (ys++xs) xks'
     where
      i' = i+1
      (ys,xks') = process (used ! x) [] xks

      process []     zs xks = (zs,xks)
      process (y:ys) zs xks
        | k == 0            = process ys (y:zs) (M.delete y xks)
        | otherwise         = process ys zs (M.insert y (k-1) xks)
       where
        k = xks ! y

  step state upds = go state [] (M.fromList [ (levels!x,xa) | xa@(x,_) <- upds ])
   where
    go state changed queue =
      case M.miView queue of
        Nothing             -> (state,changed)
        Just ((x,a),queue') -> go 
-}

--------------------------------------------------------------------------------

mu :: (Ord x, Eq a) => a -> [(x, [x], [a] -> a)] -> [x] -> [a]
mu bot defs zs = compute bot ftab (dft graph zs)
 where
  ftab  = M.fromList [ (x,f)  | (x,_, f) <- defs ]
  graph = M.fromList [ (x,xs) | (x,xs,_) <- defs ]

-- depth-first trees

data DFTree a
  = Node a [DFTree a]
  | Up   a
  | Side a
 deriving ( Eq, Ord, Show )

dft :: Ord x => Map x [x] -> [x] -> [DFTree x]
dft graph starts = go S.empty [ (S.empty,x) | x <- starts ]
 where
  go seen []            = []
  go seen ((ups,x):xs)
    | x `S.member` ups  = Up x   : go seen xs
    | x `S.member` seen = Side x : go seen xs
    | otherwise         = Node x (take n ts) : drop n ts
   where
    seen' = S.insert x seen
    ups'  = S.insert x ups
    ys    = graph!x
    n     = length ys
    ts    = go seen' ([(ups',y) | y <- ys] ++ xs)

-- a monad for evaluation

newtype M x b a = M (b -> Map x b -> (a, Map x b))

instance Functor (M x b) where
  f `fmap` M h = M (\b s -> let (x,s') = h b s in (f x, s'))

instance Applicative (M x b) where
  pure x        = M (\b s -> (x,s))
  M mf <*> M mx = M (\b s -> let (f,s') = mf b s; (x,s'') = mx b s' in (f x, s''))

instance Monad (M x b) where
  return x   = M (\b s -> (x,s))
  M mx >>= k = M (\b s -> let (x,s') = mx b s; M h = k x in h b s')

get :: Ord x => x -> M x a a
get x = M (\b s -> case M.lookup x s of
                     Just a -> (a, s)
                     Nothing -> (b, s))

(=:) :: Ord x => x -> a -> M x a ()
x =: a = M (\b s -> ((), M.insert x a s))

run :: M x b a -> b -> a
run (M h) b = let (x, _) = h b M.empty in x

-- computing over a depth-first tree

data C x b a
  = Done a
  | Open (Set x) (Set x) (M x b a)

compute :: (Ord x, Eq a) => a -> Map x ([a]->a) -> [DFTree x] -> [a]
compute bot ftab ts = run (sequence [ top t | t <- ts ]) bot
 where
  top t =
    do c <- go t
       case c of
         Done a     -> return a
         Open _ _ m -> m
 
  go (Side x) =
    do return (Open S.empty S.empty (get x))

  go (Up x) =
    do return (Open (S.singleton x) S.empty (get x))

  go (Node x ts) =
    do cs <- sequence [ go t | t <- ts ]
       c  <- calc x cs
       case c of
         Open wait args m | x `S.member` wait ->
           if S.null wait' then
             let xs = S.toList args'
             
                 fix as =
                   do a   <- m
                      as' <- sequence [ get x | x <- xs ]
                      if as' == as
                        then return (Done a)
                        else fix as'
              
              in sequence [ get x | x <- xs ] >>= fix
           else
             do return (Open wait' args' m)
          where
           wait' = S.delete x wait
           args' = S.insert x args
         
         _ ->
           do return c
             
  calc x cs =
    case foldr (-:) (Done []) cs of
      Done as ->
        do let a = f as
           x =: a
           return (Done a)
     
      Open wait args m ->
        do return (Open wait args (do as <- m
                                      let a = f as
                                      x =: a
                                      return a))
   where
    f = ftab!x
 
    Done x       -: Done xs      = Done (x:xs)
    Done x       -: Open ws zs n = Open ws zs ((x:)  `fmap` n)
    Open vs ys m -: Done xs      = Open vs ys ((:xs) `fmap` m)
    Open vs ys m -: Open ws zs n =
      Open (vs `S.union` ws) (ys `S.union` zs) ((:) <$> m <*> n)

--------------------------------------------------------------------------------

