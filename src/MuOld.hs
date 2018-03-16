module Mu where

import Data.Map( Map, (!) )
import qualified Data.Map as M
import Data.Set( Set )
import qualified Data.Set as S

import Test.QuickCheck
import qualified Data.List as L
import Data.List ((\\))

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
  | Up   a -- an arrow upwards in the same component
  | Side a -- an arrow sideways in the same component
  | Prev a -- an arrow sideways to another (previous) component
 deriving ( Eq, Ord, Show )

dft :: Ord x => Map x [x] -> [x] -> [DFTree x]
dft graph starts = go S.empty S.empty [ (S.empty,x) | x <- starts ]
 where
  go done seen []            = []
  go done seen ((ups,x):xs)
    | x `S.member` ups  = Up x   : go done seen xs
    | x `S.member` done = Prev x : go done seen xs
    | x `S.member` seen = Side x : go done seen xs
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
compute bot ftab ts = [ a | Done a <- run (sequence [ go t | t <- ts ]) bot ]
 where
  go (Prev x) =
    do a <- get x
       return (Done a)

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
  shrink (Map f d p) = [ p ]
                    ++ [ Map f' d p | f' <- shrink f ]
                    ++ [ Map f d' p | d' <- shrink d ]
                    ++ [ Map f d p' | p' <- shrink p ]
  shrink (This xs)   = [ This (L.nub (L.sort xs')) | xs' <- shrink xs ]

eval :: Mono -> [[Int]] -> [Int]
eval (Arg _) []      = [] 
eval (Arg 0) (xs:_)  = xs
eval (Arg i) (_:xss) = eval (Arg (i-1)) xss
eval (Union p q) xss = eval p xss `L.union` eval q xss
eval (Inter p q) xss = eval p xss `L.intersect` eval q xss
eval (Map f d p) xss = concatMap (h f d) (eval p xss)
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
  , (n, Inter <$> arbMono k c n2 <*> arbMono k c n2)
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
  arbitrary = arbProblem 5 5
  
  shrink (P defs xs) =
    [ P defs (xs \\ [x])
    | x <- xs
    ] ++
    [ P [ (y,filter (x/=) ys,del (count x ys) f) | (y,ys,f) <- defs, y /= x ] (xs \\ [x])
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

prop_Mu :: Problem -> Property
prop_Mu p@(P _ xs) =
  let ds  = defs p
      gr  = M.fromList [ (x,xs) | (x,xs,_) <- ds ]
      new = mu  [] ds xs
      old = mu0 [] ds xs
      
      report =
        do putStrLn (show new ++ " /= " ++ show old)
           print (dft gr xs)
      
   in whenFail report (new == old)

