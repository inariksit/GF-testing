module Mu where

import Data.Map( Map, (!) )
import qualified Data.Map as M
import Data.Set( Set )
import qualified Data.Set as S
import qualified Control.Monad.State as SM

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
mu :: (Ord x, Eq a) => (a->a->a) -> a -> [(x, [x], [a] -> a)] -> [x] -> [a]
mu (\/) bot defs zs = [ done!z | z <- zs ]
 where
  xs    = [ x | (x, _, _) <- defs ]
  deps  = M.fromList [ (x,ys) | (x,ys,_) <- defs ]
  qSet  = break 0 S.empty [ (z,0) | z <- zs ]
  qs    = S.toList qSet
  done  = iter [ tab0!q | q <- qs ] tab0
  tab0  = step (M.fromList [ (x,bot) | x <- xs ])

  break l seen [] =
    S.empty
  
  break l seen ((x,i):xs) =

  iter vs tab
    | vs == vs' = tab
    | otherwise = iter vs' tab'
   where
    vs'  = [ tab'!q | q <- qs ]
    tab' = step tab

  step tab = tab'
   where
    tab' = M.fromList
           [ (x, (tab'!x) \/ f [ if y `S.member` qSet
                                   then tab!y
                                   else tab'!y | y <- ys ])
           | (x, ys, f) <- defs
           ]
-}

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

data SCCState x
  = SCCState
  { index    :: Int
  , mindex   :: Map x Int
  , mlowlink :: Map x Int
  }

scc :: Ord x => Map x [x] -> [x] -> [[x]]
scc graph starts =
    reverse
  . map snd
  . M.toList
  . M.fromListWith (++)
  $ [ (i,[x])
    | (x,i) <- M.toList (mlowlink s)
    ]
 where
   s =
     SM.execState (sccM graph starts) $ SCCState 0 M.empty M.empty

sccM :: Ord x => Map x [x] -> [x] -> SM.State (SCCState x) ()
sccM graph starts =
  sequence_
    [ do mi <- getIndexOf x
         case mi of
           Nothing -> sccNode graph x
           _       -> return ()
    | x <- starts
    ]

sccNode :: Ord x => Map x [x] -> x -> SM.State (SCCState x) ()
sccNode graph x =
  do i <- getIndex
     setIndexOf x i
     setLowlinkOf x i
     setIndex (i+1)
     sequence_
       [ do mj <- getIndexOf y
            case mj of
              Nothing ->
                do sccNode graph y
                   i <- getLowlinkOf x
                   j <- getLowlinkOf y
                   setLowlinkOf x (i `min` j)
               
              Just j ->
                do i <- getLowlinkOf x
                   setLowlinkOf x (i `min` j)
       | y <- graph ! x
       ]

getIndex :: SM.State (SCCState x) Int
getIndex = SM.gets index

getIndexOf :: Ord x => x -> SM.State (SCCState x) (Maybe Int)
getIndexOf x = SM.gets (M.lookup x . mindex)

getLowlinkOf :: Ord x => x -> SM.State (SCCState x) Int
getLowlinkOf x = SM.gets ((! x) . mlowlink)

setIndex :: Int -> SM.State (SCCState x) ()
setIndex i = SM.modify $ \s -> s{ index = i }

setIndexOf :: Ord x => x -> Int -> SM.State (SCCState x) ()
setIndexOf x i = SM.modify $ \s -> s{ mindex = M.insert x i (mindex s) }

setLowlinkOf :: Ord x => x -> Int -> SM.State (SCCState x) ()
setLowlinkOf x i = SM.modify $ \s -> s{ mlowlink = M.insert x i (mlowlink s) }

--------------------------------------------------------------------------------

