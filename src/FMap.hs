module FMap where

--------------------------------------------------------------------------------
-- implementation

data FMap a b = Ask a (FMap a b) (FMap a b) | Nil | Answer b
  deriving ( Eq, Ord, Show )

contents :: FMap a b -> [([a],b)]
contents (Ask x yes no) = [ (x:xs,z) | (xs,z) <- contents yes ] ++ contents no
contents Nil            = []
contents (Answer z)     = [([],z)]

covers :: Ord a => FMap a b -> [a] -> Bool
Nil          `covers` _         = False
_            `covers` []        = True
Answer _     `covers` _         = False
Ask x yes no `covers` zs@(y:ys) =
  case x `compare` y of
    LT -> (yes `covers` zs) || (no `covers` zs)
    EQ -> yes `covers` ys
    GT -> False

ask :: a -> FMap a b -> FMap a b -> FMap a b
ask x Nil Nil = Nil
ask x s   t   = Ask x s t

del :: Ord a => [a] -> FMap a b -> FMap a b
del _  Nil                  = Nil
del _  (Answer _)           = Nil
del [] (Ask x yes no)       = ask x yes (del [] no)
del (x:xs) t@(Ask y yes no) =
  case x `compare` y of
    LT -> del xs t
    EQ -> ask y (del xs yes) (del xs no)
    GT -> ask y yes (del (x:xs) no)

add :: Ord a => [a] -> b -> FMap a b -> FMap a b
add []       y Nil              = Answer y
add (x:xs)   y Nil              = Ask x (add xs y Nil) Nil
add xs@(_:_) y (Answer _)       = add xs y Nil
add (x:xs)   y t@(Ask z yes no) =
  case x `compare` z of
    LT -> Ask x (add xs y Nil) (del xs t)
    EQ -> Ask x (add xs y yes) (del xs no)
    GT -> Ask z yes (add (x:xs) y no)

--------------------------------------------------------------------------------

