module GrammarC where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import qualified Data.Set as S

import Debug.Trace

import qualified PGF2
import qualified PGF2.Internal as I

--------------------------------------------------------------------------------
-- grammar types

-- name

type Name = String
type Cat  = PGF2.Cat -- i.e. String
type SeqId = Int

-- concrete category

data ConcrCat = CC (Maybe Cat) I.FId -- i.e. Int
  deriving ( Ord, Eq )

instance Show ConcrCat where
  show (CC (Just cat) fid) = cat ++ "_" ++ show fid 
  show (CC Nothing fid)    = "_" ++ show fid 

-- tree

data Tree
  = App{ top :: Symbol, args :: [Tree] }
 deriving ( Eq, Ord )

-- symbol

data Symbol
  = Symbol
  { name :: Name
  , seqs :: [SeqId]
  , typ  :: ([Cat], Cat)
  , ctyp :: ([ConcrCat],ConcrCat)
  }
 deriving ( Eq, Ord )

hole :: ConcrCat -> Symbol
hole c = Symbol "()" [] ([], "") ([],c)

isHole :: Symbol -> Bool
isHole (Symbol "()" _ _ _) = True
isHole _                   = False


-- grammar

data Grammar
  = Grammar
  {
  -- parse        :: String -> [Tree]
    linearize    :: Tree -> String
  , linearizeAll :: Tree -> [String]
  , tabularLin   :: Tree -> [(String,String)]
  , concrCats    :: [(PGF2.Cat,I.FId,I.FId,[String])]
--  , productions  :: I.FId -> [I.Production] --just for debugging
  , coercions    :: [(ConcrCat,ConcrCat)]
  , startCat     :: Cat
  , symbols      :: [Symbol]
  , concrSeqs    :: SeqId -> [Either String (Int,Int)] 
  , feat         :: FEAT
  }

--------------------------------------------------------------------------------
-- analyzing contexts

uses :: Grammar -> Tree -> ([S.Set Int],S.Set Int)
uses gr (App h []) | isHole h =
  ([ S.singleton i | i <- [0..n-1] ], S.fromList [ i | i <- [0..n-1] ])
 where
  (_,c) = ctyp h
  n     = head [ length (seqs f) 
                 | f <- symbols gr      -- all functions that return a tree 
                 , let (_,c') = ctyp f  -- of the same type as h
                 , coerce gr c == coerce gr c' ]

  coerce :: Grammar -> ConcrCat -> ConcrCat
  coerce gr ccat = fromMaybe ccat (lookup ccat (coercions gr))

uses gr (App f xs) =
  ([ S.unions
    [ (us !! i) !! j
    | Right (i,j) <- concrSeqs gr sid
    ]
  | sid <- seqs f
  ], is)
 where
  uxs = map (uses gr) xs
  us = trace (show uxs) $ map fst uxs
  is = snd $ head uxs

contextsFor :: Grammar -> ConcrCat -> ConcrCat -> [Tree -> Tree]
contextsFor gr top hole =
  concat
  [ map (path2context . snd) paths
  | (c,paths) <- fix improve start
  , c == top
  ]
 where
  allcats = S.toList $ S.fromList $
    hole :
    [ c
    | f <- symbols gr
    , let (_,c) = ctyp f
    ] ++
    [ c
    | (_,c) <- coercions gr
    ]

  arHole =
    head $
    [ length (seqs f)
    | f <- symbols gr
    , snd (ctyp f) == hole
    ] ++
    error ("no symbol found with result type " ++ show hole)

  start =
    [ (c, if c == hole
            then [([S.fromList [i] | i <- [0..arHole-1]],[])]
            else [])
    | c <- allcats
    ]

  improve tab =
    [ (c,paths `imprs`
           [ (apply (f,i) str, (f,i):fis)
           | f <- symbols gr
           , snd (ctyp f) == c
           , all (\t -> not (null (featAll gr t))) (fst (ctyp f))
           , (t,i) <- fst (ctyp f) `zip` [0..]
           , (c',paths') <- tab
           , t == c'
           , (str,fis) <- paths'
           ])
    | (c,paths) <- tab
    ]
   where
    apply (f,i) str =
      [ S.fromList
        [ s
        | Right (x,y) <- concrSeqs gr sq
        , x == i
        , s <- S.toList (str !! y)
        ]
      | sq <- seqs f
      ]

    paths `imprs` paths' =
      foldr impr paths (M.toList $ M.fromList $ paths')
     where
      (str',path') `impr` paths
        | any (`covers` str') (map fst paths) =
          paths
        
        | otherwise =
          (str',path') : filter (not . (str' `covers`) . fst) paths

      str1 `covers` str2 =
        and [ s2 `S.isSubsetOf` s1 | (s1,s2) <- str1 `zip` str2 ]

  path2context []          x = x
  path2context ((f,i):fis) x =
    App f
    [ if j == i
        then path2context fis x
        else head (featAll gr t)
    | (t,j) <- fst (ctyp f) `zip` [0..]
    ]

  fix f x | fx == x   = x
          | otherwise = fix f fx
         where
          fx = f x

--------------------------------------------------------------------------------
-- name

mkName, mkCat :: String -> Name
mkName = id
mkCat  = id

-- tree

instance Show Tree where
  show = showTree

showTree :: Tree -> String
showTree (App a []) = show a
showTree (App f xs) = unwords (show f : map showTreeArg xs)
  where showTreeArg (App a []) = show a
        showTreeArg t = "(" ++ showTree t ++ ")"

catOf :: Tree -> Cat
catOf (App f _) = snd (typ f)

-- symbol

instance Show Symbol where
  show f = name f --CId is just a String in PGF2

-- grammar

toGrammar :: PGF2.PGF -> Grammar
toGrammar pgf =
  let gr =
        Grammar
        {
         --parse = \s ->
         --   case PGF2.parse lang (PGF2.startCat pgf) s of 
         --     PGF2.ParseOk es_fs -> map (mkTree.fst) es_fs
         --     PGF2.ParseFailed i s -> error s
         --     PGF2.ParseIncomplete -> error "Incomplete parse"

          linearize = \t ->
            PGF2.linearize lang (mkExpr t)

        , linearizeAll = \t -> 
            PGF2.linearizeAll lang (mkExpr t)

        , tabularLin = \t ->
            PGF2.tabularLinearize lang (mkExpr t)

        , startCat =
            mkCat (PGF2.startCat pgf)

        , concrCats = I.concrCategories lang

        , symbols = 
           [ Symbol { 
               name = nm,
               seqs = sqs,
               ctyp = (cArgTypes, cResType),
               typ = (map abstrCat cArgTypes, abstrCat cResType) } --this takes care of coercions

             | (cat,bg,end,_) <- concrCats gr 
             , resFid <- [bg..end] 
             , I.PApply funId pargs <- I.concrProductions lang resFid
             , let cArgTypes = [ CC (getGFCat fid) fid | I.PArg _ fid <- pargs ]
             , let cResType = CC (getGFCat resFid) resFid 
             , let (nm,sqs) = I.concrFunction lang funId ]

        , coercions = coerces

        , concrSeqs = map cseq2Either . I.concrSequence lang

        , feat =
            mkFEAT gr
        }
   in gr
 where

  cseq2Either (I.SymKS tok) = Left tok
  cseq2Either (I.SymCat x y) = Right (x,y)
  cseq2Either x = Left (show x)

  lang = snd $ head $ M.assocs $ PGF2.languages pgf  

  coerces = [ ( CC Nothing afid
              , CC ccat cfid )
              | afid <- [0..I.concrTotalCats lang]
              , I.PCoerce cfid  <- I.concrProductions lang afid 
              , let ccat = getGFCat cfid ] --can there be multiple coercionss, like X -> Y -> Z?


  abstrCat :: ConcrCat -> Cat
  abstrCat c@(CC Nothing _)  = let Just ccat = lookup c coerces 
                                in abstrCat ccat
  abstrCat (CC (Just cat) _) = cat
  
  getGFCat :: I.FId -> Maybe Cat
  getGFCat fid = 
      let cats = nub [ cat | (cat,bg,end,xs) <- I.concrCategories lang
                         , fid `elem` [bg..end] ]
     in case cats of 
          [] -> --trace ("getGFCat: no values for fID " ++ show fid) $ 
                Nothing -- means it's coercion
          (x:xs) -> Just x

-- Only needed for parse. If we need it, find out what to put in ctyp of Symbol.
{-
  mkSymbol f = Symbol f ([mkCat x | (_,_,x) <- xs],y) [????]
   where
    Just ft    = PGF2.functionType pgf f -- functionType :: PGF -> Fun -> Maybe Type
    (xs, y, _) = PGF2.unType ft

   
  mkTree t =
   case PGF2.unApp t of
     Just (f,xs) -> App (mkSymbol f) [ mkTree x | x <- xs ]
     _           -> error (PGF2.showExpr [] t)
-}  
  mkExpr (App n []) | not (null s) && all isDigit s =
    PGF2.mkInt (read s)
   where
    s = show n

  mkExpr (App f xs) =
    PGF2.mkApp (name f) [ mkExpr x | x <- xs ]
  
  mkCat  tp  = cat where (_, cat, _) = PGF2.unType tp
  mkType cat = PGF2.mkType [] cat []

readGrammar :: FilePath -> IO Grammar
readGrammar file =
  do pgf <- PGF2.readPGF file
     return (toGrammar pgf)

-- FEAT-style generator magic

type FEAT = [ConcrCat] -> Maybe ConcrCat -> Int -> (Integer, Integer -> [Tree])

-- compute how many trees there are of a given size and type
featCard :: Grammar -> ConcrCat -> Int -> Integer
featCard gr c n = featCardVec gr [c] n

-- generate the i-th tree of a given size and type
featIth :: Grammar -> ConcrCat -> Int -> Integer -> Tree
featIth gr c n i = head (featIthVec gr [c] n i)

-- generate all trees (infinitely many) of a given type
featAll :: Grammar -> ConcrCat -> [Tree]
featAll gr c = [ featIth gr c n i | n <- [0..], i <- [0..featCard gr c n-1] ]

-- compute how many tree-vectors there are of a given size and type-vector
featCardVec :: Grammar -> [ConcrCat] -> Int -> Integer
featCardVec gr cs n = fst (feat gr cs Nothing n)

-- generate the i-th tree-vector of a given size and type-vector
featIthVec :: Grammar -> [ConcrCat] -> Int -> Integer -> [Tree]
featIthVec gr cs n i = snd (feat gr cs Nothing n) i

-- compute how many contexts there are of a given size, type, and hole-type
featCardCtx :: Grammar -> ConcrCat -> ConcrCat -> Int -> Integer
featCardCtx gr c hc n = fst (feat gr [c] (Just hc) n)

-- generate the i-th context of a given size, type, and hole-type
featIthCtx :: Grammar -> ConcrCat -> ConcrCat -> Int -> Integer -> Tree
featIthCtx gr c hc n i = head (snd (feat gr [c] (Just hc) n) i)

mkFEAT :: Grammar -> FEAT
mkFEAT gr = catList
 where
  catList' :: FEAT
  catList' [] Nothing 0 = (1, \0 -> [])
  catList' [] _       _ = (0, error "empty")

  catList' [c] mh s =
    parts $ 
          [ (n, \i -> [App f (h i)])
          | s > 0 
          , f <- symbols gr
          , let (xs,y) = ctyp f
          , y == c
          , let (n,h) = catList xs mh (s-1)
          ] ++
          [ catList [x] mh s -- put (s-1) if it doesn't terminate
          | s > 0 
          , (x,y) <- coercions gr
          , y == c
          ] ++
          [ (1, \0 -> [App (hole c) []])
          | s == 1 -- holes have size 1
          , Just c' <- [mh]
          , c == c'
          ]

  catList' (c:cs) Nothing s =
    parts [ (nx*nxs, \i -> hx (i `mod` nx) ++ hxs (i `div` nx))
          | k <- [0..s]
          , let (nx,hx)   = catList [c] Nothing k
                (nxs,hxs) = catList cs Nothing (s-k)
          ]

  catList' (c:cs) mh s =
    parts $ [ (nx*nxs, \i -> hx (i `mod` nx) ++ hxs (i `div` nx))
          | k <- [0..s]
          , let (nx,hx)    = catList [c] mh k
                (nxs',hxs) = catList cs Nothing (s-k)
                nxs        = if nxs' == 0 then 0 else 1
          ] ++
          [ (nx*nxs, \i -> hx (i `mod` nx) ++ hxs (i `div` nx))
          | k <- [0..s]
          , let (nx',hx)  = catList [c] Nothing k
                nx        = if nx' == 0 then 0 else 1
                (nxs,hxs) = catList cs mh (s-k)
          ]


  catList :: FEAT
  catList =
    memoList (\cs ->
      memoMaybe (\mh ->
        memoNat (catList' cs mh)))
   where
    cats = nub $ [ x | f <- symbols gr
                     , let (xs,y) = ctyp f
                     , x <- y:xs ] ++
                 [ z | (x,y) <- coercions gr
                     , z <- [x,y] ] --adds all possible categories in the grammar

    memoList f = \cs -> case cs of
                    []   -> fNil
                    a:as -> fCons a as
     where
      fNil  = f []
      fCons = \a -> head [ fc | (c,fc) <- tab, a == c ]
      tab   = [ (c, memoList (f . (c:))) | c <- cats ]

    memoMaybe f = \mx -> case mx of
                    Nothing -> fNothing
                    Just a  -> fJust a
     where
      fNothing = f Nothing
      fJust    = \a -> head [ fc | (c,fc) <- tab, a == c ]
      tab      = [ (c, f (Just c)) | c <- cats ]

    memoNat f = (tab!!)
     where
      tab = [ f i | i <- [0..] ]

  parts []          = (0, error "empty parts ")
  parts ((n,h):nhs) = (n+n', \i -> if i < n then h i else h' (i-n))
   where
    (n',h') = parts nhs

--------------------------------------------------------------------------------

