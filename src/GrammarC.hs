module GrammarC where

import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import qualified Mu

import GHC.Exts ( the )
import Debug.Trace

import qualified PGF2
import qualified PGF2.Internal as I

--------------------------------------------------------------------------------
-- grammar types

-- name

type Name = String

-- concrete category

type Cat  = PGF2.Cat -- i.e. String

data ConcrCat = CC (Maybe Cat) I.FId -- i.e. Int
  deriving ( Ord, Eq )

instance Show ConcrCat where
  show (CC (Just cat) fid) = cat ++ "_" ++ show fid 
  show (CC Nothing    fid) = "_" ++ show fid 

-- tree

data Tree
  = App { top :: Symbol, args :: [Tree] }
 deriving ( Eq, Ord )

data AmbTree -- only used as an intermediate structure for parsing
  = AmbApp { atop :: [Symbol], aargs :: [AmbTree] } 

-- symbol

type SeqId = Int

data Symbol
  = Symbol
  { name :: Name
  , seqs :: [SeqId]
  , typ  :: ([Cat], Cat)
  , ctyp :: ([ConcrCat],ConcrCat)
  }
 deriving ( Eq, Ord )

instance Show Symbol where
  show = name

arity :: Symbol -> Int
arity = length . fst . ctyp

hole :: ConcrCat -> Symbol
hole c = Symbol (show c) [] ([], "") ([],c)

-- grammar

type Lang = String

data Grammar 
  = Grammar
  {
    parse        :: String -> [Tree]
  , readTree     :: String -> Tree
  , linearize    :: Tree -> String
  , tabularLin   :: Tree ->  [(String,String)]
  , concrCats    :: [(PGF2.Cat,I.FId,I.FId,[String])]
  , coercions    :: [(ConcrCat,ConcrCat)] -- M.Map ConcrCat ConcrCat 
  , coercionTab  :: S.Set (ConcrCat,ConcrCat)
  , startCat     :: Cat
  , symbols      :: [Symbol]
  , lookupSymbol :: String -> [Symbol]
  , functionsByCat :: Cat -> [Symbol]
  , concrSeqs    :: SeqId -> [Either String (Int,Int)] 
  , feat         :: FEAT
  }


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

-- grammar

readGrammar :: Lang -> FilePath -> IO Grammar
readGrammar lang file =
  do pgf <- PGF2.readPGF file
     return (toGrammar pgf lang)


toGrammar :: PGF2.PGF -> Lang -> Grammar
toGrammar pgf langName =
  let gr =
        Grammar
        {


         parse = \s ->
            case PGF2.parse lang (PGF2.startCat pgf) s of 
              PGF2.ParseOk es_fs -> map (mkTree.fst) es_fs
              PGF2.ParseFailed i s -> error s
              PGF2.ParseIncomplete -> error "Incomplete parse"

        , readTree = \s ->
            case PGF2.readExpr s of
              Just t  -> mkTree t
              Nothing -> error "readTree: no parse"

        , linearize = \t ->
            PGF2.linearize lang (mkExpr t)

        , tabularLin = \t ->
            PGF2.tabularLinearize lang (mkExpr t)

        , startCat =
            mkCat (PGF2.startCat pgf)

        , concrCats = 
            I.concrCategories lang

        , symbols = symbs

        , lookupSymbol = lookupSymbs

        , functionsByCat = \c ->
            S.toList $ S.fromList [ symb | symb <- symbs, snd (typ symb) == c ]

        , coercions = coerces

        , coercionTab =
            let go tab []         = tab
                go tab ((a,b):cs) = new (S.insert (a,b) tab)
                                        ( [ (x,b) | (x,y) <- S.toList tab, y == a ]
                                       ++ [ (a,y) | (x,y) <- S.toList tab, x == b ]
                                        )
                                        cs
                
                new tab (ab:ds) cs
                  | ab `S.member` tab = new tab ds cs
                  | otherwise         = new tab ds (ab:cs)
                new tab [] cs         = go tab cs

             in go S.empty [ (a,b) | (b,a) <- coerces ]
              
        , concrSeqs = 
            map cseq2Either . I.concrSequence lang

        , feat =
            mkFEAT gr
        }
   in gr
 where

  -- language
  lang = case M.lookup langName (PGF2.languages pgf) of
           Just la -> la
           Nothing -> let (defName,defGr) = head $ M.assocs $ PGF2.languages pgf
                          msg = "no grammar found with name " ++ langName ++ 
                                ", using " ++ defName
                      in trace msg defGr

  -- categories and coercions

  mkCat tp = cat where (_, cat, _) = PGF2.unType tp

  mkExpr (App n []) | not (null s) && all isDigit s =
    PGF2.mkInt (read s)
   where
    s = show n

  mkExpr (App f xs) =
    PGF2.mkApp (name f) [ mkExpr x | x <- xs ] 
     
  mkCC fid = CC ccat fid 
   where ccat = case [ cat | (cat,bg,end,_) <- I.concrCategories lang
                           , fid `elem` [bg..end] ] of 
                  [] -> Nothing -- means it's coercion
                  xs -> Just $ the xs


  coerces = [ ( CC Nothing afid, mkCC cfid )
              | afid <- [0..I.concrTotalCats lang]
              , I.PCoerce cfid <- I.concrProductions lang afid ]

  coerce c = case lookupAll coerces c of
               [] -> [c]
               cs -> cs



  -- symbols
  symbs = [ Symbol { 
              name = nm,
              seqs = sqs,
              ctyp = (argsCC, resCC),
              typ = (map coerceAbsCat argsCC, rescat)
            }
            | (rescat,bg,end,_) <- I.concrCategories lang
            , resfid <- [bg..end] 
            , I.PApply funId pargs <- I.concrProductions lang resfid
            , let resCC  = CC (Just rescat) resfid 
            , let argsCC = [ mkCC argfid | I.PArg _ argfid <- pargs ]
            , let (nm,sqs) = I.concrFunction lang funId ]
   where
    coerceAbsCat c = case c of
      CC (Just cat) _ -> cat
      CC Nothing    _ -> the [ coerceAbsCat x | x <- coerce c ]


  lookupSymbs = lookupAll (map symb2table symbs)
   where symb2table s = (name s, s)


  -- parsing and reading trees

  mkTree :: PGF2.Expr -> Tree
  mkTree = at2tree . mkAmbTree

  mkAmbTree t = -- :: PGF2.Expr -> AmbTree
    case PGF2.unApp t of
      Just (f,xs) -> AmbApp (lookupSymbs f) [ mkAmbTree x | x <- xs ]
      _           -> error (PGF2.showExpr [] t)

  at2tree at =  -- :: AmbTree -> Tree
    case iterate reduce at !! 10 of --TODO: some more legit fixpoint computation here?
      AmbApp [x]   ts -> App x [ at2tree t | t <- ts ]
      AmbApp []   _ts -> error "mkTree: empty tree"
      AmbApp (x:_) ts -> error "mkTree: ambiguous tree"
       {- trace ("\n*** mkTree: ambiguous tree " ++ show x ++ " ***\n") $
                        App x [ at2tree t | t <- ts ] -}
 
   where 
    reduce at = AmbApp (reduceSymbol at) [ reduce t | t <- aargs at ]

    reduceSymbol (AmbApp fs as) = case all (singleton . atop) as of
      True  -> let red = [ symbol
                           | symbol <- fs
                           , let argTypes = map coerce ( fst $ ctyp symbol )
                           , let resTypes = map coerce [ snd $ ctyp s 
                                                        | (AmbApp [s] _) <- as ] 
                           , and [ intersect a r /= [] -- TODO: can this lead to ambiguous trees?
                                 | (a,r) <- zip argTypes resTypes ] ]
                in case red of
                    [x] -> [x]
                    _   -> fs
      False -> fs

  -- misc
  lookupAll kvs key = [ v | (k,v) <- kvs, k==key ]  

  singleton [x] = True
  singleton xs  = False

  cseq2Either (I.SymKS tok)  = Left tok
  cseq2Either (I.SymCat x y) = Right (x,y)
  cseq2Either x              = Left (show x)

--------------------------------------------------------------------------------
-- analyzing contexts

data Pair a b = Pair{ pfst :: a, psnd :: b } deriving ( Ord, Show )

instance Eq a => Eq (Pair a b) where
  Pair x _ == Pair y _ = x == y

contextsFor :: Grammar -> ConcrCat -> ConcrCat -> [Tree -> Tree]
contextsFor gr top hole =
  let [paths] = Mu.mu [] defs [top] in
    map (path2context . psnd) paths
 where
  -- all non-empty categories
  goodCats =
    [ c
    | c <- S.toList $ S.fromList $
           hole :
           [ c
           | f <- symbols gr
           , let (as,b) = ctyp f
           , c <- b:as
           ] ++
           [ c
           | (b,a) <- coercions gr -- !now flipped
           , c <- [a,b]
           ]
    , any ((>0) . featCard gr c) [0..25] -- 15 is arbitrary
    ]

  -- all cats reachable from top
  reachTop = go S.empty [top]
   where
    go seen []     = seen
    go seen (c:cs)
      | c `S.member` seen = go seen cs
      | otherwise         = go (S.insert c seen) $
                            [ a
                            | f <- goodSyms
                            , let (as,b) = ctyp f
                            , b == c
                            , a <- as
                            ] ++
                            [ a
                            | (b,a) <- coercions gr
                            , b == c
                            ] ++
                            cs

  -- all cats that can reach hole
  reachHole = go S.empty [hole]
   where
    go seen []     = seen
    go seen (c:cs)
      | c `S.member` seen = go seen cs
      | otherwise         = go (S.insert c seen) $
                            [ b
                            | f <- goodSyms
                            , let (as,b) = ctyp f
                            , c `elem` as
                            ] ++
                            [ b
                            | (b,a) <- coercions gr
                            , a == c
                            ] ++
                            cs

  -- all symbols with at least one argument, and only good arguments
  goodSyms =
    [ f
    | f <- symbols gr
    , arity f >= 1
    , all (\t -> t `elem` goodCats) (fst (ctyp f))
    ]
 
  -- length of string vector for the hole type
  arHole =
    head $
    [ length (seqs f)
    | f <- symbols gr
    , snd (ctyp f) == hole
    ] ++
    error ("no symbol found with result type " ++ show hole)

  reachCats = reachTop `S.intersection` reachHole

  -- definitions table for fixpoint iteration
  defs =
    [ if hole `isCoercableTo` c
        then (c, [], \_ -> [Pair [S.fromList [i] | i <- [0..arHole-1]] []])
        else (c, ys, h)
    | c <- S.toList reachCats
    , let fs = [ Right f | f <- goodSyms, arity f >= 1, snd (ctyp f) == c ] ++
               [ Left b | (a,b) <- coercions gr, a == c, b `S.member` reachCats ]
          ys = S.toList $ S.fromList [ a | f <- fs, a <- case f of
                                                           Right f -> fst (ctyp f)
                                                           Left b  -> [b], a `S.member` reachCats ]

          h ps =
            best $
            [ Pair (apply (f,i) str) ((f,i):fis)
            | Right f <- fs
            , (t,i) <- fst (ctyp f) `zip` [0..]
            , t `S.member` reachCats
            , Pair str fis <- args M.! t
            ] ++
            [ q
            | Left b <- fs
            , q <- args M.! b
            ]
           where
            args = M.fromList (ys `zip` ps)
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

    best paths =
      sort $ foldr impr [] $ map snd $ reverse $ sort [ (size p, p) | p <- paths ]
     where
      Pair str' path' `impr` paths
        | any (`covers` str') (map pfst paths) =
          paths
        
        | otherwise =
          Pair str' path' : filter (not . (str' `covers`) . pfst) paths

      str1 `covers` str2 =
        and [ s2 `S.isSubsetOf` s1 | (s1,s2) <- str1 `zip` str2 ]

      size (Pair _ p) =
        sum [ if i == j then 1 else smallest gr t
            | (f,i) <- p
            , let (ts,_) = ctyp f
            , (t,j) <- ts `zip` [0..]
            ]

  a `isCoercableTo` b = a==b || ((a,b) `S.member` coercionTab gr)
  
  path2context []          x = x
  path2context ((f,i):fis) x =
    App f
    [ if j == i
        then path2context fis x
        else head (featAll gr t)
    | (t,j) <- fst (ctyp f) `zip` [0..]
    ]

--------------------------------------------------------------------------------
-- FEAT-style generator magic

type FEAT = [ConcrCat] -> Int -> (Integer, Integer -> [Tree])

smallest :: Grammar -> ConcrCat -> Int
smallest gr c = head [ n | n <- [0..], featCard gr c n > 0 ]

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
featCardVec gr cs n = fst (feat gr cs n)

-- generate the i-th tree-vector of a given size and type-vector
featIthVec :: Grammar -> [ConcrCat] -> Int -> Integer -> [Tree]
featIthVec gr cs n i = snd (feat gr cs n) i

mkFEAT :: Grammar -> FEAT
mkFEAT gr = catList
 where
  catList' :: FEAT
  catList' [] 0 = (1, \0 -> [])
  catList' [] _ = (0, error "indexing in an empty sequence")

  catList' [c] s =
    parts $ 
          [ (n, \i -> [App f (h i)])
          | s > 0 
          , f <- symbols gr
          , let (xs,y) = ctyp f
          , y == c
          , let (n,h) = catList xs (s-1)
          ] ++
          [ catList [x] s -- put (s-1) if it doesn't terminate
          | s > 0 
          , (y,x) <- coercions gr -- !now flipped
          , y == c
          ]

  catList' (c:cs) s =
    parts [ (nx*nxs, \i -> hx (i `mod` nx) ++ hxs (i `div` nx))
          | k <- [0..s]
          , let (nx,hx)   = catList [c] k
                (nxs,hxs) = catList cs (s-k)
          ]

  catList :: FEAT
  catList = memoList (memoNat . catList')
   where
    -- all possible categories of the grammar
    cats = S.toList $ S.fromList $
           [ x | f <- symbols gr
               , let (xs,y) = ctyp f
               , x <- y:xs ] ++
           [ z | (y,x) <- coercions gr -- !now flipped
               , z <- [x,y] ]

    memoList f = \cs -> case cs of
                    []   -> fNil
                    a:as -> fCons a as
     where
      fNil  = f []
      fCons = (tab M.!)
      tab   = M.fromList [ (c, memoList (f . (c:))) | c <- cats ]

    memoNat f = (tab!!)
     where
      tab = [ f i | i <- [0..] ]

  parts []          = (0, error "indexing outside of a sequence")
  parts ((n,h):nhs) = (n+n', \i -> if i < n then h i else h' (i-n))
   where
    (n',h') = parts nhs

--------------------------------------------------------------------------------
-- compare two grammars

diffCats :: Grammar -> Grammar -> [(Cat,[Int],[String],[String])]
diffCats gr1 gr2 = 
  [ (acat1,[difFid c1, difFid c2],labels1  \\ labels2,labels2 \\ labels1)
    | c1@(acat1,_i1,_j2,labels1) <- concrCats gr1
    , c2@(acat2,_i2,_j2,labels2) <- concrCats gr2
    , difFid c1 /= difFid c2 -- different amount of concrete categories
      || labels1 /= labels2 -- or the labels are different
    , acat1==acat2 ]

 where
  difFid (_,i,j,_) = 1 + (j-i)


