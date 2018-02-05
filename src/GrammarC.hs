module GrammarC where

import Data.Either ( lefts )
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Char
import qualified Data.Set as S
import qualified Mu
import qualified FMap as F
import EqRel

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
  deriving ( Eq )

instance Show ConcrCat where
  show (CC (Just cat) fid) = cat ++ "_" ++ show fid 
  show (CC Nothing    fid) = "_" ++ show fid 

instance Ord ConcrCat where
  (CC _ fid1) `compare` (CC _ fid2) = fid1 `compare` fid2

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
  , coercions    :: [(ConcrCat,ConcrCat)]
  , coercionTab  :: S.Set (ConcrCat,ConcrCat)
  , contextsTab  :: M.Map ConcrCat (M.Map ConcrCat [Tree -> Tree])
  , startCat     :: Cat
  , symbols      :: [Symbol]
  , lookupSymbol :: String -> [Symbol]
  , functionsByCat :: Cat -> [Symbol]
  , concrSeqs    :: SeqId -> [Either String (Int,Int)] 
  , feat         :: FEAT
  , nonEmptyCats :: S.Set ConcrCat
  }


startConcrCats :: Grammar -> [ConcrCat]
startConcrCats gr = S.toList $ S.fromList $
  [ snd (ctyp f)
  | f <- functionsByCat gr (startCat gr)
  ]

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
        
        , contextsTab =
            M.fromList
            [ (top, M.fromList (contexts gr top))
            | top <- allCats
            ]
         
        , concrSeqs = 
            map cseq2Either . I.concrSequence lang

        , feat =
            mkFEAT gr
        
        , nonEmptyCats =
            S.fromList
            [ c
            | let -- all functions, organized by result type
                  funs = M.fromListWith (++) $
                    [ (b,[Right f])
                    | f <- symbols gr
                    , let (_,b) = ctyp f
                    ] ++
                    [ (b,[Left a])
                    | (b,a) <- coercions gr
                    ]
            
                  -- all categories, with their dependencies
                  defs =
                    [ if or [ arity f == 0 | Right f <- fs ]
                        then (c, [], \_ -> True) -- has a word
                        else (c, ys, h)          -- no word
                    | c <- allCats
                    , let -- relevant functions for c
                          fs = case M.lookup c funs of
                                 Nothing -> []
                                 Just fs -> fs
                    
                          -- categories we depend on
                          ys = S.toList $ S.fromList $
                               [ a | Right f <- fs, a <- fst (ctyp f) ] ++
                               [ a | Left a <- fs ]
                    
                          -- compute if we're empty, given the emptiness of others
                          h bs = or $
                            [ and [ tab M.! a | a <- as ]
                            | Right f <- fs
                            , let (as,_) = ctyp f
                            ] ++
                            [ tab M.! a
                            | Left a <- fs
                            ]
                           where
                            tab = M.fromList (ys `zip` bs)
                    ]
            , (c,True) <- allCats `zip` Mu.mu False defs allCats
            ]
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

  allCats = S.toList $ S.fromList $
            [ a
            | f <- symbs
            , let (as,b) = ctyp f
            , a <- b:as
            ] ++
            [ c
            | (a,b) <- coerces
            , c <- [a,b]
            ]

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

equalFields :: Grammar -> [(ConcrCat,EqRel Int)]
equalFields gr = cs `zip` eqrels
 where
  eqrels = Mu.mu Top defs cs
  cs     = S.toList (nonEmptyCats gr)
  
  defs =
    [ (c, ys, h)
    | c <- cs
      -- fs = everything that has c as a goal category
    , let fs = [ Right f
               | f <- symbols gr
               , all (`S.member` nonEmptyCats gr) (fst (ctyp f))
               , c == snd (ctyp f)
               ] ++
               [ Left b
               | (a,b) <- coercions gr
               , a == c
               , b `S.member` nonEmptyCats gr
               ]

          -- all the categories c depends on
          ys = S.toList $ S.fromList $ concat
               [ case f of
                   Right f -> fst (ctyp f)
                   Left b  -> [b]
               | f <- fs
               ]

          -- computes the equivalence relation, given the eq.rels of its arguments
          h rs = foldr (/\) Top $ [ apply f eqs
                  | Right f <- fs
                  , let eqs = map (args M.!) (fst $ ctyp f)
                  ] ++
                  [ args M.! b
                  | Left b <- fs
                  ]
           where
            args = M.fromList (ys `zip` rs)
    ]
   where
    apply f eqs =
      basic [ concatMap lin (concrSeqs gr sq)
            | sq <- seqs f
            ]
      where 
        lin (Left str)    = [ str | not (null str) ]
        lin (Right (i,j)) = [ show i ++ rep (eqs !! i) j ]

contextsFor :: Grammar -> ConcrCat -> ConcrCat -> [Tree -> Tree]
contextsFor gr top hole =
  case M.lookup hole (contextsTab gr M.! top) of
    Nothing -> []
    Just cs -> cs

contexts :: Grammar -> ConcrCat -> [(ConcrCat,[Tree -> Tree])]
contexts gr top =
  [ (c, map (path2context . reverse . snd) (F.toList paths))
  | (c, paths) <- cs `zip` pathss
  ]
 where
  pathss = Mu.muDiff F.nil F.isNil dif uni defs cs
  cs     = S.toList (nonEmptyCats gr)
  
  -- all symbols with at least one argument, and only good arguments
  goodSyms =
    [ f
    | f <- symbols gr
    , arity f >= 1
    , snd (ctyp f) `S.member` nonEmptyCats gr
    , all (`S.member` nonEmptyCats gr) (fst (ctyp f))
    ]
 
  -- definitions table for fixpoint iteration
  fm1 `dif` fm2 =
    [ d | d@(xs,_) <- F.toList fm1, not (fm2 `F.covers` xs) ] `ins` F.nil
  
  fm1 `uni` fm2 =
    F.toList fm1 `ins` fm2
  
  paths `ins` fm =
       foldl collect fm
     . map snd
     . sort
     $ [ (size p, p) | p <- paths ]
   where
    collect fm (str,p)
      | fm `F.covers` str = fm
      | otherwise         = F.add str p fm

    size (_,p) =
      sum [ if i == j then 1 else smallest gr t
          | (f,i) <- p
          , let (ts,_) = ctyp f
          , (t,j) <- ts `zip` [0..]
          ]

  defs =
    [ if c `isCoercableTo` top
        then (c, [], \_ -> F.unit [0] [])
        else (c, ys, h)
    | c <- cs
    , let fs = [ Right (f,k)
               | f <- goodSyms
               , (t,k) <- fst (ctyp f) `zip` [0..]
               , t == c
               ] ++
               [ Left a
               | (a,b) <- coercions gr
               , b == c
               , a `S.member` nonEmptyCats gr
               ]
          
          ys = S.toList $ S.fromList $
               [ case f of
                   Right (f,_) -> snd (ctyp f)
                   Left a      -> a
               | f <- fs
               ]

          h ps = ([ (apply (f,k) str, (f,k):fis)
                  | Right (f,k) <- fs
                  , (str,fis) <- args M.! snd (ctyp f)
                  ] ++
                  [ q
                  | Left a <- fs
                  , q <- args M.! a
                  ]) `ins` F.nil
           where
            args = M.fromList (ys `zip` map F.toList ps)
    ]
   where
    apply (f,k) is =
      S.toList $ S.fromList $
      [ y
      | (sq,i) <- seqs f `zip` [0..]
      , i `elem` is
      , Right (x,y) <- concrSeqs gr sq
      , x == k
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

--traceLength s xs = trace (s ++ ":" ++ show (length xs)) xs

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

--------------------------------------------------------------------------------
-- return a list of symbols that have a specified string, e.g. "it" in English
-- grammar appears in functions CleftAdv, CleftNP, ImpersCl, DefArt, it_Pron

hasConcrString :: Grammar -> String -> [Symbol]
hasConcrString gr str =
  [ symb
  | symb <- symbols gr 
  , str `elem` concatMap (lefts . concrSeqs gr) (seqs symb) ]

