module Lib
    ( testTree
    , testFun
    , compareTree
    , ccats
    , Comparison(..)
    , treesUsingFun
    , showConcrFun
    , lookupSymbol
    , tabularPrint
    ) where

import GrammarC
import Paths_GF_testing
import Control.Monad ( when )
import Data.List
import Data.Either
import qualified Data.Set as S
import Data.Maybe
import Debug.Trace

type LinTree = ((Lang,String),(Lang,String),(Lang,String),(Lang,String))
data Comparison = Comparison { funTree :: String, linTree :: [LinTree] }
instance Show Comparison where
  show c = unlines $ funTree c : map showLinTree (linTree c)

showLinTree :: LinTree -> String
showLinTree ((an,hl),(l1,t1),(l2,t2),(_l,[])) = unlines ["", an++hl, l1++t1, l2++t2]
showLinTree ((an,hl),(l1,t1),(l2,t2),(l3,t3)) = unlines ["", an++hl, l1++t1, l2++t2, l3++t3]


compareFun :: Grammar -> Grammar -> [Grammar] -> Name -> [Comparison]
compareFun gr oldgr transgr funname =
  [ compareTree gr oldgr transgr tree
  | tree <- treesUsingFun gr (lookupSymbol gr funname) ]

compareTree :: Grammar -> Grammar -> [Grammar] -> Tree -> Comparison
compareTree gr oldgr transgr t = Comparison {
  funTree = "### " ++ show t
, linTree = [ ( (absName gr,hl), (langName gr,newLin), (langName oldgr, oldLin), transLin )
            | ctx <- ctxs
            , let hl = show (ctx (App (hole c) []))
            , let transLin = case transgr of
                              []  -> ("","")
                              g:_ -> (langName g, linearize g (ctx t))
            , let newLin = linearize gr (ctx t)
            , let oldLin = linearize oldgr (ctx t)
            , newLin /= oldLin ]
 }
 where
  w    = top t
  c    = snd (ctyp w)
  ctxs = concat
         [ contextsFor gr sc c
         | sc <- startConcrCats gr
         ] 
  langName gr = concrLang gr ++ "> "
  absName gr = (reverse $ drop 3 $ reverse $ concrLang gr) ++ "> "

type Result = String

testFun :: Bool -> Grammar -> [Grammar] -> Cat -> Name -> Result
testFun debug gr trans startcat funname = unlines
  [ testTree debug gr trans startcat tree
  | tree <- treesUsingFun gr (lookupSymbol gr funname) ]


testTree :: Bool -> Grammar -> [Grammar] -> Cat -> Tree -> Result
testTree debug gr tgrs startcat t = unlines 
  [ ("### " ++ showConcrFun gr w) 
  , show t
  , if debug then unlines $ tabularPrint gr t else ""
  , unlines $ concat 
       [ [ ""
         , absName gr ++ show (ctx (App (hole c) []))
       --  , "  --> " ++ linearize gr (ctx (App (hole c) []))
         , langName gr ++ linearize gr (ctx t) 
         ] ++
         [ langName tgr ++ linearize tgr (ctx t) 
         | tgr <- tgrs ]
       | ctx <- ctxs
       ]
  , "" ]
 where
  w    = top t
  c    = snd (ctyp w)
  ctxs = concat
         [ contextsFor gr sc c
         | sc <- ccats gr startcat
         ] 

  langName gr = concrLang gr ++ "> "
  absName gr = (reverse $ drop 3 $ reverse $ concrLang gr) ++ "> "
--------------------------------------------------------------------------------

ccats :: Grammar -> Cat -> [ConcrCat]
ccats gr utt = [ cc
               | cc@(CC (Just cat) _) <- S.toList (nonEmptyCats gr)
               , cat == utt ]

treesUsingFun :: Grammar -> [Symbol] -> [Tree] 
treesUsingFun gr detCNs = 
  [ tree
    | detCN <- detCNs
    , let (dets_cns,np_209) = ctyp detCN -- :: ([ConcrCat],ConcrCat)
    , let bestArgs = case dets_cns of
                      [] -> [[]] 
                      xs -> bestTrees detCN gr dets_cns 
    , tree <- App detCN <$> bestArgs ]
  

--------------------------------------------------------------------------------

bestTrees :: Symbol -> Grammar -> [ConcrCat] -> [[Tree]]
bestTrees fun gr cats =
  bestExamples fun gr $ take 200 -- change this to something else if too slow
  [ featIthVec gr cats size i
  | all (`S.member` nonEmptyCats gr) cats
  , size <- [0..10]
  , let card = featCardVec gr cats size 
  , i <- [0..card-1]
  ]

--------------------------------------------------------------------------------

testsAsWellAs :: (Eq a, Eq b) => [a] -> [b] -> Bool
xs `testsAsWellAs` ys = go (xs `zip` ys)
 where
  go [] =
    True
    
  go ((x,y):xys) =
    and [ y' == y | (x',y') <- xys, x == x' ] &&
    go [ xy | xy@(x',_) <- xys, x /= x' ]


bestExamples :: Symbol -> Grammar -> [[Tree]] -> [[Tree]]
bestExamples fun gr vtrees = go [] vtrees_lins

 where
  syncategorematics = concatMap (lefts . concrSeqs gr) (seqs fun)
  vtrees_lins = [ (vtree, syncategorematics ++ concatMap (map snd . tabularLin gr) vtree) --this linearises all trees at once, no interplay with 
                 | vtree <- vtrees ] :: [([Tree],[String])]

  go cur []  = map fst cur
  go cur (vt@(ts,lins):vts) 
    | any (`testsAsWellAs` lins) (map snd cur) = go cur vts
    | otherwise = go' (vt:[ c | c@(_,clins) <- cur
                              , not (lins `testsAsWellAs` clins) ])
                      vts

  go' cur vts | enough cur = map fst cur
              | otherwise  = go cur vts

  enough :: [([Tree],[String])] -> Bool
  enough [(_,lins)] = all len1 (group $ sort lins) -- can stop earlier but let's not do that
  enough _          = False 

  len1 [x] = True
  len1 _   = False

--------------------------------------------------------------------------------

tabularPrint :: Grammar -> Tree -> [String]
tabularPrint gr t = 
    let cseqs = [ concatMap showCSeq cseq | cseq <- map (concrSeqs gr) (seqs $ top t) ]
        tablins = tabularLin gr t :: [(String,String)]
     in [ fieldname ++ ":\t" ++ lin ++ "\t" ++ s | ((fieldname,lin),s) <- zip tablins cseqs ]
 where
  showCSeq (Left tok) = " " ++ show tok ++ " "
  showCSeq (Right (i,j)) = " <" ++ show i ++ "," ++ show j ++ "> "

showConcrFun :: Grammar -> Symbol -> String
showConcrFun gr detCN = show detCN ++ " : " ++ 
                        concatMap (\x -> show x ++ " → ") dets_cns ++
                        show np_209
 where (dets_cns,np_209) = ctyp detCN 

lookupAll :: (Eq a) => [(a,b)] -> a -> [b]
lookupAll kvs key = [ v | (k,v) <- kvs, k==key ]

