module Lib
    ( testTree
    , testFun
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

testFun :: Bool -> Grammar -> [Grammar] -> String -> IO ()
testFun debug gr trans funname = sequence_
  [ do print tree
       testTree debug gr trans tree
    | tree <- treesUsingFun gr (lookupSymbol gr funname) ]


testTree :: Bool -> Grammar -> [Grammar] -> Tree -> IO ()
testTree debug gr tgrs t =
  do putStrLn ("### " ++ showConcrFun gr w) 
     when debug $ mapM_ putStrLn (tabularPrint gr t)
     putStr $ unlines $ concat 
       [ [ ""
         , "- "     ++ show (ctx (App (hole c) []))
         , "  --> " ++ linearize gr (ctx (App (hole c) []))
         , "  --> " ++ linearize gr (ctx t) 
         ] ++
         [ "  --> " ++ linearize tgr (ctx t) 
         | tgr <- tgrs ]
       | ctx <- ctxs
       ]
     putStrLn ""
 where
  w    = top t
  c    = snd (ctyp w)
  ctxs = concat
         [ contextsFor gr sc c
         | sc <- starts
         ] 

  starts = ccats gr "S"

--------------------------------------------------------------------------------

ccats :: Grammar -> String -> [ConcrCat]
ccats gr cl = [ CC (Just cat) fid 
                 | (cat,start,end,_) <- concrCats gr
                 , cat == cl
                 , fid <- [start..end] ]

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
bestTrees fun gr cats = bestExamples fun gr $ take 200
  [ featIthVec gr cats size i
  | all (`S.member` nonEmptyCats gr) cats -- so we dare to have no bound on size!
  , size <- [0..] 
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