module Lib
    ( assertLin
    , testHole
    , testWord
    , treesUsingFun
    , showConcrFun
    , startCCats
    ) where

import GrammarC
import Paths_GF_testing
import Control.Monad ( when )
import Data.List
import Data.Either
import qualified Data.Set as S
import Data.Maybe
import Data.Tuple ( swap )
import Debug.Trace


assertLin :: Bool -> Grammar -> (Tree,ConcrCat,String) -> IO ()
assertLin debug gr (tree,ccat,funname) = do
  putStrLn $ "\n" ++ funname 
  putStrLn $ [ '-' | c <- funname ] ++ "\n"
  when debug $ do
    putStrLn $ intercalate "\n" (tabularPrint gr tree)
    putStrLn "--------------------\n"
  
  let ctxs = concat [ contextsFor gr st ccat 
                     | st <- ccats gr "S" ] :: [Tree -> Tree]

  let trees = map ($ tree) ctxs

  let holes = map ($ (App (hole ccat) [])) ctxs
  
  putStrLn $ show tree ++ " : " ++ show ccat
  putStrLn $ linearize gr tree

  putStrLn "\nNow showing that tree in context:"

  sequence_ [ do putStrLn (linearize gr hole)
                 putStrLn (linearize gr tree)
              | (tree,hole) <- zip trees holes ]  
 where
  tabularPrint :: Grammar -> Tree -> [String]
  tabularPrint gr t = 
    let cseqs = [ concatMap showCSeq cseq | cseq <- map (concrSeqs gr) (seqs $ top t) ]
        tablins = tabularLin gr t :: [(String,String)]
     in [ fieldname ++ ":\t" ++ lin ++ "\t" ++ s | ((fieldname,lin),s) <- zip tablins cseqs ]

  showCSeq (Left tok) = " " ++ show tok ++ " "
  showCSeq (Right (i,j)) = " <" ++ show i ++ "," ++ show j ++ "> "


testHole :: Grammar -> ConcrCat -> IO ()
testHole gr cn_3 = do
  putStrLn "---"
  putStrLn $ show cn_3 ++ "-shaped hole in the start category:"
      
  let bestCtxs = [ map ($ App (hole cn_3) []) (contextsFor gr startcat cn_3)
                   | startcat <- startCCats gr ] 

  mapM_ putStrLn $ 
       [ show t ++ "\n\t" ++ linearize gr t ++ "\n"
          | ts <- bestCtxs, t <- ts ]

testWord :: Grammar -> Symbol -> IO ()
testWord gr w =
  do putStrLn ("*** WORD: " ++ showConcrFun gr w ++ " " ++ show (tabularLin gr (App w [])))
     putStr $ unlines $ concat $
       [ [ ""
         , "- "     ++ show (ctx (App (hole c) []))
         , "  --> " ++ linearize gr (ctx (App (hole c) []))
         , "  --> " ++ linearize gr (ctx (App w []))
         ] 
         {- ++
         [ "  " ++ s
         | f <- nub $ funs (ctx (App w []))
         , s <- analFun f
         ] -}
       | ctx <- ctxs
       ]
     putStrLn ""
 where
  c    = snd (ctyp w)
  ctxs = concat
         [ contextsFor gr sc c
         | sc <- starts
         ] 

  starts = ccats gr "S"
  
  funs (App f ts) = f : concatMap funs ts

  analFun f =
    [ showConcrFun gr f' ++ if f == f' then " <=" else "" 
    | f' <- symbols gr
    , show f == show f'
    ]

lookupSymbols :: Grammar -> String -> [Symbol]
lookupSymbols gr str = 
  lookupAll (symb2table <$> symbols gr) (mkName str)
 where
  symb2table s@(Symbol nm _ _ _) = (nm,s)

ccats :: Grammar -> String -> [ConcrCat]
ccats gr cl = [ CC (Just cat) fid 
                 | (cat,start,end,_) <- concrCats gr
                 , cat == cl
                 , fid <- [start..end] ]

startCCats :: Grammar -> [ConcrCat]
startCCats gr = ccats gr (startCat gr)


--------------------------------------------------------------------------------

treesUsingFun :: Grammar -> String -> [(Tree,ConcrCat,String)] --last 2 for debug
treesUsingFun gr funname = 
  [ (tree,np_209,concrFunStr)
    | detCN <- lookupSymbols gr funname
    , let (dets_cns,np_209) = ctyp detCN -- :: ([ConcrCat],ConcrCat)
    , tree <- App detCN <$> bestTrees detCN gr dets_cns
    , let concrFunStr = showConcrFun gr detCN ]
  
showConcrFun :: Grammar -> Symbol -> String
showConcrFun gr detCN = show detCN ++ " : " ++ 
                        intercalate " → " (map show dets_cns) ++
--                        show (coerce gr <$> dets_cns) ++
                        " → " ++ show np_209
 where (dets_cns,np_209) = ctyp detCN 
--------------------------------------------------------------------------------

bestTrees :: Symbol -> Grammar -> [ConcrCat] -> [[Tree]]
bestTrees fun gr cats = bestExamples fun gr $ take 10000
  [ featIthVec gr cats size i
    | size <- [1..5] 
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

hasArg :: Symbol -> Bool
hasArg s = case s of
  Symbol _ _ ([], _) _ -> False
  _                  -> True

lookupAll :: (Eq a) => [(a,b)] -> a -> [b]
lookupAll kvs key = [ v | (k,v) <- kvs, k==key ]

coerce :: Grammar -> ConcrCat -> [ConcrCat]
coerce gr ccat = case lookupAll (coercions gr) ccat of
                    [] -> [ccat]
                    xs -> xs

uncoerce :: Grammar -> ConcrCat -> [ConcrCat]
uncoerce gr = lookupAll (map swap $ coercions gr)

isSubtree :: Tree -> Tree -> Bool
isSubtree t1 t2 = t1 `elem` args t2

norepeat :: Tree -> Bool
norepeat t = let tops = collapse t in nub tops == tops

collapse :: Tree -> [Symbol]
collapse (App tp as) = tp : concatMap collapse as


