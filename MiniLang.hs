{-# LANGUAGE DeriveDataTypeable, KindSignatures, TypeFamilies, DataKinds, PolyKinds, TypeInType #-}

module MiniLang where

import Data.Data
import Test.Feat ( Enumerable, Enumerate
                 , enumerate, consts, unary, funcurry 
                 , values )
import PGF hiding (Tree)
import qualified PGF


----------------------------------------------------

enumBounded :: (Enum a, Bounded a) => Enumerate a
enumBounded = consts $ map pure [minBound..maxBound]


type family
  HasArg a (cons :: a -> b) :: Bool 

{-
This works in theory, when using :k in GHCI:

λ> :k  HasArg GAP GAdjCN 
HasArg GAP GAdjCN :: Bool      -- GAP is indeed an argument of CAdjCN, we get a Bool


λ> :k  HasArg GAP GMassNP      -- GAP is not an argument of GMassNP, we get an error

<interactive>:1:12: error:
    • Expected kind ‘GAP -> GNP’, but ‘'GMassNP’ has kind ‘GCN -> GNP’
    • In the second argument of ‘HasArg’, namely ‘GMassNP’
      In the type ‘HasArg GAP GMassNP’
-}


----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> PGF.Tree
  fg :: PGF.Tree -> a
  

newtype GString = GString String  deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int  deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double  deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkDouble x
  fg t =
    case unDouble t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GA =
   Gbad_A 
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GA where enumerate = enumBounded

data GAP = GPositA GA 
  deriving (Show,Eq,Data,Typeable)
instance Enumerable GAP where
  enumerate = unary GPositA

data GAdv =
   GPrepNP GPrep GNP 
 | Galready_Adv 
 | Gnow_Adv 
  deriving (Show,Eq,Data,Typeable)
instance Enumerable GAdv where
  enumerate = consts ( unary (funcurry GPrepNP):
                       map pure [Galready_Adv,Gnow_Adv] 
                     )



data GCN =
   GAdjCN GAP GCN 
 | GUseN GN 
  deriving (Show,Eq,Data,Typeable)
instance Enumerable GCN where
  enumerate = consts [ unary (funcurry GAdjCN)
                     , unary GUseN
                     ]

data GCl = GPredVP GNP GVP 
  deriving (Show,Eq,Data,Typeable)
instance Enumerable GCl where
  enumerate = consts [ unary (funcurry GPredVP) ]

data GConj =
   Gand_Conj 
 | Gor_Conj 
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GConj where enumerate = enumBounded

data GDet =
   GthePl_Det 
 | Gthe_Det 
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GDet where enumerate = enumBounded

data GN =
   Ganimal_N 
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GN where enumerate = enumBounded

data GNP =
   GDetCN GDet GCN 
 | GMassNP GCN 
 | GUsePN GPN 
 | GUsePron GPron 
  deriving (Show,Eq,Data,Typeable)
instance Enumerable GNP where
  enumerate = consts [ unary (funcurry GDetCN)
                     , unary GMassNP
                     , unary GUsePN
                     , unary GUsePron
                     ]

data GPN =
   Gjohn_PN 
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GPN where enumerate = enumBounded

data GPol =
   GPNeg 
 | GPPos 
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GPol where enumerate = enumBounded

data GPrep =
   Gin_Prep 
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GPrep where enumerate = enumBounded  

data GPron =
   Gi_Pron 
 | Gshe_Pron 
 | GyouSg_Pron
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GPron where enumerate = enumBounded  

data GS =
   GCoordS GConj GS GS 
 | GUsePresCl GPol GCl 
  deriving (Show,Eq,Data,Typeable)
instance Enumerable GS where
  enumerate = consts [ unary (funcurry GUsePresCl)
                     , unary (funcurry (funcurry GCoordS))
                     ]

data GUtt =
   GUttNP GNP 
 | GUttS GS 
  deriving (Show,Eq,Data,Typeable)
instance Enumerable GUtt where
  enumerate = consts [ unary GUttNP
                     , unary GUttS
                     ]

data GV =
  Gsleep_V 
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GV where enumerate = enumBounded  

data GV2 =
  Gsee_V2 
  deriving (Show,Eq,Data,Typeable,Enum,Bounded)
instance Enumerable GV2 where enumerate = enumBounded  

data GVP =
   GAdvVP GVP GAdv 
 | GComplV2 GV2 GNP 
 | GUseAP GAP 
 | GUseV GV 
  deriving (Show,Eq,Data,Typeable)
instance Enumerable GVP where
  enumerate = consts [ unary GUseAP
                     , unary GUseV
                     , unary (funcurry GAdvVP)
                     , unary (funcurry GComplV2)
                     ]

instance Gf GA where
  gf Gbad_A = mkApp (mkCId "bad_A") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "bad_A" -> Gbad_A 
      _ -> error ("no A " ++ show t)

instance Gf GAP where
  gf (GPositA x1) = mkApp (mkCId "PositA") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "PositA" -> GPositA (fg x1)


      _ -> error ("no AP " ++ show t)

instance Gf GAdv where
  gf (GPrepNP x1 x2) = mkApp (mkCId "PrepNP") [gf x1, gf x2]
  gf Galready_Adv = mkApp (mkCId "already_Adv") []
  gf Gnow_Adv = mkApp (mkCId "now_Adv") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "PrepNP" -> GPrepNP (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "already_Adv" -> Galready_Adv 
      Just (i,[]) | i == mkCId "now_Adv" -> Gnow_Adv 


      _ -> error ("no Adv " ++ show t)

instance Gf GCN where
  gf (GAdjCN x1 x2) = mkApp (mkCId "AdjCN") [gf x1, gf x2]
  gf (GUseN x1) = mkApp (mkCId "UseN") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjCN" -> GAdjCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UseN" -> GUseN (fg x1)


      _ -> error ("no CN " ++ show t)

instance Gf GCl where
  gf (GPredVP x1 x2) = mkApp (mkCId "PredVP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "PredVP" -> GPredVP (fg x1) (fg x2)


      _ -> error ("no Cl " ++ show t)

instance Gf GConj where
  gf Gand_Conj = mkApp (mkCId "and_Conj") []
  gf Gor_Conj = mkApp (mkCId "or_Conj") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "and_Conj" -> Gand_Conj 
      Just (i,[]) | i == mkCId "or_Conj" -> Gor_Conj 


      _ -> error ("no Conj " ++ show t)

instance Gf GDet where
  gf GthePl_Det = mkApp (mkCId "thePl_Det") []
  gf Gthe_Det = mkApp (mkCId "the_Det") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "thePl_Det" -> GthePl_Det 
      Just (i,[]) | i == mkCId "the_Det" -> Gthe_Det 


      _ -> error ("no Det " ++ show t)

instance Gf GN where
  gf Ganimal_N = mkApp (mkCId "animal_N") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "animal_N" -> Ganimal_N 
      _ -> error ("no N " ++ show t)

instance Gf GNP where
  gf (GDetCN x1 x2) = mkApp (mkCId "DetCN") [gf x1, gf x2]
  gf (GMassNP x1) = mkApp (mkCId "MassNP") [gf x1]
  gf (GUsePN x1) = mkApp (mkCId "UsePN") [gf x1]
  gf (GUsePron x1) = mkApp (mkCId "UsePron") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "DetCN" -> GDetCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "MassNP" -> GMassNP (fg x1)
      Just (i,[x1]) | i == mkCId "UsePN" -> GUsePN (fg x1)
      Just (i,[x1]) | i == mkCId "UsePron" -> GUsePron (fg x1)


      _ -> error ("no NP " ++ show t)

instance Gf GPN where
  gf Gjohn_PN = mkApp (mkCId "john_PN") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "john_PN" -> Gjohn_PN 
      _ -> error ("no PN " ++ show t)

instance Gf GPol where
  gf GPNeg = mkApp (mkCId "PNeg") []
  gf GPPos = mkApp (mkCId "PPos") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "PNeg" -> GPNeg 
      Just (i,[]) | i == mkCId "PPos" -> GPPos 


      _ -> error ("no Pol " ++ show t)

instance Gf GPrep where
  gf Gin_Prep = mkApp (mkCId "in_Prep") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "in_Prep" -> Gin_Prep 
      _ -> error ("no Prep " ++ show t)

instance Gf GPron where
  gf Gi_Pron = mkApp (mkCId "i_Pron") []
  gf Gshe_Pron = mkApp (mkCId "she_Pron") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "i_Pron" -> Gi_Pron 
      Just (i,[]) | i == mkCId "she_Pron" -> Gshe_Pron 
      _ -> error ("no Pron " ++ show t)

instance Gf GS where
  gf (GCoordS x1 x2 x3) = mkApp (mkCId "CoordS") [gf x1, gf x2, gf x3]
  gf (GUsePresCl x1 x2) = mkApp (mkCId "UsePresCl") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "CoordS" -> GCoordS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "UsePresCl" -> GUsePresCl (fg x1) (fg x2)


      _ -> error ("no S " ++ show t)

instance Gf GUtt where
  gf (GUttNP x1) = mkApp (mkCId "UttNP") [gf x1]
  gf (GUttS x1) = mkApp (mkCId "UttS") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "UttNP" -> GUttNP (fg x1)
      Just (i,[x1]) | i == mkCId "UttS" -> GUttS (fg x1)


      _ -> error ("no Utt " ++ show t)

instance Gf GV where
  gf Gsleep_V = mkApp (mkCId "sleep_V") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "sleep_V" -> Gsleep_V 
      _ -> error ("no V " ++ show t)

instance Gf GV2 where
  gf Gsee_V2 = mkApp (mkCId "see_V2") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "see_V2" -> Gsee_V2 
      _ -> error ("no V2 " ++ show t)

instance Gf GVP where
  gf (GAdvVP x1 x2) = mkApp (mkCId "AdvVP") [gf x1, gf x2]
  gf (GComplV2 x1 x2) = mkApp (mkCId "ComplV2") [gf x1, gf x2]
  gf (GUseAP x1) = mkApp (mkCId "UseAP") [gf x1]
  gf (GUseV x1) = mkApp (mkCId "UseV") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvVP" -> GAdvVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplV2" -> GComplV2 (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UseAP" -> GUseAP (fg x1)
      Just (i,[x1]) | i == mkCId "UseV" -> GUseV (fg x1)


      _ -> error ("no VP " ++ show t)


