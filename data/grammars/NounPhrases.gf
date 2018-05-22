abstract NounPhrases = {
  flags startcat = S ;
  cat
    S ; NP ; Adv ;               -- Categories formed from other categories
    CN ; Det ; Adj ; Prep ;      -- Lexical categories
  fun
    UttNP : NP -> S ;
    PredAdj : NP -> Adj -> S ;   -- e.g. "this house is blue"
    PredAdv : NP -> Adv -> S ;   -- e.g. "this house is on the hill"
    DetNP : Det -> NP ;          -- e.g. "this"
    DetCN : Det -> CN -> NP ;    -- e.g. "this house"
    PrepNP : Prep -> NP -> Adv ; -- e.g. "without this house"
    AdjCN : Adj -> CN -> CN ;    -- e.g. "small house"
    AdvCN : Adv -> CN -> CN ;    -- e.g. "house on the hill"

    house, hill, cake : CN ;
    a, theSg, thePl, this, these, your : Det ;
    good, small, blue, ready, tired : Adj ;
    on, from, without : Prep ;
}