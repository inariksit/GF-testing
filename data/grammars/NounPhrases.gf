abstract NounPhrases = {
  flags startcat = TopNP ;
  cat
    TopNP ;
    NP ; Adv ;                   -- Categories formed from other categories
    CN ; Det ; Adj ; Prep ;      -- Lexical categories
  fun
    topNP : NP -> TopNP ;
    DetNP : Det -> NP ;          -- e.g. "this"
    DetCN : Det -> CN -> NP ;    -- e.g. "this house"
    PrepNP : Prep -> NP -> Adv ; -- e.g. "without this house"
    AdjCN : Adj -> CN -> CN ;    -- e.g. "small house"
    AdvCN : Adv -> CN -> CN ;    -- e.g. "house on the hill"

    house, hill : CN ;
    a, theSg, thePl, this, these, your : Det ;
    good, small, blue, ready : Adj ;
    on, from, without : Prep ;
}