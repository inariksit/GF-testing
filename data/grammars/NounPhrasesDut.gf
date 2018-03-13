concrete NounPhrasesDut of NounPhrases = open Prelude, (L=LangDut), ResDut in {

  lincat
    TopNP = SS ;
    NP    = L.NP ;
    Adv   = L.Adv ;
    CN    = L.CN ;
    Adj   = Adjective ;
    Det   = L.Det ;
    Prep  = L.Prep ;

  lin
    topNP np = ss (np.s ! NPNom) ;
    DetNP  = L.DetNP ;
    DetCN  = L.DetCN ;
    PrepNP = L.PrepNP ;
    AdjCN a cn = L.AdjCN (L.PositA a) cn ;
    AdvCN a cn = L.AdvCN cn a ;

    house = L.UseN L.house_N ;
    hill = L.UseN L.hill_N ;
    your = detSg (L.PossPron L.youSg_Pron) ;
    a = detSg L.IndefArt ;
    theSg = detSg L.DefArt ;
    thePl = detPl L.DefArt ;
    this = detSg L.this_Quant ;
    these = detPl L.this_Quant ;    
    good = L.good_A ;
    small = L.small_A ;
    blue = L.blue_A ;
--      ready : Adj ;
    on = L.on_Prep ;
    from = L.from_Prep ;
    without = L.without_Prep ;

  oper
    detSg : L.Quant -> L.Det = \q -> L.DetQuant q L.NumSg ;
    detPl : L.Quant -> L.Det = \q -> L.DetQuant q L.NumPl ;
}