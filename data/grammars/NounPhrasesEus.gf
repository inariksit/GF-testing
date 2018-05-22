concrete NounPhrasesEus of NounPhrases = open Prelude in {

  lincat
    S    = SS ;
    NP   = NounPhrase ;
    CN   = CommonNoun ;
    Adj  = Adjective ;
    Det  = Determiner ;
    Prep = Postposition ;

  lin
    PredAdj np adj = { s = linNP np ++ adj.s ! Def np.n Abs ++ copula ! np.n } ;
    PredAdv np adv = { s = linNP np ++ adv.s                ++ copula ! np.n } ; 
    UttNP np = { s = linNP np } ;
--    DetNP det = det ; -- only relevant for Dutch
    DetCN dt cn =
      { s = \\c => case dt.pl of {
	      Suffix => cn.adv ++ cn.s ! Def dt.n c ;
	      End    => cn.adv ++ cn.s ! Indef ++ dt.s ! c ;
	      Mid    => cn.adv ++ dt.s ! Abs ++ cn.s ! Def dt.n c } ;
        n = dt.n
      } ;

    PrepNP pp np = { s = np.s ! pp.c ++ pp.s } ;
    AdjCN adj cn = cn ** { s = \\nf => cn.s ! Indef ++ adj.s ! nf } ;
    AdvCN adv cn = cn ** { adv = adv.s } ;


    house = cn "etxe" ;
    hill = cn "muino" ;
    cake = cn "pastel" ;
    a = det "bat" "baten" "batetik" Sg End ;
    theSg = suffix Sg ;
    thePl = suffix Pl ;
--    this = det "hau" "honen" "honetatik" Sg End ;
    these = det "hauek" "hauen" "hauetatik" Pl End ;
    your = det "zure" "zure" "zure" Sg Mid ; -- definiteness comes from the suffix
    good = adj "ondo" ;
    small = adj "txiki" ;
    blue = adj "urdin" ;
    ready = adj "gertu" ;
    on = { s = "gainean" ; c = Gen } ;
    from = { s = [] ; c = Ela } ;
    without = { s = "gabe" ; c = Abs } ;

  linref
    NP = linNP ;

  param
    Case = Abs | Gen | Ela ; -- only those needed for this small grammar
    Number = Sg | Pl ;
    Placement = Mid | End | Suffix ;
    NForm = Indef | Def Number Case ;
    

  oper
    NounPhrase   : Type = { s : Case => Str ; n : Number } ;
    CommonNoun   : Type = { s : NForm => Str ; adv : Str } ;
    Adjective    : Type = { s : NForm => Str } ;
    Postposition : Type = { s : Str ; c : Case } ;
    Determiner   : Type = { s : Case => Str ; n : Number ; pl : Placement } ; 

    nf : Str -> (NForm => Str) = \etxe ->
      table { Indef      => etxe ;
	      Def Sg Abs => etxe + "a" ;
	      Def Pl Abs => etxe + "ak" ;
	      Def Sg Gen => etxe + "aren" ;
	      Def Pl Gen => etxe + "en" ;
	      Def Sg Ela => etxe + "tik" ;
	      Def Pl Ela => etxe + "etatik"
      } ;

    cn  : Str -> CommonNoun = \s -> { s = nf s ; adv = [] } ;
    adj : Str -> Adjective  = \s -> { s = nf s } ;

    suffix : Number -> Determiner = \n -> { s = \\_ => [] ; n = n ; pl = Suffix } ;
    det : (_,_,_ : Str) -> Number -> Placement -> Determiner = \hau,honen,honetatik,n,p ->
     { s = table { Abs => hau ; Gen => honen ; Ela => honetatik } ; n = n ; pl = p } ;

    copula : Number => Str = table { Sg => "da" ; Pl => "dira" } ;
    
    linNP : NounPhrase -> Str = \np -> np.s ! Abs ;
}