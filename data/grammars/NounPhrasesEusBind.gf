concrete NounPhrasesEusBind of NounPhrases = open Prelude in {

  lincat
    NP = NounPhrase ;
    CN = CommonNoun ;
    Adj = Adjective ;
    Det = Determiner ;
    Prep = Postposition ;

  lin
--    DetNP : Det -> NP ;          -- e.g. "this"
    DetCN dt cn =
      { s = case dt.pl of {
	      End    => cn.adv ++ cn.s ; 
	      Mid    => cn.adv ++ dt.s ++ cn.s } ;
	n = dt.n ;
	det = case <dt.n,dt.pl> of { 
	         <Sg,Mid> => theSg.s ;
		 <Pl,Mid> => thePl.s ;
		 _        => dt.s } ;
      } ;

    PrepNP pp np = { s = np.s ++ pp.s ! np.n } ; -- don't use np.det, but the case from the preposition
    AdjCN adj cn = cn ** { s = cn.s ++ adj.s } ;
    AdvCN adv cn = cn ** { adv = adv.s } ;

    house = cn "etxe" ;
    hill = cn "muino" ;
    cake = cn "pastel" ;
    a = det "bat" Sg End ;
    theSg = det (BIND ++ "a") Sg End ;
    thePl = det (BIND ++ "ak") Pl End ;
--    this = det "hau" Sg End ;
--    these = det "hauek" "hauen" "hauetatik" Pl End ;
    your = det "zure" Sg Mid ;
    good = adj "ondo" ;
    small = adj "txiki" ;
    blue = adj "urdin" ;
    on = pp Gen "gainean" ;
    from = pp Ela [] ;
    without = pp Abs "gabe" ;

  linref
    NP = linNP ;

  param
    Case = Abs | Ela | Gen ;
    Number = Sg | Pl ;
    Placement = Mid | End ;

  oper
    NounPhrase   : Type = { s : Str ; det : Str ; n : Number } ;
    CommonNoun   : Type = { s : Str ; adv : Str } ;
    Adjective    : Type = { s : Str } ;
    Postposition : Type = { s : Number => Str } ;
    Determiner   : Type = { s : Str ; n : Number ; pl : Placement } ;

    pp : Case -> Str -> Postposition = \c,s ->
      { s = table { Sg => BIND ++ 
	 	      case c of {
			Abs => "a" ;
			Gen => "aren" ;
			Ela => "tik" } ++ s ;
	           Pl => BIND ++ 
		     case c of {
		       Abs => "ak" ;
		       Gen => "en" ;
		       Ela => "etatik" } ++ s } 
      } ;

 -- table { Sg => BIND ++ "a" ++ s ;
 -- 		       Pl => BIND ++ "ak" ++ s } ;
 -- 	Gen => table { Sg => BIND ++ "aren" ++ s ;
 -- 		       Pl => BIND ++ "en" ++ s } ;
 -- 	Ela => table { Sg => BIND ++ "tik" ++ s ;
 -- 		       Pl => BIND ++ "etatik" ++ s } ;

    cn  : Str -> CommonNoun = \s -> { s = s ; adv = [] } ;
    adj : Str -> Adjective  = \s -> { s = s } ;

    det : Str -> Number -> Placement -> Determiner = \hau,n,p ->
     { s = hau ; n = n ; pl = p } ;

    linNP : NounPhrase -> Str = \np -> np.s ++ np.det ;
}