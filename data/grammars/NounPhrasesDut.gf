concrete NounPhrasesDut of NounPhrases = open Prelude in {

  lincat
    TopNP = SS ;
    NP    = NounPhrase ;
    CN    = CommonNoun ;
    Adj   = Adjective ;
    Det   = Determiner ;
    Prep  = Preposition ;

  lin
    topNP np = ss np.s ;
    DetNP dt = dt ** { s = dt.sp } ;
    DetCN dt cn = { s = dt.s ! cn.g ++ cn.s ! dt.defi ! dt.n ; contr = <False,[]> } ;
    PrepNP prep np =
      { s = case <prep.contr.p1,np.contr.p1> of {
	  <True,True> => glue np.contr.p2 prep.contr.p2 ;
	  _           => prep.s ++ np.s }
      } ;
    AdjCN adj cn = cn ** { s = \\d,n => case d of {
			     Indef => adj.s ! cn.g ++ cn.s ! d ! n ;
			     Def   => adj.s ! Utr  ++ cn.s ! d ! n }
			 } ;
    AdvCN adv cn = cn ** { s = \\d,n => cn.s ! d ! n ++ adv.s } ;

    house = { s = table { _ => table { Sg => "huis" ; Pl => "huizen" } } ; g = Neutr } ;
    hill = { s = table { _ => table { Sg => "heuvel" ; Pl => "heuvels" } } ; g = Utr } ;
    your = detSg "jouw" "jouw" "het jouwe" Def ;
    a = detSg "een" "een" "een" Indef ;
    theSg = detSg "de" "het" "het" Def ** { contr = <True,"er"> };
    thePl = detPl "de" "de" "de" Def ** { contr = <True,"er"> };
    this = detSg "deze" "dit" "dit" Def ** { contr = <True,"hier"> } ;
    these = detPl "deze" "deze" "deze" Def ** { contr = <True,"hier"> } ;
    good = adj "goed" "goede" ;
    small = adj "klein" "kleine" ;
    blue = adj "blauw" "blauwe" ;
--      ready : Adj ;
    on = prep "op" ;
    from = prep "van" ;
    without = noContr ** ss "zonder" ;

  param
    Number = Sg | Pl ;
    Gender = Neutr | Utr ;
    Defi = Def | Indef ;

  oper
    Contracts : Type = { contr : Bool*Str } ;
    Determiner : Type = Contracts ** { s : Gender => Str ; sp : Str ; n : Number ; defi : Defi } ;
    NounPhrase : Type = Contracts ** { s : Str } ;
    Preposition : Type = Contracts ** { s : Str } ;
    CommonNoun : Type = { s : Defi => Number => Str ; g : Gender } ;
    Adjective : Type = { s : Gender => Str } ;

    noContr : Contracts = { contr = <False,[]> } ;

    detSg : (_,_,_ : Str) -> Defi -> Determiner = \de,het,jouwe,d ->
      noContr ** { s = table { Utr => de ; Neutr => het } ;
		   sp = jouwe ; n = Sg ; defi = d } ;

    detPl : (_,_,_ : Str) -> Defi -> Determiner = \x,y,z,d ->
      detSg x y z d ** { n = Pl } ;

    adj : (_,_ : Str) -> Adjective = \goed,goede ->
      { s = table { Utr => goede ; Neutr => goed } } ;

    prep : Str -> Preposition = \op -> { s = op ; contr = <True,op> } ;
}