concrete NounPhrasesEst of NounPhrases = open Prelude in {
  flags startcat = TopNP ;
  lincat
    TopNP = SS ;
    NP = NounPhrase ;
    CN = CommonNoun ;
    Det = Determiner ;
    Adj = Adjective ;
    Prep = Preposition ;

  -- linref
  --   NP = linNP ;   

  
  lin
    topNP np = { s = linNP np } ;
    DetNP det = det ; -- this example is only relevant for Dutch
    DetCN det cn = { s = \\c => det.s ! c ++ cn.s ! det.n ! c } ;
    PrepNP pp np = { s = pp.s ++ np.s ! pp.c } ;		       
    AdjCN adj cn = { s = \\n,c => case <adj.at,c> of {
		       <(Invariable|Participle),_> => adj.s ! Sg ! Nom ++ cn.s ! n ! c ;
		       <_,Abe> => adj.s ! n ! Gen ++ cn.s ! n ! c ;
		       _   => adj.s ! n ! c   ++ cn.s ! n ! c }
		   } ;
    AdvCN adv cn = { s = \\n,c => cn.s ! n ! c ++ adv.s } ;

    house = cn "maja" "maja" "majade" ;
    hill = cn "mägi" "mäe" "mägede" ;
    a, theSg = det [] [] Sg ;
    thePl = det [] [] Pl ;
    this = det "see" "selle" Sg ;
    these = det "need" "nende" Pl ;
 --   your = det "sinu" "sinu" Sg ; -- this example doesn't really matter outside the Basque grammar
    good = adj "hea" "hea" "heade" Regular ;
    small = adj "väike" "väikese" "väikeste" Regular ;
    blue = adj "sinine" "sinise" "siniste" Regular ;
    ready = invarAdj "valmis" ;
    on = prep [] Ade False ;
    from = prep [] Ela False ;
    without = prep "ilma" Abe True ;

  param
    Case = Nom | Gen | Ade | Ela | Abe ;
    Number = Sg | Pl ;
    AdjType = Regular | Participle | Invariable ;


  oper
    NounPhrase : Type = { s : Case => Str } ;
    CommonNoun : Type = { s : Number => Case => Str } ;
    Determiner : Type = { s : Case => Str ; n : Number } ;
    Adjective  : Type = CommonNoun ** { at : AdjType } ;
    Preposition : Type = { s : Str ; c : Case ; genAgr : Bool } ;

    cases = table { Ade => "l" ; Ela => "st" ; Abe => "ta" ; _ => [] } ;
    
    cn : (_,_,_ : Str) -> CommonNoun = \mägi,mäe,mägede ->
      { s = \\n,c => case <n,c> of {
                       <Sg,Nom> => mägi ;
                       <Pl,Nom> => mäe + "d" ;
                       <Sg,x>   => mäe + cases ! x ;
                       <Pl,y>   => mägede + cases ! y }
      } ;

    adj : (_,_,_ : Str) -> AdjType -> Adjective = \väike,väikese,väikeste,atype ->
      cn väike väikese väikeste ** { at = atype } ;

    invarAdj : Str -> Adjective = \valmis ->
      { s = \\n,c => valmis ; at = Invariable } ;
    
    det : (_,_ : Str) -> Number -> Determiner = \need,nende,num ->
      { s = table { Nom => need ;
		    x   => nende + cases ! x } ;
	n = num
      } ;

    prep : Str -> Case -> Bool -> Preposition = \s,cas,gagr ->
      { s = s ; c = cas ; genAgr = gagr } ;
    
    linNP : NounPhrase -> Str = \np -> np.s ! Nom ;
}