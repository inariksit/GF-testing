concrete FoodsSpaBuen of Foods = open Prelude,Predef in {

	lincat
		Comment = SS ;
		Item = NP ;
		Kind = CN ;
		Quality = AP ;

  lin 
    Pred i q = { s = i.s ++ copula ! i.a ++ q.s ! i.a.g ! i.a.n ! Predic } ;
    This = det "este" "esta" Sg ;
    That = det "ese" "esa" Sg ;
    These = det "estos" "estas" Pl ;
    Those = det "esos" "esas" Pl ;
    Mod q k = k ** 
      { s = \\n =>
        let attr = q.s ! k.g ! n ! Attrib 
        in case q.isPre of {
                      True  => attr ++ k.s ! n ;
                      False => k.s ! n ++ attr } 
      } ;
    Wine = cn "vino" Masc ;
    Cheese = cn "queso" Masc ;
    Fish = cn "pescado" Masc ;
    Pizza = cn "pizza" Fem ;
    Very q = q ** { s = \\g,n,p => "muy" ++ q.s ! g ! n ! p } ;
    Fresh = ap "fresco" False ;
    Delicious = ap "delicioso" False ;
    Italian = ap "italiano" False ;
    Vegan = ap "vegano" False ;
    Good = ap "bueno" True ;

param
  Number   = Sg | Pl ;
  Gender   = Masc | Fem ;
  Position = Attrib | Predic ;

oper 
  NP : Type = { s : Str ; a : Agr } ;
  CN : Type = { s : Number => Str ; g : Gender } ;
  AP : Type = { s : Gender => Number => Position => Str ; isPre : Bool } ;
  Agr : Type = { g : Gender ; n : Number } ;

  det : (_,_ : Str) -> Number -> CN -> NP = \ese,esa,num,kind ->
    let this = table { Masc => ese ; Fem => esa } ;
    in  { s = this ! kind.g ++ kind.s ! num ;
          a = { g = kind.g ; n = num } } ;
  
  ap : Str -> Bool -> AP = \bueno,isPre ->
    let buen = tk 1 bueno in 
    { s = \\g,n,p => case <g,n,p> of {
          <Masc,Sg,Attrib> => if_then_Str isPre buen bueno ;
          <Masc,Sg,Predic> => bueno ;
          <Masc,Pl> => bueno + "s" ;
          <Fem,Sg> => buen + "a" ;
          <Fem,Pl> => buen + "as" } ;
      isPre = isPre } ;

  cn : Str -> Gender -> CN = \pizza,fem -> 
    { s = table { Sg => pizza ; Pl => pizza + "s" } ;
      g = fem } ; 

  copula : Agr => Str = table {
    { n = Sg } => "es" ;
    { n = Pl } => "son" } ;

}