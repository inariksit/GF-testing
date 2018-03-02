-- (c) 2009 Aarne Ranta under LGPL

abstract Foods = {
  flags startcat = Comment ;
  cat
    Comment ; Item ; Kind ; Quality ; Utt ; Question ;
  fun
    QUtt : Question -> Utt ;
    CUtt : Comment -> Utt ;
    Quest : Item -> Quality -> Question ;
    Pred : Item -> Quality -> Comment ;
    This, That, These, Those : Kind -> Item ;
    Mod : Quality -> Kind -> Kind ;
    Wine, Cheese, Fish, Pizza : Kind ;
    Very : Quality -> Quality ;
    Fresh, Warm, Italian, Good, 
      Expensive, Delicious, Vegan : Quality ;
}
