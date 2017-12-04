-- Like Lang, but with less words.

abstract TestLang = Lang
--  Noun,
--  Verb,
--  Cat, 
--  Adjective,
--  Adverb,
--  Numeral,
--  Sentence, 
--  Question,
--  Relative,
--  Conjunction,
--  Phrase,
--  Text,
--  Structural, 
----  ----[AdA, AdV, Conj, VV, Det, V2, Pron, Subj, always_AdV, so_AdA, and_Conj, every_Det,
----  ----    he_Pron, i_Pron, she_Pron, they_Pron, we_Pron, youSg_Pron, youPl_Pron,
----  ----    if_Subj, can_VV, have_V2],
--  Idiom,
--  Tense,
------  Transfer,
------  TestLexicon
--  Lexicon
  ** { 

flags startcat=Phr ; 

--fun

  --default_N : N ;
  --default_V : V ;
  --default_V2 : V2 ;
  --default_Det : Det ;
-- Some ad hoc constructs for defaultTree
  --default_NP : NP ;


} ;
