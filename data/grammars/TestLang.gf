-- Like Lang, but with less words.

abstract TestLang = 
  Noun,
  Verb, 
  Adjective,
  Adverb,
  Numeral,
  Sentence, 
  Question,
  Relative,
  Conjunction,
  Phrase,
  Text,
  Structural, 
  --[AdA, AdV, Conj, VV, Det, V2, Pron, Subj, always_AdV, so_AdA, and_Conj, every_Det,
  --    he_Pron, i_Pron, she_Pron, they_Pron, we_Pron, youSg_Pron, youPl_Pron,
  --    if_Subj, can_VV, have_V2],
  Idiom,
  Tense,
--  Transfer,
  TestLexicon
  ** { flags startcat=Phr ; } ;
