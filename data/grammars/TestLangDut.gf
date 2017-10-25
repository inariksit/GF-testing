concrete TestLangDut of TestLang =
  NounDut,
  VerbDut, 
  AdjectiveDut,
  AdverbDut,
  NumeralDut,
  SentenceDut, 
  QuestionDut,
  RelativeDut,
  ConjunctionDut,
  PhraseDut,
  TextX,
  IdiomDut,
  TenseX,
  StructuralDut,
  LexiconDut
  ** open ParadigmsDut,SyntaxDut in {

lin
  --default_N : N ;
  --default_V : V ;
  --default_V2 : V2 ;
  --default_Det : Det ;

-- Some ad hoc constructs for defaultTree
  default_NP = mkNP beer_N ;

} ;