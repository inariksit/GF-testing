Dutch:

```Lang> p -cat=QCl "which is the best" | l -treebank -lang=Dut
Lang: QuestIComp (CompIP (IdetIP (IdetQuant which_IQuant NumSg))) (DetNP (DetQuantOrd DefArt NumSg (OrdSuperl good_A)))
LangDut: welk is die beste
Lang: QuestIComp (CompIP (IdetIP (IdetQuant which_IQuant NumPl))) (DetNP (DetQuantOrd DefArt NumSg (OrdSuperl good_A)))
LangDut: welke is die beste
Lang: QuestVP (IdetIP (IdetQuant which_IQuant NumSg)) (UseComp (CompNP (DetNP (DetQuantOrd DefArt NumSg (OrdSuperl good_A)))))
LangDut: welk is die beste
Lang: QuestVP (IdetIP (IdetQuant which_IQuant NumSg)) (UseComp (CompNP (DetNP (DetQuantOrd DefArt NumPl (OrdSuperl good_A)))))
LangDut: welk is die beste
```

How to fix?

***

### Weirdness with VPSlashPrep

New grammar: 

```
Lang> p -cat=S "ik ben altijd niet zonder het"
UseCl … (AdVVP always_AdV (UseComp (CompAdv (PrepNP without_Prep (UsePron it_Pron)))))
UseCl … (AdVVP always_AdV (AdvVP UseCopula (PrepNP without_Prep (UsePron it_Pron))))
UseCl … (AdvVP (AdVVP always_AdV UseCopula) (PrepNP without_Prep (UsePron it_Pron)))

Lang> p -cat=S "ik ben zonder het altijd niet"
UseCl … (AdVVP always_AdV (ComplSlash (VPSlashPrep UseCopula without_Prep) (UsePron it_Pron)))
UseCl … (ComplSlash (VPSlashPrep (AdVVP always_AdV UseCopula) without_Prep) (UsePron it_Pron))
```

Old grammar:

```
TestLang> p -cat=S "ik ben altijd niet zonder het"
UseCl … (AdVVP always_AdV (UseComp (CompAdv (PrepNP without_Prep (UsePron it_Pron)))))
UseCl … (AdVVP always_AdV (ComplSlash (VPSlashPrep UseCopula without_Prep) (UsePron it_Pron)))
UseCl … (ComplSlash (VPSlashPrep (AdVVP always_AdV UseCopula) without_Prep) (UsePron it_Pron))
UseCl … (AdVVP always_AdV (AdvVP UseCopula (PrepNP without_Prep (UsePron it_Pron))))
UseCl … (AdvVP (AdVVP always_AdV UseCopula) (PrepNP without_Prep (UsePron it_Pron)))
```

and no parses for "ik ben zonder het altijd niet".

TODO: should make sure that VPSlash created by VPSlashPrep doesn't try to put things in n0 or n2 fields?

```
- UseCl (TTAnt TFut ASimul) PNeg (PredVP (UsePron i_Pron) VP_399)
 new> ik zal ermee niet draaien
 old> ik zal niet ermee draaien

- UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron i_Pron) VP_399)
 new> ik draai ermee niet
 new> ik draai niet ermee
```

***

This has bugs in both old and new version:

```
- UseCl (TTAnt TPres AAnter) PNeg (PredVP (ConjNP either7or_DConj (BaseNP something_NP (UsePron i_Pron))) (ComplSlash (Slash2V3 add_V3 something_NP) (UsePron it_Pron)))
  new> ofwel iets of ik hebben eraan iets niet toe toegevoegd
  old> ofwel iets of ik hebben niet eraan iets toe toegevoegd
  ```

Which one is correct, or are both wrong?

```
- UseCl (TTAnt TFut ASimul) PPos (PredVP (DetNP somePl_Det) VP_401)
  new> sommigen zullen zichzelf zonder me af vragen wie er is
  old> sommigen zullen zonder me zich af vragen wie er is
```


*** 

Reflexive + number of the NP with ConjNP:

```
UseCl (TTAnt TPast ASimul) PPos (PredVP (ConjNP either7or_DConj (BaseNP something_NP (UsePron i_Pron))) VP_387)
TestLangDut> ofwel iets of ik smeekten het ons te schrijven
TestLangEng> either something or I begged it to write myself
```

What happens here with the reflexive pronoun?

```
UseCl (TTAnt TPast ASimul) PPos (PredVP (UsePron we_Pron) VP_387)
TestLangDut> we smeekten het zich te schrijven
TestLangEng> we begged it to write ourselves
```

***

```
TestLang> UseCl (TTAnt TPres ASimul) PPos (ExistNP (MassNP (AdjCN (ComplA2 A2_2 (UsePron it_Pron)) (UseN worm_N))))
TestLangDut> er is getrouwde ermee worm
TestLangEng> there is worm married to it

TestLang> UseCl (TTAnt TPres ASimul) PPos (ExistNP (MassNP (AdjCN (ComplA2 A2_2 something_NP) (UseN worm_N))))
TestLangDut> er is gemakkelijke voor iets worm
TestLangEng> there is worm easy for something
```

split between heavy and light APs?

