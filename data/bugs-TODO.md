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

TODO: should make sure that VPSlash created by VPSlashPrep doesn't try to put things in n0 or n2 fields.

***

This has bugs in both old and new version:

```
- UseCl (TTAnt TPres AAnter) PNeg (PredVP (ConjNP either7or_DConj (BaseNP something_NP (UsePron i_Pron))) (ComplSlash (Slash2V3 add_V3 something_NP) (UsePron it_Pron)))
  --> ofwel iets of ik hebben eraan iets niet toe toegevoegd
  ==> ofwel iets of ik hebben niet eraan iets toe toegevoegd
  ```
