## Basque

Creating test cases for `RelNP`, we get the following output.
```
* RelNP everything_NP (UseRCl (TTAnt TPres ASimul) PPos (RelCl (ExistNP everything_NP)))
…
** 6) ImpPl1 (UseComp (CompNP NP_13246))
TestLangEus> denaek da denaek
TestLangEng> let's be everything, such that there is everything

** 7) ImpPl1 (UseComp (CompAdv (PrepNP by8agent_Prep NP_13246)))
TestLangEus> denaek da denaek
TestLangEng> let's be by everything, such that there is everything
```

(Why does the tool generate both of these sentences in Basque, even though they linearise to the same? Because the forms come from different fields (which is wrong)?)


## Dutch

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

Is this correct? If not, what is correct?

***

### ReflVP and choice of agreement

Consider the following sentences.

* "I help him turn with himself."
* "I help him turn with myself."

Here is AST 1:

```
PredVP (UsePron i_Pron) 
       (ReflVP 
           (VPSlashPrep 
               (ComplSlash 
                  (SlashV2V help_V2V 
                           (UseV turn_V)
                  )
                  (UsePron he_Pron)
               ) 
               with_Prep
            )
        )
```

We have the following steps: 
* "help _ turn" (SlashV2V)
* "help `he_Pron` turn" (ComplSlash)
* "help him turn `with`" (VPSlashPrep)
* "help him turn with `refl`" (ReflVP)
* "`i_Pron` help him turn with `myself`" (PredVP)

Seems like a reasonable This is also the behaviour I get in English, German and Dutch when I do the following command (changed help to beg, because that's in the RGL lexicon):

* `TestLang> gt PredVP (UsePron ?) (ReflVP ( VPSlashPrep ( ComplSlash (SlashV2V beg_V2V (UseV turn_V)) (UsePron he_Pron)) with_Prep)) | l`


```
I beg with myself him to turn
it begs with itself him to turn
she begs with herself him to turn
...
```

Now, let's stack the constructors in different order. AST 2:

```
PredVP (UsePron i_Pron) 
       (ComplSlash 
           (SlashV2V 
               help_V2V 
               (ReflVP 
                   (VPSlashPrep 
                       (UseV turn_V) 
                       with_Prep
                   )
                )
            )
           (UsePron he_Pron)
       )
```

Now the steps, from innermost, are the following:

* "turn with _" (VPSlashPrep)
* "turn with `refl` (ReflVP)
* "help _ turn with `refl`" (SlashV2V)
* "help `he_Pron` turn with `refl`" (ComplSlash)
* "I help him turn with ???" (PredVP)

I think this sentence should linearise to "I help him turn with himself". However, trying this out with different pronouns gives the following in English:

```
I help him turn with myself
it helps him turn with itself
she helps him turn with herself
```

It's the same in German, but as I described for Dutch.

```
TestLang> gt PredVP (UsePron ?) (ComplSlash (SlashV2V help_V2V (ReflVP (VPSlashPrep (UseV turn_V) with_Prep))) (UsePron he_Pron)) | l -treebank
TestLang: PredVP (UsePron he_Pron) (ComplSlash (SlashV2V help_V2V (ReflVP (VPSlashPrep (UseV turn_V) with_Prep))) (UsePron he_Pron))
TestLangDut: hij helpt hem met zichzelf te draaien
TestLangEng: he helps him turn with himself
TestLangGer: er hilft ihn mit sich zu drehen
TestLang: PredVP (UsePron i_Pron) (ComplSlash (SlashV2V help_V2V (ReflVP (VPSlashPrep (UseV turn_V) with_Prep))) (UsePron he_Pron))
TestLangDut: ik help hem met zichzelf te draaien
TestLangEng: I help him turn with myself
TestLangGer: ich helfe ihn mit mir zu drehen
TestLang: PredVP (UsePron it_Pron) (ComplSlash (SlashV2V help_V2V (ReflVP (VPSlashPrep (UseV turn_V) with_Prep))) (UsePron he_Pron))
TestLangDut: het helpt hem met zichzelf te draaien
TestLangEng: it helps him turn with itself
TestLangGer: es hilft ihn mit sich zu drehen
TestLang: PredVP (UsePron she_Pron) (ComplSlash (SlashV2V help_V2V (ReflVP (VPSlashPrep (UseV turn_V) with_Prep))) (UsePron he_Pron))
TestLangDut: zij helpt hem met zichzelf te draaien
TestLangEng: she helps him turn with herself
TestLangGer: sie hilft ihn mit sich zu drehen
TestLang: PredVP (UsePron they_Pron) (ComplSlash (SlashV2V help_V2V (ReflVP (VPSlashPrep (UseV turn_V) with_Prep))) (UsePron he_Pron))
TestLangDut: zij helpen hem met zichzelf te draaien
TestLangEng: they help him turn with themselves
TestLangGer: sie helfen ihn mit sich zu drehen
TestLang: PredVP (UsePron we_Pron) (ComplSlash (SlashV2V help_V2V (ReflVP (VPSlashPrep (UseV turn_V) with_Prep))) (UsePron he_Pron))
TestLangDut: wij helpen hem met zichzelf te draaien
TestLangEng: we help him turn with ourselves
TestLangGer: wir helfen ihn mit uns zu drehen
```


### Weirdness with VPSlashPrep

```
* VPSlashPrep (ReflVP (SlashV2V help_V2V (ProgrVP (UseV turn_V)))) without_Prep

** UseCl (TTAnt TFut ASimul) PPos (GenericCl (ComplSlash ∅ (ConjNP or_Conj (BaseNP (UsePron i_Pron) (UsePron youSg_Pron)))))
TestLangDut> men zal zonder mij of jou helpen jezelf aan het draaien te zijn
TestLangDut-OLD> men zal zonder mij of jou helpen mijzelf aan het draaien te zijn
TestLangEng> one will help itself be turning without me or you
```

ComplSlash is choosing the agreement of help oneself to turn based on "me or you", but it should be GenericCl that chooses the argument.

Would probably help to change VPSlashPrep so that it leaves an open slot for an Adv, not an actual object.

***

TODO: should make sure that VPSlash created by VPSlashPrep doesn't try to put things in n0 or n2 fields?

```
- UseCl (TTAnt TFut ASimul) PNeg (PredVP (UsePron i_Pron) VP_399)
 new> ik zal ermee niet draaien
 old> ik zal niet ermee draaien

- UseCl (TTAnt TPres ASimul) PNeg (PredVP (UsePron i_Pron) VP_399)
 new> ik draai ermee niet
 old> ik draai niet ermee
```

***


Which one is correct, or are both wrong?

```
- UseCl (TTAnt TFut ASimul) PPos (PredVP (DetNP somePl_Det) VP_401)
  new> sommigen zullen zichzelf zonder me af vragen wie er is
  old> sommigen zullen zonder me zich af vragen wie er is
```
