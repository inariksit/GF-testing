# Bugs fixed in the Dutch RGL

### Variety of word order weirdness in verbal complements



Wrong: men is aan het *het* een worm **helpen** te zijn
Native suggestion: men is *het* aan het **helpen** een worm te zijn

ProgrVP used to construct a fixed string: "aan het" ++ obj ++ fin ++ inf.
But we wanted to get the first thing out of the object: "it a worm" so that we can say "it **help** *to be* a worm".
This required changes in a few other VP-manipulating functions:

1) When inserting an object to a VP, check if the VP has an inf. If it does, move the current object into the inf field.

For instance: `{ s = help ; obj = worm ; inf = to be }` is the initial VP. We add *it* as an object to *help*, and move *worm* into inf. New situation: `{ s = help ; obj = it ; inf = to be worm }`.

But if the initial VP has no inf, we stack objects like they used to. For instance: `{ s = give ; obj = to worm ; inf = [] }` and we add *it* as an object, it just becomes `{ s = give ; obj = it to worm ; inf = [] }`.

2) If we move the old object into the VP, choose its agreement at this stage (`ComplSlash`), based on the agreement of the new object to be inserted. Example: *help* **it** *to be* **a worm** / *help* **them** *to be* **worms**.

#### 3) This still needs to be fixed: the previous does a wrong thing now with VPSlashes constructed by VPSlashPrep: chooses agreement based on an adverbial complement.

### Participles

Add missing inflected forms for past participles for verbs.

Add function `PastPartAP` that uses it. Modify `mkClause` to output also participle forms; no need to write several functions that handle word order.

Add a new field `isHeavy` for VPs, so we know if the resulting AP is pre- or postmodifier.

### Bug in postmodifier APs

Wrong: "ik ben een getrouwde met X worm"  
Right: "ik ben een worm getrouwd met X"

Two bugs: placement and the adjective form. "een getrouwde worm" is correct, but a heavier AP should become a postmodifier, and in that case, the adjective form should be without the *e* at the end.

Easy fix, a few lines in AdjCN, and s/True/False/ in CompA2.

### Extra prefix & missing participle

Wrong: "ik zal me vinden"  
Right: "ik zal me leuk vinden"

Was a bug in `infVP`, fixed with 1 line.

Wrong: "ik heb iets toe toegevoegd"  
Right: "ik heb iets toegevoegd"

Was a bug in `mkClause`, fixed with 4 lines.

### Lexicon / other small fixes

* youPol_Pron had agreement of `Sg P2`, changed it to `Sg P3` so that a correct reflexive pronoun is chosen. It also affects the inverted word order: *ben je* but *bent u*. (TODO is the new form correct?)

* CleftNP and CleftAdv had *'t*, changed them to *het*.

* talk_V3 had the preposition *met* formed with `mkPrep "met"`, but this produces wrong form for cases when the preposition contracts and merges with a NP. Fixed it to use `StructuralDut.with_Prep`.

* Choose always stressed forms of personal pronouns.

* Change agreement in conjunctions: *ik of jij bent een worm* instead of *ik of jij ben een worm*. Based on the following heuristic:

```
I know: it is the least upper bound of all forms!
ik < jij < hij/zij < wij/jullie/zij
I think?
and then you take the "biggest" according to that ordering
```
