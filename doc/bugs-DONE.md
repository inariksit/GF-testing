# Bugs fixed in the Dutch RGL

### Variety of word order weirdness in verbal complements



Wrong: men is aan het *het* een worm **helpen** te zijn  
Native suggestion: men is *het* aan het **helpen** een worm te zijn

This error was due to the function `ProgrVP`, but to fix it, we had to change several other functions.

ProgrVP used to construct a fixed string: "aan het" ++ obj ++ fin ++ inf.
But we wanted to get the first thing out of the object: "it a worm" so that we can say "it **help** *to be* a worm".
This required changes in a few other VP-manipulating functions:

1) When inserting an object to a VP, check if the VP has an inf. If it does, move the current object into the inf field.

For instance: `{ s = help ; obj = worm ; inf = to be }` is the initial VP. We add *it* as an object to *help*, and move *worm* into inf. New situation: `{ s = help ; obj = it ; inf = to be worm }`.

But if the initial VP has no inf, we stack objects like they used to. For instance: `{ s = give ; obj = to worm ; inf = [] }` and we add *it* as an object, it just becomes `{ s = give ; obj = it to worm ; inf = [] }`.

2) If we move the old object into the VP, choose its agreement at this
stage (`ComplSlash`), based on the agreement of the new object to be
inserted. Example: *help* **you** *to like* **yourself** / *help* **them**
*to like* **themselves**.

3) If the VPSlash has a preposition and ComplSlash introduces a
complement, put that complement (with the preposition) into an
adverbial position, not to the object position. This is only changed
for ComplSlash which inserts a NP, not any other argument-introducing function.

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

### Imperatives

Plural imperative should be the same as the singular (simplest solution) 

(TODO write more about this)

### Lexicon / other small fixes

* youPol_Pron had agreement of `Sg P2`, changed it to `Sg P3` so that a correct reflexive pronoun is chosen. It also affects the inverted word order: *ben je* but *bent u*. This had to be fixed separately in `ImpVP` -- maybe merge that code to mkClause at some point? Seems redundant.

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

* 1) UseCl (TTAnt TPres ASimul) PPos (GenericCl (ComplVQ know_VQ (UseQCl (TTAnt TPres ASimul) PPos QCl_172)))
TestLangDut> men weet wat zichzelf vandaag helpen te zijn
TestLangEng> one knows what help themselves be today
- is "what" plural here? In which case I would say "welke".

* willen and zullen:
jij, zij, hij, men *wil* & *zal* (was: *wilt* and *zult*)  
Past tens of willen "wilde", "wilden" instead of "wou", "wouden"

# TODO / in progress

- niet een -> geen, niet (plural) -> geen (plural)

Had to add yet another parameter (Polarity) into NPs and VPs.

# Other languages

## German & English

For both languages, I added a Boolean `isMissingAdv` in the VPSlash
category, and an `objAgr` auxiliary function, which takes a NP, a VP*
and forces the agreement of the VP*'s object to follow the NP's
agreement.

Here is a high-level example:

initial VP: `{ s = like ; obj = table { I => "myself" ; You => "yourself" ; … } ; inf = []}`

If we added a subject at that point, the subject would choose the
appropriate agreement: *I* like *myself*, *you* like *yourself*. But
if we add something like `help_V2V` into this construction, we get the
following:

new VP: `{ s = help ; obj = table { I => "myself" ; You => "yourself"; … } ; inf = like}`

Adding an object `he_Pron`:

Old version: `{ s = help ; obj = table { I => "him like myself" ; You => "him like yourself"; … } ; inf = []}`

In the old version, we just concatenated the object and the infinitive
with the reflexive that was already in the object table. But the scope
of the reflexive was wrong: when adding an object to a VP that has a
slashy complement clause, the object should complete the slashy
complement **and** pick the agreement. It's not in the scope for the
subject.

New version: `{ s = help ; obj = table { I => "him like himself" ; You => "him like himself"; … } ; inf = []}`

The only exception to this is when the `VPSlash` is formed by
`VPSlashPrep : VP -> Prep -> VPSlash`. That's why we added the boolean
`missingAdv`: the VPSlash is not missing a core argument, so it
shouldn't affect the agreement. Otherwise we get sentences like "I
like ourselves without us". It would be a valid linearisation for the
tree where [ourselves without us] is a constituent, but when the order
of the constructors is

1) `ReflVP like` "like `table { I =>` myself `; You =>` yourself `; … }`"
2) `VPSlashPrep (ReflVP like) without`  "like `table { I =>` myself `; You =>` yourself `; … }` without _"
3) `ComplSlash (VPSlashPrep (ReflVP like) without) we_Pron)`  "like `table { I =>` myself `; You =>` yourself `; … }` without us"

The VPSlash constructed by VPSlashPrep would look identical to
ComplSlash, so we added a Boolean `missingAdv` that tells not to pick
agreement from the NP argument of `ComplSlash`.

This was the change we made to the grammar. Comparing against the old
version gives some expected changes:

TestLangGer> wir helfen etwas sich zu werfen -- "we help something throw itself"  
TestLangGer-OLD> wir helfen etwas uns zu werfen -- "we help something throw ourselves"  

Exactly the same changes were done for English.

### English word order


TestLang: UseCl (TTAnt TPres ASimul) PPos (GenericCl (ComplSlash (SlashVV can8know_VV (Slash3V3 add_V3 (UsePron it_Pron))) (UsePron i_Pron)))  
TestLangEng: one can me add to it

Lang: UseCl (TTAnt TPres ASimul) PPos (GenericCl (ComplSlash (SlashVV must_VV (Slash3V3 give_V3 (UsePron it_Pron))) (UsePron i_Pron)))  
LangEng: one must me give it

Lang: UseCl (TTAnt TPres ASimul) PPos (GenericCl (ComplSlash (SlashVV want_VV (Slash3V3 give_V3 (UsePron it_Pron))) (UsePron i_Pron)))  
LangEng: one wants me to give it

