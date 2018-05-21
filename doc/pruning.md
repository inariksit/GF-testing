# Pruning the trees within one set of test cases

This example is partly fictional for educational purposes.

We test `PropQuality` from Estonian Phrasebook, and we get the following test cases:

* `PropQuality (PropCit (CitiNat Swedish))` (instance `PropQuality : Property_54900 → Quality_54904`)
* `PropQuality Suspect` (instance `PropQuality : Property_54898 → Quality_54902`)

This is legit: Swedish as a property doesn't inflect in an attributive position, but Suspect inflects everywhere.
This behaviour is encoded in a parameter in the GF concrete syntax, and the parameter translates into different concrete categories:
`Quality_54904` for noninflecting and `Quality_54902` for inflecting.

Now let us look at the contexts for the phrases.
* `PropQuality (PropCit (CitiNat Swedish))` : Quality_54904
  1) `PQuestion (QProp (IsMass Salt Quality_54904))`  (sg nominative, predicative)
  2) `PQuestion (QProp (Is (Thes Pizza) Quality_54904))` (pl nominative, predicative)
  3) `PSentence (SHaveNo IMale (SuchKind Quality_54904 Pizza))` (invariant form, in practice sg nominative)
* `PropQuality Suspect`
  1) `PQuestion (QProp (IsMass Salt Quality_54902))` (sg nominative, predicative)
  2) `PQuestion (QProp (Is (Thes Pizza) Quality_54902))` (pl nominative, predicative)
  3) `PQuestion (HowMuchCost (That (SuchKind Quality_54902 Pizza)))` (sg nominative)
  4) `PQuestion (QWhereDoVerbPhrase IMale (V2Buy (OneObj (ObjIndef (SuchKind Quality_54902 Pizza)))))` (sg accusative)
  5) `PSentence (SHaveNoMass IMale (SuchMassKind Quality_54902 Salt))` (sg partitive)
  6) `PQuestion (QProp (PropAction (ALike IMale (That (SuchKind Quality_54902 Pizza)))))` (sg elative)
  7) `PQuestion (HowMuchCost (Thes (SuchKind Quality_54902 Pizza)))`(pl nominative)
  8) `PSentence (SHaveNo IMale (SuchKind Quality_54902 Pizza))` (pl partitive)
  9) `PQuestion (QProp (PropAction (ALike IMale (Thes (SuchKind Quality_54902 Pizza)))))` (pl elative)

For the first sentence, only 3 contexts are generated. This is enough to squeeze out all the variation in the quality Swedish.
For the second sentence, 9 contexts are needed.

We notice that all 3 contexts for `PropQuality (PropCit (CitiNat Swedish))` are included in the contexts for `PropQuality Suspect`.
Now, we look into the coercions in the grammar to find out if some of this repetition could be avoided.

Remember, the test case generation has produced two test cases, in categories Quality_54904 and Quality_54902.
We want to find out if there happens to be any coercion that covers these. Good news, there are (more than one!):

```
_63778 -> [Quality_54904,Quality_54902]
_63820 -> [Quality_54904,Quality_54902, …]
```

Can we pick any of the two coercions? How do we know now which to choose?

The context generation works for all categories, including coercions. So we look into the context list for both of these coercions.

```
_63778:
        PQuestion (QProp (IsMass Salt ∅))
        PQuestion (QProp (Is (Thes Pizza) ∅))
_63820:
        <something irrelevant for us now>
```

In _63778, we find two contexts that appear in the lists of both Suspect and Swedish! 
So we can keep them in only one of the lists, and remove from all others. 
Currently the algorithm chooses always the smallest tree (number of constructors), so it will favour Suspect.
This is the final pruned list that will be linearised--12 sentences shrunk into 10.

* `PropQuality (PropCit (CitiNat Swedish)) : Quality_54904`
  1) `PSentence (SHaveNo IMale (SuchKind Quality_54904 Pizza))`
* `PropQuality Suspect : Quality_54902`
  1) `PQuestion (QProp (IsMass Salt Quality_54902))` 
  2) `PQuestion (QProp (Is (Thes Pizza) Quality_54902))`
  3) `PQuestion (HowMuchCost (That (SuchKind Quality_54902 Pizza)))` 
  4) `PQuestion (QWhereDoVerbPhrase IMale (V2Buy (OneObj (ObjIndef (SuchKind Quality_54902 Pizza)))))` 
  5) `PSentence (SHaveNoMass IMale (SuchMassKind Quality_54902 Salt))` 
  6) `PQuestion (QProp (PropAction (ALike IMale (That (SuchKind Quality_54902 Pizza)))))`
  7) `PQuestion (HowMuchCost (Thes (SuchKind Quality_54902 Pizza)))`
  8) `PSentence (SHaveNo IMale (SuchKind Quality_54902 Pizza))`
  9) `PQuestion (QProp (PropAction (ALike IMale (Thes (SuchKind Quality_54902 Pizza)))))`

## Generalisation into more categories and coercions

The method works the same for any number of initial test cases and coercions.

Take any GF function `F : A -> B -> C` that translates into some list of concrete functions:

```
F_1 : A_1 -> B_1 -> C_1 ;
…
F_n : A_n -> B_n -> C_n ;
```

For each of these instances, a test case is generated. Each test case gets a non-empty list of contexts:

```
F_1 : A_1 -> B_1 -> C_1
  <context C_1^1>
  …
  <context C_1^m>
…
F_n : A_n -> B_n -> C_n
  <context C_n^1>
  …
  <context C_n^m>
```

Assume we have a non-empty list of coercions that cover at least 2 categories $C_k$:

```
Coercion_1 : [C_a,C_b,C_c]
…
Coercion_k : [C_k,C_l,C_m,C_n]
```

Any number of these coercions may contain contexts that overlap with $C_{1…n}^{1…m}. 
We take *all* contexts that overlap, and make sure each of them is only introduced once.
For each context, we choose the smallest tree to be the representative.

