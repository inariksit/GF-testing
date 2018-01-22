# GF-testing

Generating test cases for GF grammars

## Baseline ideas
 
* Generate all the examples in the API documentation http://www.grammaticalframework.org/lib/doc/synopsis.html#toc5 e.g. “more warmly than he runs” 
* Implement a standard application grammar, such as Phrasebook, by using the RG to be tested. The
* Blacklist a number of confusing constructors: `PPartNP` (“the man seen”), `ExtAdvNP`, `VocNP`, anything in `Constructions`, … and generate a set of sentences as usual
* Restrict the lexicon to 1-2 default nouns, verbs (one of each valency), adjectives, determiners… 
  * If the language contains grammatical gender/other noun classes, the test lexicon should contain one of each
  * For semantic purposes, ideally at least one human and non-human noun
  * All pronouns, if they show in verbal inflection; but when testing an unrelated feature, should just stick to one pronoun
  * Determiners should have variation: e.g. articles, numbers, demonstratives, possessives
* Test for empty strings in grammars! They may require just the right combination of parameters to show themselves; easy example, some case for some noun is linearised as `[]` instead of `nonExist`, and combined with a certain preposition (or PartNP or PossNP) it gets chosen. 

## Slightly more advanced ideas
* Define groups of functions as pools where to draw constructors to form trees. Like soft whitelists; “this pool is meant to test relative clauses”, and then generate, e.g. using FEAT, from this pool
  * Different pools can partially overlap, and some constructors probably appear in all of them (such as `UseN`).
* Write an application grammar for testing purposes, with functions that use RGL functions but are more abstract, one for each manageable cluster of features to test. For instance, `NP`s would have variants with `MassNP`, `DetCN`, with/without advs and adjs, …
  * Control semantics: sentences like “I gave the beer you” are confusing, because a human would assume that “beer” is the direct object and “you” is the indirect object, and would correct it to “I gave you the beer” or “I gave the beer to you”.
* Define layers of grammar: first generate examples only from e.g. Miniresource, and move on to more constructions if those are correct.

## Testing single functions
* Take a single function, such as `PositA` or `EmbedVP`, and generate all sentences that could change if we change the function.
 * In the case of EmbedVP, it may be irrelevant to test both `EmbedVP (UseComp (CompAP (PositA beautiful_A)))` and `EmbedVP (UseComp (CompAP (UseComparA beautiful_A)))`, because the difference is just which string is chosen from `beautiful_A`.
 * In the case of PositA, it may be irrelevant to test both `EmbedVP (UseComp (CompAP (PositA beautiful_A)))` and `PredSCVP (EmbedVP (UseComp (CompAP (PositA beautiful_A)))) <some other VP>`. If the category SC is just a `{s:Str}`, then certainly the other argument of `PredSCVP` will not make a difference to the SC.
 * It depends on concrete syntax which tests are necessary. If all lincats are just {s : Str}, then we don't have to test anything.
 
## Technical details

GF grammar compiles into a low-level format called PGF.
After the compilation, we get one category for each combination of parameters: for English adjectives, A => A_pos, A_comp, A_superl, and for Spanish, A_pos×sg×masc, A_pos×sg×fem, A_pos×pl×masc, A_pos×pl×fem, A_comp×sg×masc, A_comp×sg×fem, A_comp×pl×masc, A_comp×pl×fem, A_superl×sg×masc, A_superl×sg×fem, A_superl×pl×masc, A_superl×pl×fem.

Suddenly, we have a bunch of new types, and those are different for each concrete syntax! The original question “we need a sample of nouns/verbs/… that makes sense” can be simplified “we need one noun/verb/… of each type”. The types are determined by the parameters in the concrete syntax.

So remember all the hassle when you can't pattern match strings to know something, but you have to define a parameter? This is actually a nice side effect from that: each parameter contributes to a new category, so it pays off in generating examples. If the feature is important for your grammar (e.g. a Boolean `isNeg` field instead of matching for the string "no"), then it's important to create a test case with that feature and without that feature.

## Changes in grammar: what to test?

* Single function: test that function.
* Single lincat: test functions that create and use that lincat (?)
* Oper used by several functions: test all affected functions (how? does the grammar writer have to know which functions use the oper?)

When we generate lots of test trees, we can linearise them using both old and new versions of the same grammar, and only output those sentences that change. But maybe sometimes the fact that something doesn't change is a bug?

True (a bit simplified) story: we discovered that interrogative pronouns can change with certain prepositions, so we added a new string field to `IP` to contain the form that only appears with prepositions. If we forget to change the `PrepIP : Prep -> IP -> IAdv` function, then our change of lincat will have brought nothing.

```haskell
lincat Prep = { s : Str } ;
lincat IP = { s : Str ; specialForm : Str } ;

-- properly working PrepIP
PrepIP : Prep -> IP -> IAdv = \prep,ip -> prep.s ++ ip.specialForm ;

-- properly working QuestVP
QuestVP : IP -> VP -> QCl = \ip,vp -> ip.s ++ vp.s ;
    
-- buggy PrepIP
PrepIP : Prep -> IP -> IAdv = \prep,ip -> prep.s ++ ip.s ;
```

The buggy PrepIP is the old behaviour. So if we forgot to change it, and ran a diff on sentences linearised with old and new grammars, there wouldn't be any difference, and the grammar writer may easily not realise that the lack of change is the problem. Also it would be hard to go and guess which ones to test.

But there are also some functions that really don't need the newly added field: consider `QuestVP : IP -> VP -> QCl`, where there is no preposition in sight, so the correct thing is to pick `ip.s`, not `ip.specialForm`.

What if only some prepositions induce the change?

```haskell
lincat Prep = { s : Str ; inducesChange : Bool } ;
lincat IP = { s : Str ; specialForm : Str } ;

-- properly working PrepIP
PrepIP : Prep -> IP -> IAdv = \prep,ip ->
  case prep.inducesChange of {
    True  => prep.s ++ ip.specialForm ;
    False => prep.s ++ ip.s } ;
```

In this case, if we made the change at the same time, it might be feasible to only test functions that operate on *both* changed categories.

Questions:
* is there a way to automatically infer that the change to IP should only affect `PrepIP` and not `QuestVP`?
* is it too annoying even if we output sentences made by `GenIP, GenModIP, StrandQuestSlash, UttAccIP, UttDatIP, GenIP, StrandQuestSlash, PiedPipingQuestSlash, ExistIP, ExistIPAdv, UttIP, QuestVP, QuestSlash, IdetCN, IdetIP, AdvIP, PrepIP, CompIP, ComplSlashIP, QuestQVP` if the only thing we're interested is `PrepIP`?
* How about first option: if some trees changed, show them; second option: if none changed **and** the change was "add more stuff to lincat X", show all trees that use X? 
* Any other ways to notice bugs? Linearise (`tabularLin`) all intermediate forms, and alert at the stage where a string gets dropped? (e.g. the string in `specialForm`) But this would also report `QuestVP` as suspicious, even though it's doing the right thing.

If the grammarian added a new field **and** moved something that used to be in a different field into the new one **and** failed to update functions, then a lot of things that used to linearise correctly now are incorrect, and diff would notice them. So maybe this particular use case is not a problem? TODO test with German grammar after S's changes.

___

Changes that create more concrete categories:
* add a new constructor to a param that was already in a table -- e.g. `{ s : Number => Str }` where `param Number = Sg | Pl` and we add a Dual to Number. We get one new concrete category.
* add a new param field -- e.g. `lincat Foo = {s : Str}` into `lincat Foo = {s : Str ; isPre : Bool}` multiplies the number of concrete categories for each value of the param. In this case the original was one concrete cat `Foo`, and it becomes two: `Foo_true` and `Foo_false`.
* Adding new string fields doesn't create new concrete categories.

___

## TODO (2018-01-22)

### Make context generation faster
Koen's on it!

### Evaluation

* Cost
  * time of generating examples
  * time of looking at examples
* Effect
  * compare against other methods -- what methods?
  
For application grammars, if you're writing them from scratch, it is actually pretty feasible to just `gt` the hell out of it as you write. But this doesn't work for bigger grammars.

Morphology can be tested efficiently againts any existing morphological analyser. I've used Apertium for Dutch and Basque.

### Include trees from corpus

1. Example generation needs particular kinds of trees, with the right kind of concrete categories and string pattern properties. 
1. Context generation needs a particular path from the top category to the tree generated in step 1; so some functions in that context tree are fixed, but many subtrees are completely free (as long as they are in the right category). These could come from a corpus.

Possibly use corpus to find thematic groups of words (or NPs/APs/...), to make more coherent sentences? But this can lead to "the car drives John" rather than "John drives the car".

### Include a gold standard & filter

Add a database of `[(AST,[lin])]` for trees and linearisations that the user has deemed correct. When running the tests again, only show trees that are not in the database, or whose linearisations are different from those in the db.

If testing an application grammar, or for some other reason the abstract syntax should change, this shouldn't be a big deal -- just update the treebank; remove trees that aren't in the grammar anymore.



