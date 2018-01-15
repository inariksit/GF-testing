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

