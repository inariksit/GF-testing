# gftest

## Installation, setup, …

If you want to use the `testLang.sh`, then you should clone this repository into the GF main directory, so that you've got `lib`, `src`, `GF-testing` etc. at the same level. If you don't want to use it, put this repository anywhere.

`cabal install` for the program. To get grammars in right places, read on:

### If you want to test a resource grammar

... here's some helper things already in place:

* Set your `$GF_LIB_PATH`, if not already set. It's a location that resembles `/Users/inari/Library/Haskell/ghc-8.0.2/lib/gf-3.9/share/lib/alltenses`.
* Use the script `mkConcrete.sh <3-letter ISO code>` in `data/` to make a concrete syntax of your choice. Usage: `./mkConcrete.sh Dut` to create `TestLang.pgf` into `data/` with Dutch as the concrete syntax.
* If you want another language, just run it again. Example: `./mkConcrete Eng`. Now your `data/TestLang.pgf` includes Dutch and English concrete syntaxes.
* The text file `dirtywords.txt` contains some words and constructions you might not want to see; e.g. `vomit_V` and functions that make a single NP/Adv/VP/… into an utterance. Modify as you wish.

If you have `cabal install`ed the program, you can run it like this anywhere -- the relevant stuff is in the `data/` directory.

`gftest -l "Dut Eng" -f UseN`

No grammar specified so it uses the `TestLang` grammar. 


### If you want to test some other grammar

Start by copying/linking the PGF in the directory `data/`. Then give it as an argument with `-g`:

`gftest -g Phrasebook -l Dut -f WherePlace`



## Common use cases

```
Common flags:
  -g --grammar=FILE        Path to the grammar (PGF) you want to test
  -l --lang="Eng Swe"      Concrete syntax + optional translations
  -f --function=UseN       Test the given function(s)
  -c --category=NP         Test all functions with given goal category
  -t --tree="UseN tree_N"  Test the given tree
  -s --start-cat=Utt       Use the given category as start category
     --show-cats           Show all available categories
     --concr-string=the    Show all functions that include given string
  -q --equal-fields        Show fields whose strings are always identical
  -e --empty-fields        Show fields whose strings are always empty
  -u --unused-fields       Show fields that never make it into the top
                           category
  -n --nullable            Show trees that are erased
  -o --old-grammar=ITEM    Path to an earlier version of the grammar
     --only-changed-cats   When comparing against an earlier version of a
                           grammar, only test functions in categories that have
                           changed between versions
  -b --treebank=ITEM       Path to a treebank
  -d --debug               Show debug output
  -? --help                Display help message
  -V --version             Print version information
```

#### `-f` 

specifies the function: we test `UseN` and put it in the default start category of the grammar. If you want to specify another start category, you can do it with the `-s` flag:

`gftest -l "Dut Eng" -f UseN -s Adv`

This only creates holes using `UseN` in `Adv`, instead of `S` or `Utt`.

`--show-cats` shows the available categories. 

```
> gftest -g Phrasebook --show-cats
Float, Phrase, PlaceKind, Item, Sentence, Date, Person, Nationality, Digit, Proposition, Citizenship, Day, Modality, Language, Object, Sub1000000, Action, Quality, Superlative, ByTransport, VerbPhrase, PrimObject, Number, Sub100, PlurKind, DrinkKind, Word, Message, Name, Country, Place, Kind, Numeral, Currency, Transport, Property, Sub10, Int, Greeting, Digits, MassKind, Price, Question, String, Sub1000, Dig
```

#### `-c` 

tests all functions that produce a certain category. e.g.  

`gftest -l Dut -t Eng -c NP -s Adv`


#### `-e`, `-q`, `-u`

Information about the fields: empty, equal and unused in the top category (specified by `-s` or the default start category of the grammar). Example:

```
> gftest -l Dut -e
### Empty fields:
==> Ant: s

==> Pol: s

==> Temp: s

==> Tense: s

==> V: particle, prefix
```

