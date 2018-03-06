# gftest

## Installation, setup, …

If you want to use the `testLang.sh`, then you should clone this repository into the GF main directory, so that you've got `lib`, `src`, `GF-testing` etc. at the same level. **If you don't want to use it, just clone this repository anywhere.**

`cabal install` for the program. To get grammars in right places, read on:

### If you want to test a resource grammar

... here's some helper things already in place:

1) Set your `$GF_LIB_PATH`, if not already set. (It should be in a location that resembles `/Users/inari/Library/Haskell/ghc-8.0.2/lib/gf-3.9/share/lib/alltenses`.)
1) Use the script `mkConcrete.sh <ISO code>` in `data/` to make a concrete syntax of your choice. Usage: `./mkConcrete.sh Dut` to create `TestLang.pgf` into `data/` with Dutch as the concrete syntax.
1) If you want another language, just run it again. Example: `./mkConcrete Eng`. Now your `data/TestLang.pgf` includes Dutch and English concrete syntaxes.
1) The text file `dirtywords.txt` contains some words and constructions you might not want to see; e.g. `vomit_V` and functions that make a single NP/Adv/VP/… into an utterance. Modify as you wish.
1) `cabal install`

### If you want to test some other grammar

1) Compile your grammar, with the relevant concrete syntaxes, into a PGF
1) Copy or symlink the PGF in the directory `data/`.
1) cabal install
1) Give the grammar as an argument with `-g`, and language with `-l`:

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
  -w --write-to-file       Write the results in a file (<GRAMMAR>_<FUN>.org)
  -? --help                Display help message
  -V --version             Print version information
```

#### `-g`

Give the PGF grammar as an argument with `-g`. The grammar must be in `data/` folder, and `cabal install`ed after you have placed the grammar in `data/`. You can give it with or without `.pgf`. 

(If the grammar you want to test is `TestLang` formed by `mkConcrete.sh`, you don't need to give the argument, but it's not wrong to give it either.)

Example:

`gftest -g Foods --show-cats`


#### `--show-cats` 

Shows the available categories.

```
> gftest -g Phrasebook --show-cats
Float, Phrase, PlaceKind, Item, Sentence, Date, Person, Nationality, Digit, Proposition, Citizenship, Day, Modality, Language, Object, Sub1000000, Action, Quality, Superlative, ByTransport, VerbPhrase, PrimObject, Number, Sub100, PlurKind, DrinkKind, Word, Message, Name, Country, Place, Kind, Numeral, Currency, Transport, Property, Sub10, Int, Greeting, Digits, MassKind, Price, Question, String, Sub1000, Dig
```

#### `-l`

Give a concrete language. It assumes the format `AbsNameConcName`, and you should only give the `ConcName` part.

You can give multiple languages, in which case it will create the test cases based on the first, and show translations in the rest.

Examples:

`gftest -g Phrasebook -l Swe --show-cats`  
`gftest -g Foods -l "Spa Eng" -f Pizza`

#### `-f` 

Given a grammar (`-g`) and a concrete language ( `-l`), test a function or several functions. 

Examples:

`gftest -l "Dut Eng" -f UseN` (no `-g` argument, i.e. uses TestLang.pgf)  
`gftest -g Phrasebook -l Spa -f "ByTransp ByFoot"`

Special case is `-f all`, which tests all functions in the grammar. (As of 6 March, takes 13 minutes for the English resource grammar, and results in ~40k lines.)

#### `-s`

Give a start category for contexts. Used in conjunction with `-f`, `-c` or `-t`. If not specified, contexts are created for the start category of the grammar.

Example:

`gftest -l "Dut Eng" -f UseN -s Adv`

This creates a hole of `CN` in `Adv`, instead of the default start category.

#### `-c`

Given a grammar (`-g`) and a concrete language ( `-l`), test all functions that return a given category.

Examples:

`gftest -g Phrasebook -l Fre -c Modality`
`gfTest -g Phrasebook -l Fre -c ByTransport -s Action`


#### `-t`

Given a grammar (`-g`) and a concrete language ( `-l`), test a complete tree.

Example:

`gftest -g Phrasebook -l Dut -t "ByTransp Bus" -s Action`

#### `--concr-string`

Show all functions where the given concrete string appears as syncategorematic string (i.e. not from the arguments).

Example:
`gftest -l Eng --concr-string it`  
which gives the answer `==> CleftAdv, CleftNP, DefArt, ImpersCl, it_Pron`

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

#### `-w`

Writes the results into a file. Recommended to open it in emacs org-mode, so you get an overview, and you can maybe ignore some trees if you think they are redundant. And you get different colours, which makes reading it slightly less monotonous.

1) When you open the file, you see a list of generated test cases, like this: ![Instructions how to use org mode](https://raw.githubusercontent.com/inariksit/GF-testing/master/doc/instruction-1.png)  
Place cursor to the left and click tab to open it.
2) You get a list of contexts for the test case. Keep the cursor where it was if you want to open everything at the same time. Alternatively, scroll down to one of the contexts and press tab there, if you only want to open one.
![Instructions how to use org mode](https://raw.githubusercontent.com/inariksit/GF-testing/master/doc/instruction-2.png)  

3) Now you can read the linearisations.  
![Instructions how to use org mode](https://raw.githubusercontent.com/inariksit/GF-testing/master/doc/instruction-3.png)

