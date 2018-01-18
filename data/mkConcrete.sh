#/bin/bash

LANG=$1

GRAMMAR="TestLang$LANG"
echo "concrete $GRAMMAR of TestLang = Grammar$LANG, Lexicon$LANG ;" > grammars/$GRAMMAR.gf
gf -make --src -gfo-dir /tmp grammars/*.gf
