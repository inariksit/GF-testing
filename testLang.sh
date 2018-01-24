#/bin/bash

LANG=$1
GPATH=/Users/inari/src/GF-github/lib/src/$LANG/
ISO="ERROR"
if [[ $LANG == basque ]]; then
  ISO="Eus"
  LANGISO="LangEus.gf"
fi
if [[ $LANG == dutch ]]; then
  ISO="Dut"
  LANGISO="LangDut.gf"
fi
if [[ $LANG == estonian ]]; then
  ISO="Est"
  LANGISO="LangEst.gf"
fi

# 1) Build the TestLang.pgf with $LANG, using the old grammar.
# The idea is:
# * you changed the files in GF/lib/src/$LANG/, but
# * old version is still in $GF_LIB_PATH 
# If you have updated $GF_LIB_PATH, then this script just compares two identical versions.

cd data
./mkConcrete.sh $ISO
cp TestLang.pgf TestLangOld.pgf # keep the old version for later comparison!
cd -
cabal build && cabal run GF-testing-exe -- -s $ISO -f all > /tmp/$LANG.lins.old

# 2) Recompile everything in the GF RGL dev directory and put .gfos to $GF_LIB_PATH
cd $GPATH
gf -make --gfo-dir $GF_LIB_PATH $LANGISO
cd -

# 3) Now build TestLang using the new gfos in $GF_LIB_PATH
cd data
./mkConcrete.sh $ISO
cd -

# 4) Run 
cabal build && cabal run GF-testing-exe -- -s $ISO -f all -o TestLangOld.gf > /tmp/$LANG.lins
echo "New results stored in /tmp/$LANG.lins"
echo "Comparing with old results, before recompiling $LANGISO"
diff -u /tmp/$LANG.lins.old /tmp/$LANG.lins

echo "Comparing if concrete categories have changed since last time"
echo "Results found in  $LANG-ccat-changes.md"


