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

# 1) Build the TestLang.pgf with Basque, using the old grammar
cd data
make $ISO
cd -
cabal build && cabal run GF-testing-exe all > /tmp/$LANG.lins.old

# 2) Recompile everything in the GF RGL dev directory and put .gfos to GF_LIB_PATH
cd $GPATH
gf -make --gfo-dir $GF_LIB_PATH $LANGISO
cd -

# 3) Now build TestLang using the new gfos 
cd data
make $ISO
cd -

# 4) Run 
cabal build && cabal run GF-testing-exe all > /tmp/$LANG.lins
echo "New results stored in /tmp/$LANG.lins"
echo "Comparing with old results, before recompiling $LANGISO"
diff -u /tmp/$LANG.lins.old /tmp/$LANG.lins
