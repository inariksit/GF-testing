#/bin/bash

BPATH=$1 
#/Users/inari/src/GF-github/lib/src/basque/

cd $BPATH
gf -make --gfo-dir $GF_LIB_PATH LangEus.gf
cd -
cd data
make
cd -
cabal build && cabal run GF-testing-exe all > /tmp/basque.lins
echo "New results stored in /tmp/basque.lins"
echo "If you have old results stored somewhere, now is a good time to diff."
echo "> ... (imagine you wrote the actual path here)"
diff -u $BPATH/tests/basque.lins /tmp/basque.lins
