#/bin/bash

ISO="Eng"
LANG="english"

# TODO: get some actual
if [[ $1 != "" ]]; then
  ISO=$1
  LANG=$2
  LANGISO="Lang$ISO.gf"
fi

if [[ $3 != "" ]]; then
	TRANS="-t $3"
fi

HERE=`pwd`
GPATH="$HERE/../lib/src/$LANG/" # Assumes that we are in the GF root directory!

# 1) Build the TestLang.pgf with $LANG, using the old grammar.
# The idea is:
# * you changed the files in GF/lib/src/$LANG/, but
# * old version is still in $GF_LIB_PATH 
# If you have updated $GF_LIB_PATH, then this script just compares two identical versions.

cd data
./mkConcrete.sh $ISO
cp TestLang.pgf TestLangOlder.pgf # keep the old version for later comparison!


# 2) Recompile everything in the GF RGL dev directory and put .gfos to $GF_LIB_PATH
cd $GPATH
gf -make --gfo-dir $GF_LIB_PATH $LANGISO
cd $HERE

# Alternative 2), if the changes in the grammar affect the API
# cabal install
# cd $HERE

# 3) Now build TestLang using the new gfos in $GF_LIB_PATH
cd data
./mkConcrete.sh $ISO
cd $HERE

# 4) Run 
gftest -l $ISO  $TRANS -o TestLangOlder.pgf 

