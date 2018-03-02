#/bin/bash

if [ $# -eq 1 ]; then
  LANG=$1
else
  LANG="Eng"
fi

if [ $# -eq 2 ]; then
  OPTIMIZEPGF="--optimize-pgf"
else
  OPTIMIZEPGF=""
fi

if [ $# -eq 3 ]; then
  DIRTYWORDS=$3
else
  DIRTYWORDS="forbidden-words.txt"
fi

if [ $# -eq 4 ]; then
  NAME="-n $4"
else
  NAME=""
fi

GRAMMAR="TestLang$LANG"
GRFILE="grammars/$GRAMMAR.gf"
echo "concrete $GRAMMAR of TestLang = Grammar$LANG - [" > $GRFILE
for FUN in `cat $DIRTYWORDS`;
  do echo " $FUN, " >> $GRFILE;
done
echo " dummy_N ]"  >> $GRFILE 

echo ", Lexicon$LANG - [" >> $GRFILE
for FUN in `cat $DIRTYWORDS`;
  do echo " $FUN, " >> $GRFILE;
done
echo " dummy_N ]"  >> $GRFILE  #hack

rm /tmp/*.gfo
gf -make $OPTIMIZEPGF --src -gfo-dir /tmp $NAME grammars/TestLang*.gf
