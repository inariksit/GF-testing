#/bin/bash

LANG=$1
DIRTYWORDS=$2
if [[ $DIRTYWORDS == "" ]]; then
	DIRTYWORDS="forbidden-words.txt"
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

gf -make --src -gfo-dir /tmp grammars/*.gf
