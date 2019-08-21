#!/bin/bash
ABCL=abcl-bin-1.5.0
MAXIMA=maxima-5.43.0
cd $MAXIMA
#
java -jar ../$ABCL/abcl.jar --eval "(progn (load \"configure.lisp\") (configure :interactive nil) (quit))"
cd src
java -jar ../../$ABCL/abcl.jar --eval "(progn (load \"maxima-build.lisp\") (maxima-compile) (quit))"
cd ../..
echo $MAXIMA build completed.
echo run MAXIMA by maxima.sh