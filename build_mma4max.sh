#!/bin/bash
ABCL=abcl-bin-1.5.0
java -jar $ABCL/abcl.jar --eval "(progn (load \"mma4max/init.lisp\") (compile-mma) (quit))"
echo mma4max compilation completed.
