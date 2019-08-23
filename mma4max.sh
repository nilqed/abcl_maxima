#!/bin/bash
ABCL=abcl-bin-1.5.0
java -jar $ABCL/abcl.jar --eval "(progn (load \"mma4max/init.lisp\") (load-mma))" --eval "(tl)"

