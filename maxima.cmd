@echo off
set ABCL=abcl-bin-1.5.0
set MAXIMA=maxima-5.43.0
cd %MAXIMA%\src
java -jar ..\..\%ABCL%\abcl.jar --eval "(progn (load \"maxima-build.lisp\") (maxima-load) (cl-user::run))"
cd ..\..