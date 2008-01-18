@echo off 
set erldir=C:\Program Files\erl5.5.5

echo "COMPILING LEXER AND PARSER"
cd ./priv/muin/
"%erldir%\bin\erlc" generate.erl
"%erldir%\bin\erl" -noshell -s generate gen -s init stop
cd ../../

echo "COMPILING COMPILERS"
"%erldir%\bin\erlc" -o ebin src/compile_code.erl

echo "COMPILING BUILD"

cd ebin
"%erldir%\bin\erl" -pa ebin -noshell -s compile_code -s init stop

cd ..