@echo off 

set erldir=C:\Program Files\erl5.6

cd ./test_server
"%erldir%\bin\erl" -s toolbar -pa ../../ebin