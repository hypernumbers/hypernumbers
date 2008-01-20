@echo off 
set erldir=C:\Program Files\erl5.6

cd ebin
"%erldir%\bin\erl" -s production_boot start toolbar -sname arrian -env ERL_MAX_ETS_TABLES 20000
cd ..