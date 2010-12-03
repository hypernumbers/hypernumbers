#!/bin/sh                                                                       

HN_LIB=~/hypernumbers/lib

dialyzer -r \
    $HN_LIB/hypernumbers-1.0/ebin/ \
    $HN_LIB/formula_engine-1.0/ebin/ \
    $HN_LIB/read_excel-1.0/ \
    > ~/hypernumbers/priv/dialyzer/output.dialyzer.txt