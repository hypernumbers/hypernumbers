#!/bin/sh                                                                       

ERL_LIB=/usr/local/lib/erlang/lib
HN_LIB=~/hypernumbers/lib

dialyzer --build_plt -r \
    $ERL_LIB/erts-5.8.1/ebin \
    $ERL_LIB/kernel-2.14.1/ebin \
    $ERL_LIB/stdlib-1.17.1/ebin \
    $ERL_LIB/mnesia-4.4.15/ebin \
    $ERL_LIB/inets-5.5/ebin \
    $ERL_LIB/crypto-2.0.1/ebin \
    $ERL_LIB/sasl-2.1.9.2/ebin \
    $ERL_LIB/mnesia-4.4.15/ebin \
    $ERL_LIB/ssl-4.0.1/ebin \
    $ERL_LIB/public_key-0.8/ebin \
    $ERL_LIB/xmerl-1.2.6/ebin \
    $HN_LIB/gettext/ebin \
    $HN_LIB/starling/ebin \
    $HN_LIB/mochiweb/ebin \
    $HN_LIB/sgte/ebin