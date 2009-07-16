#!/bin/bash
erlc -o ./ebin -I /usr/local/lib/erlang/lib/xmerl-1.1.12/include/  ./src/static_template.erl
erlc -o ./ebin ./src/render.erl
erl -pa ./ebin -noshell -s render run -s init stop