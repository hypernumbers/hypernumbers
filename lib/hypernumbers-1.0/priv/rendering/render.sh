#!/bin/bash
erlc -o ./ebin ./src/render.erl
erl -pa ./ebin -noshell -s render run -s init stop