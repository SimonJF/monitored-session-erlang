#!/usr/bin/env bash
cd src/parser
erlc generate_lexer.erl
erlc generate_parser.erl
erl -noinput -run generate_lexer main -run init stop
erl -noinput -run generate_parser main -run init stop
