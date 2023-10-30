#!/bin/sh

menhir --only-tokens tokens.mly
menhir -b parser --table --explain --external-tokens Tokens tokens.mly parser.mly || true
rm -f parser.mli parser.ml tokens.mli tokens.ml
cat parser.conflicts

