#!/bin/zsh

grep -v "^#" data/hoge.KNP | ./dist/build/readPhraseStructure/readPhraseStructure
