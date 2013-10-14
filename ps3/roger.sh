!#/bin/bash

rm -rf assembly/test
mkdir -p assembly/test

make

for file in test/*
do
  OUTPUT="./assembly/$file"    
  ./ps3 $file > $OUTPUT.asm
done
