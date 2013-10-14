!#/bin/bash

mkdir -p assembly/test

make

for file in test/*
do
  OUTPUT="./assembly/$file"    
  ./ps3 $file > $OUTPUT
done
