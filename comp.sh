#!/bin/bash

if [ ! -d "competition" ]; then
	mkdir -p competition
	cd competition
	git clone https://github.com/stedolan/bf.sed.git
	mkdir nayuki
	curl https://www.nayuki.io/res/optimizing-brainfuck-compiler/bfc.py -o nayuki/bfc.py 

	git clone https://github.com/brianquinlan/brainfuck-jit.git
	cd ../
fi

mkdir -p build

echo "running trainmuck"
mlton bf.sml
cat examples/mandle.bf | ./bf -c > tmp.c
cat examples/mandle.bf | ./bf > tmp.s

gcc -O3 tmp.c -o ./build/mandle_tm_opt 
gcc tmp.c -o ./build/mandle_tm 
as tmp.s -o tmp.o
gcc driver.c tmp.o -o ./build/mandle_tm_as

echo "running bfoptimizer"
cat examples/mandle.bf | ./competition/bfoptimization/optimizr.py all > tmp.c
gcc -O3 tmp.c -o ./build/mandle_bfo_opt
gcc tmp.c -o ./build/mandle_bfo

echo "running bf.sed"
cat examples/mandle.bf | ./competition/bf.sed/bf.sed > ./build/mandle_bfsed

echo "running bfc.py"
python3 ./competition/nayuki/bfc.py examples/mandle.bf tmp.c
gcc -O3 tmp.c -o ./build/mandle_nayuki_opt
gcc tmp.c -o ./build/mandle_nayuki

hyperfine -r 5 $(for i in $(ls build); do echo "./build/"$i; done) './competition/brainfuck-jit/bf --mode=jit examples/mandle.bf' --export-markdown stats.md

rm tmp.c
rm tmp.o
rm tmp.s
rm -rf build
