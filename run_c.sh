#!/bin/bash
cat $1 | ./bf -c > gen.c
gcc -Wall -Werror -Wpedantic -O3 -faggressive-loop-optimizations -funroll-all-loops -fwrapv gen.c
time ./a.out