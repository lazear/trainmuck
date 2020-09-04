#!/bin/bash
cat $1 | ./bf -c > gen.c
gcc -O3 gen.c
time ./a.out