#!/bin/bash
cat $1 | ./bf > gen.s
as gen.s -o gen.o && gcc driver.c gen.o -O3
time ./a.out