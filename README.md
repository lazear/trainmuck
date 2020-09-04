# trainmuck

An optimizing brainfuck compiler in 200 lines of Standard ML (should compile with all SML compilers)

Generated C code outperforms that of the Tritium BF compiler ["... BF interpreter/compiler/JIT runner makes other programs look slow. It is simply the fastest BF interpreter you'll find"](https://github.com/rdebath/Brainfuck/tree/master/tritium)

Complete with 2 separate backends
- x84-64 assembly
- C codgen

Optimizations include constant folding, elimination of zeroing loops, elimination of multiplication/division loops, and folding of cell movements along with arithmetic operations.

This all results in fairly optimized assembly, and using the C backend with `-O3` flag set on gcc provides only about a ~2x speedup for most cases (I consider that pretty good!). 
The generated assembly beats the speed of compiling the generated C code without optimizations

On my machine, I can run `example/mandle.bf` in around 700 ms when compiled to assembly, and 325 ms when compiled to C with optimizations. Intrepretation of the same program takes around 7.5 seconds

Benchmarks:
```
trainmuck_c_O3  mandlebrot  326 +/- 1 ms
trainmuck_c_O0  mandlebrot  2200 +/ 50 ms
trainmuck_asm   mandlebrot  730 +/- 1 ms

tritium_c_O3    mandlebrot  340 +/- 6 ms
tritium_c_O0    mandlebrot  590 +/- 2 ms
```