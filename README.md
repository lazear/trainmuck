# trainmuck

An optimizing brainfuck compiler in 200 lines of Standard ML (should compile with all SML compilers)

Complete with 2 separate backends
- x84-64 assembly
- C codgen

Optimizations include constant folding, elimination of zeroing loops, elimination of multiplication/division loops, and folding of cell movements along with arithmetic operations.

This all results in fairly optimized assembly, and using the C backend with `-O3` flag set on gcc provides only about a ~2x speedup for most cases (I consider that pretty good!). 
The generated assembly beats the speed of compiling the generated C code without optimizations

On my machine, I can run `example/mandle.bf` in around 700 ms when compiled to assembly, and 450 ms when compiled to C with `-O3`. Intrepretation of the same program takes around 7.5 seconds