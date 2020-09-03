fun enumerate xs = let open List in ListPair.zip (tabulate (length xs, fn x => x), xs) end

structure Syntax = struct
  datatype t
    = Off of int
    | Inc of int
    | Dec of int
    | JumpF of int
    | JumpB of int
    | Out
    | In

  local 
    fun fold (Off a :: Off b :: xs)   = fold (Off (a + b) :: xs)
      | fold (Inc a :: Inc b :: xs)   = fold (Inc (a + b) :: xs) 
      | fold (Dec a :: Dec b :: xs)   = fold (Dec (a + b) :: xs)
      | fold (Off 0 :: xs) = fold xs
      | fold (x :: xs) = x :: fold xs
      | fold [] = []

    (* collect : int list -> int list -> (int * t) list -> ((int * int) list * t list) 
    * take 2 accumulator lists to store forward and backward jump locations
    * take 1 enumerated list of Syntax.t
    * return a list of jump patches, and instructions
    *)
    fun collect fs bs ((idx, JumpF i) :: xs) = ((fn (bs, xs) => (bs, JumpF idx :: xs)) o collect (idx::fs) bs) xs
      | collect (f::fs) bs ((idx, JumpB i) :: xs) = let val (bs, xs) = collect fs ((f, idx)::bs) xs in (bs, JumpB f :: xs) end 
      | collect [] bs ((idx, JumpB i) :: xs) = raise Fail "Missing matching jump"
      | collect fs bs ((_, x) :: xs) =  let val (bs, xs) = collect fs bs xs in (bs, x::xs) end
      | collect _ bs [] = (bs, []) 

    fun patch (ls, JumpF i :: xs) = 
        (case List.find (fn (f, _) => f = i) ls 
          of SOME (_, r) => JumpF r :: patch (ls, xs)
            | NONE => raise Fail "Missing match jump")
      | patch (ls, x::xs) = x :: patch (ls, xs)
      | patch _ = []
    
    fun parseChar #">" = Off 1
      | parseChar #"<" = Off ~1
      | parseChar #"+" = Inc 1
      | parseChar #"-" = Dec 1
      | parseChar #"[" = JumpF 1
      | parseChar #"]" = JumpB 1
      | parseChar #"." = Out
      | parseChar #"," = In
      | parseChar _ = Off 0
  in
    val parse = patch o (collect [] []) o enumerate o fold o (map parseChar) o explode
  end
end

signature IR_SIG = sig 
datatype t 
    = Off of int
    | Loop of t list
    | Add of int * int
    | Sub of int * int
    | Zero
    | Print
    | Read

  val lower : Syntax.t list -> t list
end

structure IR :> IR_SIG = struct
  datatype t 
    = Off of int
    | Loop of t list
    | Add of int * int
    | Sub of int * int
    | Zero
    | Print
    | Read

  structure S = Syntax;

  (* Compile to IR *)
  fun translate (v: S.t list) = 
    let 
      fun loop target ((idx, x) :: xs) =
          let 
            val inner =
            fn S.JumpF tgt => 
                let val (a, b) = List.partition (fn (idx, _) => idx < tgt) xs
                in Loop (loop tgt a) :: (loop target b) end
              | S.JumpB tgt => if target = tgt then [] else loop target xs
              | S.Off x => Off x :: loop target xs
              | S.Inc x => Add (0, x) :: loop target xs
              | S.Dec x => Sub (0, x):: loop target xs
              | S.In => Read :: loop target xs
              | S.Out => Print:: loop target xs
          in 
            inner x
          end
        | loop _ [] = []
    in 
      loop (List.length v) (enumerate v)
    end

  (* Optimization of IR *)  
  fun fold x (Add (y, b) :: Off z :: xs) = if y = z then Off y :: Add (0, b) :: fold x xs else Add (x+y, b) :: fold (x+z) xs
    | fold x (Sub (y, b) :: Off z :: xs) = if y = z then Off y :: Sub (0, b) :: fold x xs else Sub (x+y, b) :: fold (x+z) xs
    | fold x (Add (y, b) :: xs) = Add (x+y, b) :: fold x xs
    | fold x (Sub (y, b) :: xs) = Sub (x+y, b) :: fold x xs
    | fold x (Off a :: xs) = fold (x+a) xs
    | fold x (Loop [Sub(0, 1)] :: xs) = fold' x Zero (fold 0 xs)
    | fold x (Loop ys :: xs) = fold' x (Loop (fold 0 ys)) (fold 0 xs)
    | fold x (instr :: xs) = fold' x instr xs
    | fold x [] = if x <> 0 then [Off x] else []
  and fold' x instr xs = if x <> 0 then Off x :: instr :: fold 0 xs else instr :: fold 0 xs

  val lower = (fold 0) o translate 
end

signature BACKEND = sig
  structure IR : IR_SIG
  val emit : IR.t list -> string
end

functor Cbackend (IR : IR_SIG) :> BACKEND = struct 
  structure IR = IR;
  fun emit' (IR.Off x) = (case Int.compare (x, 0) 
      of GREATER => "ptr += " ^ (Int.toString x) ^ ";"
        | LESS => "ptr -= " ^ (Int.toString (~x)) ^ ";"
        | EQUAL => "")
    | emit' (IR.Add (off, x))  = "*(ptr + " ^ (Int.toString off) ^") += " ^ (Int.toString x) ^ ";"
    | emit' (IR.Sub (off, x))  = "*(ptr + " ^ (Int.toString off) ^") -= " ^ (Int.toString x) ^ ";"   
    | emit' (IR.Loop xs) = "while (*ptr) {\n" ^ String.concatWith "\n" (map emit' xs) ^ "}"
    | emit' IR.Print = "putchar(*ptr);"
    | emit' IR.Read = "*ptr = getchar();"
    | emit' IR.Zero = "*ptr = 0;"

  fun emit xs = "#include <stdio.h>\n#include <stdlib.h>\nint main() {\nchar* ptr = (char*) calloc(30000, 1);char* pp = ptr;\n\n" 
    ^ String.concatWith "\n" (map emit' xs) ^ "\nfree(pp);\nreturn 0;\n}\n"
end;

functor AMD64 (IR : IR_SIG) :> BACKEND = struct
  structure IR = IR;
  val names = ref 0;
  fun fresh () = 
    let val next = !names
        val () = names := (next + 1)
        val s = "label" ^ (Int.toString next)
    in s end

  val prelude  = ".section .text\n.global bf\n.global storage\n.extern putchar\n.extern getc\nbf:\n\tlea storage(%rip), %rdx\n\t"
  val epilogue = ".section .data\n.lcomm storage, 30000\n"
  val printSys = "print:\n\tpushq %rdx\n\tmov (%rdx), %rdi\n\tcall putchar\n\tpopq %rdx\n\tret\n"
  val readSys  = "read:\n\tpushq %rdx\n\tmov (%rdx), %rdi\n\tcall getchar\n\tpopq %rdx\n\tret\n"

  fun emit' (IR.Off x)  = 
    (case Int.compare (x, 0) 
      of GREATER => "addq $" ^ (Int.toString x) ^ ", %rdx"
        | LESS => "subq $" ^ (Int.toString (~x)) ^ ", %rdx"
        | EQUAL => "")
    | emit' (IR.Loop xs) = 
      let val label = fresh ()
          val post = fresh ()
          val cond = "movq (%rdx), %rax\n\ttest %al, %al\n\tjz " ^ post ^ "\n"
          val jmp = "movq (%rdx), %rax\n\ttest %al, %al\n\tjnz " ^ label ^ "\n\n" ^ post ^ ":"
          val instrs = String.concatWith "\n\t" (map emit' xs)
      in String.concatWith "\n\t" [cond, label ^ ":", instrs, jmp] end
    | emit' IR.Print = "call print"
    | emit' IR.Read = "call read"
    | emit' (IR.Add (off, v)) = "addq $" ^ (Int.toString v) ^ ", " ^ (Int.toString off) ^ "(%rdx)"
    | emit' (IR.Sub (off, v)) = "subq $" ^ (Int.toString v) ^ ", " ^ (Int.toString off) ^ "(%rdx)"
    | emit' IR.Zero = "movq $0, (%rdx)"
  fun emit xs = prelude ^ String.concatWith "\n\t" (map emit' xs) ^ "\n\tret\n" ^ printSys ^ readSys ^ epilogue
end

functor Compiler(C : BACKEND) = struct
  val compile = C.emit o C.IR.lower o Syntax.parse
end

structure C = Compiler(Cbackend(IR));
structure Asm = Compiler(AMD64(IR));

val hello = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."


val input = TextIO.input TextIO.stdIn
val _ = print (C.compile input)