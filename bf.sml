fun enumerate xs = let open List in ListPair.zip (tabulate (length xs, fn x => x), xs) end

structure Syntax = struct
  exception Parse of char
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
      | parseChar c = raise Parse c
  in
    val parse = patch o (collect [] []) o enumerate o fold o (map parseChar) o explode
  end
end

signature ASSEMBLY =
sig
  type asm
  type state
  val translate : Syntax.t list -> asm list
  val emit : asm list -> string
  val eval : asm list -> state
  val emitAsm : asm list -> string
end

structure Assembly = struct
  datatype asm 
    = Off of int
    | AddR of int 
    | SubR of int 
    | Loop of asm list
    | FAdd of int * int
    | FSub of int * int
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
              | S.Inc x => AddR x :: loop target xs
              | S.Dec x => SubR x :: loop target xs
              | S.In => Read :: loop target xs
              | S.Out => Print:: loop target xs
          in 
            inner x
          end
        | loop _ [] = []
    in 
      loop (List.length v) (enumerate v)
    end
  
  
  fun fold x (Off a :: AddR b :: xs)  = FAdd (x+a, b) :: fold (x+a) xs
    | fold x (Off a :: SubR b :: xs)  = FSub (x+a, b) :: fold (x+a) xs
    | fold x (Off a :: xs) = fold (x+a) xs
    | fold x (Loop ys :: xs) = fold' x (Loop (fold 0 ys)) (fold x xs)
    | fold x (instr :: xs) = fold' x instr xs
    | fold x [] = if x <> 0 then [Off x] else []
  and fold' x instr xs = if x <> 0 then Off x :: instr :: fold 0 xs else instr :: fold 0 xs

  (* C emitter *)
  fun emitC (Off x) = "ptr += " ^ (Int.toString x) ^ ";"
    | emitC (AddR x)  = "*ptr += " ^ (Int.toString x) ^ ";"
    | emitC (SubR x)  = "*ptr -= " ^ (Int.toString x) ^ ";"   
    | emitC (Loop xs) = "while (*ptr) {\n" ^ String.concatWith "\n" (map emitC xs) ^ "}"
    | emitC Print = "putchar(*ptr);"
    | emitC Read = "*ptr = getc();"
    | emitC _ = raise Match

  fun emit xs = "#include <stdio.h>\n#include <stdlib.h>\nint main() {\nchar* ptr = (char*) malloc(0x4096);\n" ^ String.concatWith "\n" (map emitC xs) ^ "return 0;\n}\n"

  (* Assembly emitter *)
  val names = ref 0;
  fun fresh () = 
    let val next = !names
        val () = names := (next + 1)
        val s = "label" ^ (Int.toString next)
    in s end

  val prelude  = ".section .text\n.global bf\n.global storage\n.extern putchar\n.extern getc\nbf:\nlea storage(%rip), %rdx\n"
  val epilogue = ".section .data\n.lcomm storage, 0x4096\n"

  val printSys = "print:\n\tpushq %rdx\n\tmov (%rdx), %rdi\n\tcall putchar\n\tpopq %rdx\n\tret\n"
  val readSys  = "read:\n\tpushq %rdx\n\tmov (%rdx), %rdi\n\tcall getc\n\tpopq %rdx\n\tret\n"

  fun emit' (Off x)  = 
    (case Int.compare (x, 0) 
       of GREATER => "addq $" ^ (Int.toString x) ^ ", %rdx"
        | LESS => "subq $" ^ (Int.toString x) ^ ", %rdx"
        | EQUAL => "")
    | emit' (AddR x)  = "addq $" ^ (Int.toString x) ^ ", (%rdx)"
    | emit' (SubR x)  = "subq $" ^ (Int.toString x) ^ ", (%rdx)"        
    | emit' (Loop xs) = 
      let val label = fresh ()
          val post = fresh ()
          val cond = "movq (%rdx), %rax\n\ttest %al, %al\n\tjz " ^ post ^ "\n"
          val jmp = "movq (%rdx), %rax\n\ttest %al, %al\n\tjnz " ^ label ^ "\n\n" ^ post ^ ":"
          val instrs = String.concatWith "\n\t" (map emit' xs)
      in String.concatWith "\n\t" [cond, label ^ ":", instrs, jmp] end
    | emit' Print = "call print"
    | emit' Read = "call read"
    | emit' _ = raise Match

  fun emitAsm xs = prelude ^ String.concatWith "\n\t" (map emit' xs) ^ "\n\tret\n" ^ printSys ^ readSys ^ epilogue


  (* Evaluator *)
  type state = {store: int vector, ptr: int}
  fun new () = {store= Vector.tabulate(256, fn _ => 0), ptr=0}

  fun update {store, ptr} f = 
    let val s = if ptr >= Vector.length store then Vector.concat [store, Vector.tabulate(256, fn _ => 0)] else store
        val v = Vector.sub (s, ptr)
        val s' = Vector.update (s, ptr, f v)
    in {store=s', ptr=ptr} end
  fun updatePtr {store, ptr} f = {store=store, ptr=f ptr}

  fun eval s (Off x) = updatePtr s (fn p => p + x)
    | eval s (AddR x) = update s (fn v => v+x)
    | eval s (SubR x) = update s (fn v => v-x)
    | eval s (Print) = update s (fn v => (print (Char.toString (chr v)); v))
    | eval s (Read) = update s (fn _ => (ord o Option.valOf o TextIO.input1) TextIO.stdIn)
    | eval s (Loop xs) = loop s xs
    | eval s _ = raise Match
  and loop (s as {store, ptr}) (xs) = 
    case Vector.sub(store, ptr)
      of 0 => s
      | _ => loop (foldl (fn (a,b) => eval b a) s xs) xs
  
  val eval = foldl (fn (a, b) => eval b a) (new ())
end

val trans = Assembly.translate o Syntax.parse
val compile = Assembly.emit o trans
val compile' = Assembly.emitAsm o trans
val run = Assembly.eval o trans
val hello = "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++."

val () = print (compile' hello)