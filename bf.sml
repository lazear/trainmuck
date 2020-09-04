structure Syntax = struct
  datatype t
    = Off of int
    | Inc of int
    | Dec of int
    | Enter
    | Leave
    | Out
    | In

  local 
    fun fold (Off a :: Off b :: xs)  = fold (Off (a + b) :: xs)
      | fold (Inc a :: Inc b :: xs)  = fold (Inc (a + b) :: xs) 
      | fold (Dec a :: Dec b :: xs)  = fold (Dec (a + b) :: xs)
      | fold (a :: Off 0 :: b :: xs) = fold (a::b::xs)
      | fold (Off 0 :: xs) = fold xs
      | fold (x :: xs) = x :: fold xs
      | fold [] = []
      
    fun parseChar #">" = SOME (Off 1)
      | parseChar #"<" = SOME (Off ~1)
      | parseChar #"+" = SOME (Inc 1)
      | parseChar #"-" = SOME (Dec 1)
      | parseChar #"[" = SOME (Enter)
      | parseChar #"]" = SOME (Leave)
      | parseChar #"." = SOME (Out)
      | parseChar #"," = SOME (In)
      | parseChar _ = NONE
    
    val filterMap = (map Option.valOf) o (List.filter Option.isSome)
  in
    val parse =  fold o filterMap o (map parseChar) o explode
  end
end

signature IR_SIG = sig 
datatype t 
    = Off of int
    | Loop of t list
    | Add of int * int
    | Sub of int * int
    | Mul of int * int
    | Div of int * int 
    | Zero
    | Print
    | Read

  val lower : Syntax.t list -> t list
end

structure IR = struct
  datatype t 
    = Off of int
    | Loop of t list
    | Add of int * int
    | Sub of int * int
    | Mul of int * int
    | Div of int * int 
    | Zero
    | Print
    | Read

  structure S = Syntax;

  (* Build explicit loops *)
  fun loop acc [] = (rev acc, [])
    | loop acc (S.Enter :: xs) = let val (a, ys) = loop [] xs in loop (Loop(a) :: acc) ys end
    | loop acc (S.Leave :: xs) = (rev acc, xs)
    | loop acc (S.Inc x :: xs) = loop (Add (0, x) ::acc) xs 
    | loop acc (S.Dec x :: xs) = loop (Sub (0, x) ::acc) xs 
    | loop acc (S.Off x :: xs) = loop (Off x :: acc) xs
    | loop acc (S.Out :: xs) = loop (Print ::acc) xs 
    | loop acc (S.In :: xs) = loop (Read ::acc) xs 

  exception CantOptimize of t list
  (* Perform loop optimizations *)
  fun optimize xs =
    let val pred = (fn Sub(0, _) => true | _ => false)
        val item = List.find pred xs
        val (subs, rest) = List.partition pred xs
        val () = 
          if (List.length subs) = 1 andalso List.all (fn Add _ => true | Sub _ => true | _ => false) rest 
          then () 
          else raise CantOptimize xs 
        (* Transform + => * and - => / *) 
        fun rep fact (Add (off, x)) = Mul (off, x * fact)
          | rep fact (Sub (off, x)) = Div (off, x * fact)
          | rep _ x = x
    in
      case item 
        of SOME (Sub(0, fact)) => map (rep fact) rest 
         | _ => raise Fail "unreachable"
    end

  (* Optimization of IR *)  
  fun fold x (Add (0, _) :: Zero :: xs) = Zero :: fold x xs
    | fold x (Sub (0, _) :: Zero :: xs) = Zero :: fold x xs
    | fold x (Add (y, b) :: xs) = Add (x+y, b) :: fold x xs
    | fold x (Sub (y, b) :: xs) = Sub (x+y, b) :: fold x xs
    | fold x (Off a :: xs) = fold (x+a) xs
    | fold x (Loop [Sub(0, 1)] :: xs) = foldOff x Zero (fold 0 xs)
    | fold x (Loop ys :: xs) = foldLoop x ys xs
    | fold x (instr :: xs) = foldOff x instr xs
    | fold x [] = if x <> 0 then [Off x] else []
  and foldOff x instr xs = if x <> 0 then Off x :: instr :: xs else instr :: xs
  and foldLoop x ys xs = 
    let val ys = fold 0 ys 
        val xs = fold 0 xs
        val instrs = optimize ys @ (Zero::xs) handle CantOptimize ys => Loop ys :: xs
    in if x <> 0 then Off x :: instrs else instrs end 
  val lower = (fold 0) o (fold 0) o #1 o loop [] 
end

signature BACKEND = sig
  structure IR : IR_SIG
  val emit : IR.t list -> string
end

(* format ints properly *)
fun fix_int x = if x < 0 then "-" ^ (Int.toString o abs) x else Int.toString x
fun fix_intp (pl,mi) x = (if x < 0 then mi else pl) ^ (Int.toString o abs) x

functor Cbackend (IR : IR_SIG) :> BACKEND = struct 
  structure IR = IR;
  fun emit' (IR.Off x) = "ptr " ^ (fix_intp ("+= ", "-= ") x) ^";"
    | emit' (IR.Add (off, x))  = "*(ptr " ^ (fix_intp ("+ ", "- ") off) ^") += " ^ (fix_int x) ^ ";"
    | emit' (IR.Sub (off, x))  = "*(ptr " ^ (fix_intp ("+ ", "- ") off) ^") -= " ^ (fix_int x) ^ ";"   
    | emit' (IR.Loop xs) = "while (*ptr) {\n" ^ String.concatWith "\n" (map emit' xs) ^ "}"
    | emit' IR.Print = "putchar(*ptr);"
    | emit' IR.Read = "*ptr = getchar();"
    | emit' IR.Zero = "*ptr = 0;"
    | emit' (IR.Mul (off, x)) = "*(ptr " ^ (fix_intp ("+ ", "- ") off) ^ ") += *ptr *" ^ (fix_int x) ^ ";"
    | emit' (IR.Div (off, x)) = "*(ptr " ^ (fix_intp ("+ ", "- ") off) ^ ") -= *ptr * " ^ (fix_int x) ^ ";"
  fun emit xs = "#include <stdio.h>\n#include <stdlib.h>\nint main()"
    ^ "{\n\tchar* ptr = (char*) calloc(30000, 1);\n\tchar* pp = ptr;\n\t\n\t" 
    ^ String.concatWith "\n\t" (map emit' xs) ^ "\n\tfree(pp);\n\treturn 0;\n}\n"
end;

functor AMD64 (IR : IR_SIG) :> BACKEND = struct
  structure IR = IR;
  val names = ref 0;
  fun fresh () = 
    let val next = !names
        val () = names := (next + 1)
    in (Int.toString next) end 

  val prelude = [".section .text", ".global bf", ".global storage", ".extern putchar", 
    ".extern getchar", "bf:", "lea storage(%rip), %rdx"]

  val epilogue = ["mov %rdx, %rax", "lea storage(%rip), %rdx", "sub %rdx, %rax", 
    "ret", "print:", "push %rdx", "mov (%rdx), %rdi", "call putchar", "pop %rdx",
    "ret", "read:", "push %rdx", "mov (%rdx), %rdi", "call getchar", "pop %rdx", 
    "mov %rax, (%rdx)", "ret", ".section .bss", ".lcomm storage, 30000\n"]

  fun reg x = (fix_int x) ^ "(%rdx)"

  (* emit multiply/divide instruction *)        
  fun cinstr off v which = [
          "lea " ^ (reg off) ^ ", %rdi",
          "movzx (%rdx), %ecx", "movzx (%rdi), %eax",
          "mov $" ^ (fix_int v) ^ ", %rsi",
          "imul %esi, %ecx", which, "movb %al, (%rdi)"]

  fun emit' (IR.Off x)  = (fix_intp ("add $", "sub $") x) ^ ", %rdx"
    | emit' (IR.Loop xs) = 
      let val label = fresh ()
          val post = "p" ^ label
          val label = "L" ^ label
          val cond = "mov (%rdx), %rax\ntest %al, %al\njz " ^ post
          val jmp = "jmp " ^ label ^ "\n\n" ^ post ^ ":"
          val instrs = String.concatWith "\n" (map emit' xs)
      in String.concatWith "\n" ["\n", label ^ ":", cond, instrs, jmp] end
    | emit' IR.Print = "call print"
    | emit' IR.Read = "call read"
    | emit' (IR.Add (off, v)) = "addb $" ^ (fix_int v) ^ ", " ^ reg off 
    | emit' (IR.Sub (off, v)) = "subb $" ^ (fix_int v) ^ ", " ^ reg off 
    | emit' IR.Zero = "movb $0, (%rdx)"
    | emit' (IR.Mul (off, v)) = String.concatWith "\n" (cinstr off v "add %ecx, %eax")  
    | emit' (IR.Div (off, v)) = String.concatWith "\n" (cinstr off v "sub %ecx, %eax")  

  fun emit xs = String.concatWith "\n" (prelude @ (map emit' xs) @ epilogue)
end

functor Compiler(C : BACKEND) = struct
  val compile = C.emit o C.IR.lower o Syntax.parse
end

structure C = Compiler(Cbackend(IR));
structure A = Compiler(AMD64(IR));

val input = TextIO.inputAll TextIO.stdIn

val args = CommandLine.arguments ()
val _ = case List.find (fn x => x = "-c") args
  of SOME _ => print (C.compile input)
   | NONE   => print (A.compile input)
