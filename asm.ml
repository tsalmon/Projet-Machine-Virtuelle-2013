open Instr

(* converts integers into little endian 4-bytes strings *)
let repr_int i =
  let s = String.make 4 '\000' in
  s.[0] <- Char.chr (i mod 256);
  s.[1] <- Char.chr ((i lsr 8) mod 256);
  s.[2] <- Char.chr ((i lsr 16) mod 256);
  s.[3] <- Char.chr ((i lsr 24) mod 256);
  s

(* returns a binop's opcode *)
let binop = function
  | Add -> "\000"
  | Sub -> "\001"
  | Mul -> "\002"
  | Div -> "\003"
  | Eqi -> "\004"
  | Cat -> "\005"

(* returns an instruction's binary code *)
let instr = function
  | Halt -> "\000"
  | Push -> "\001"
  | Print -> "\002"
  | Apply -> "\003"

  | Acc i -> "\004" ^ repr_int i
  | Const i -> "\005" ^ repr_int i
  | Return i -> "\006" ^ repr_int i
  | Pop i -> "\007" ^ repr_int i
  | Branchif i -> "\008" ^ repr_int i
  | Branch i -> "\009" ^ repr_int i
  | Getblock i -> "\010" ^ repr_int i

  | Makeblock (i, j) -> "\011" ^ repr_int i ^ repr_int j
  | Closure (i, j) -> "\012" ^ repr_int i ^ repr_int j
  | Binop b -> "\013" ^ binop b
  | Str s -> "\014" ^ repr_int  (String.length s) ^ s

(* assembles the given bytecode *)
let assemble c =
  let b = Buffer.create 42 in
  Array.iter (fun i -> Buffer.add_string b (instr i)) c;
  Buffer.contents b

let refactory s = 
  if (String.length s != 4) then
    failwith "Incorrect length of string"
  else
    Char.code s.[0] + (Char.code s.[1] lsl 8) + (Char.code s.[2] lsl 16)+ (Char.code s.[3] lsl 24)

(* disassembles the given bytecode *)
(*
let disassemble s =
  let p = Array.make (String.length s) Push in
  let k = ref 0 in
  let i = ref 0 in
  while (!i) < String.length s - 1 do
    (p.(!i) <- match Char.code s.[!k] with
      | 0  -> print_string("0\n"); Halt  
      | 1  -> print_string("1\n"); Push     
      | 2  -> print_string("2\n"); Print     
      | 3  -> print_string("3\n"); Apply
      | 4  -> print_string("4\n"); Acc (refactory (String.sub s (!k+1) 5))
      | 5  -> print_string("5\n"); Const (refactory (String.sub s (!k+1) 5))
      | 6  -> print_string("6\n"); Return (refactory (String.sub s (!k+1) 5))
      | 7  -> print_string("7\n"); Pop (refactory (String.sub s (!k+1) 5))
      | 8  -> print_string("8\n"); Branchif (refactory (String.sub s (!k+1) 5))    
      | 9  -> print_string("9\n"); Branch (refactory (String.sub s (!k+1) 5))
      | 10 -> print_string("10\n"); Getblock (refactory (String.sub s (!k+1) 5))
      | 11 -> print_string("11\n"); Makeblock (refactory (String.sub s (!k+1) 5), refactory (String.sub s (!k+6) 5))
      | 12 -> print_string("12\n"); Closure(refactory (String.sub s (!k+1) 5), refactory (String.sub s (!k+6) 5))
      | 13 -> print_string("13\n");
	(match refactory (String.sub s !k 5) with 
	  | 0 -> Binop(Add)
	  | 1 -> Binop(Sub)
	  | 2 -> Binop(Mul)
	  | 3 -> Binop(Div)
	  | 4 -> Binop(Eqi)
	  | 5 -> Binop(Cat)
	  | _ -> failwith "(asm.d 1) invalide bytecode"
	)
      | 14 -> print_string("14\n"); Str("str a faire")
      | _ ->failwith "(asm.d 2)invalide bytecode");
    if(!k > 3) then
      if(!k == 11 || !k == 12) then
	k := !k + 10
      else
	k := !k + 5
    ;
    i := !i + 1
  done ; p
    *)

let desassemble_operateur = function
  | 0 -> Binop(Add)
  | 1 -> Binop(Sub)
  | 2 -> Binop(Mul)
  | 3 -> Binop(Div)
  | 4 -> Binop(Eqi)
  | 5 -> Binop(Cat)
  | _ -> failwith "(asm.d 1) invalide bytecode"

(*let rec print_asm s i =  
  if(i < String.length s) then
    "\\" ^ string_of_int(Char.code s.[i]) ^ print_asm s (i+1)
  else
    "\n"*)

let disassemble s =
  let rec aux k p = 
    print_string(string_of_int(Char.code s.[k]) ^ "\n");
    if(k+1 < String.length s) then
      match Char.code s.[k] with
	| 0  -> aux (k+1) (Halt::p)
	| 1  -> aux (k+1) (Push::p)  
	| 2  -> aux (k+1) (Print::p)   
	| 3  -> aux (k+1) (Apply::p)
	| 4  -> aux (k+4) ((Acc (refactory (String.sub s (k+1) 4)))::p)
	| 5  -> aux (k+4) ((Const (refactory (String.sub s (k+1) 4)))::p)
	| 6  -> aux (k+4) ((Return (refactory (String.sub s (k+1) 4)))::p)
	| 7  -> aux (k+4) ((Pop (refactory (String.sub s (k+1) 4)))::p)
	| 8  -> aux (k+4) ((Branchif (refactory (String.sub s (k+1) 4)))::p)
	| 9  -> aux (k+4) ((Branch (refactory (String.sub s (k+1) 4)))::p)
	| 10 -> aux (k+4) ((Getblock (refactory (String.sub s (k+1) 4)))::p)
	| 11 -> aux (k+9) ((Makeblock (refactory (String.sub s (k+1) 4), refactory (String.sub s (k+5) 4)))::p)
	| 12 -> aux (k+9) ((Closure(refactory (String.sub s (k+1) 4), refactory (String.sub s (k+5) 4)))::p)
	| 13 -> aux (k+2) ((desassemble_operateur (Char.code s.[k+1]))::p)
	| 14 -> aux (k+1+(refactory (String.sub s (k+1) 4))) ((Str("str a faire"))::p)
	| _ -> print_string(string_of_int(k) ^ "\n") ; failwith "(asm.d 2)invalide bytecode";
    else 
      p
  in Array.of_list(List.rev(aux 0 []))
