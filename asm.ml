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
  | Str s -> "\014" ^ repr_int (String.length s) ^ s

(* assembles the given bytecode *)
let assemble c =
  let b = Buffer.create 42 in
  Array.iter (fun i -> Buffer.add_string b (instr i)) c;
  Buffer.contents b


(* disassembles the given bytecode *)
let disassemble s = failwith "Students, this is your job."
