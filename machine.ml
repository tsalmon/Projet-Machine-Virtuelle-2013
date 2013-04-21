open Instr

exception Finished


let soi i = string_of_int(i)
let gI = function
  | Int(i) -> i
  | _ -> failwith "get integer fail"
let gS = function
  | Str(s) -> s
  | _ -> failwith "get string fail"

let binop_step b a s = match b with 
  | Add -> Int(gI(a) + gI(s))
  | Sub -> Int(gI(a) - gI(s))
  | Mul -> Int(gI(a) * gI(s))
  | Div -> let x = gI(s) in 
	   if(x > 0) then Int(a / x) 
	   else failwith "division par zero"
  | Eqi -> if(a = s) then Int(1) else Int(0)
  | Cat -> Str(gS(a) ^ gS(s))
    
let print_acc = function
  | Int(i) -> print_int(i)
  | String(s) -> print_string(s)

let step s = 
  begin
    match s.stack.(s.pc) with
      | Halt -> raise Finished 
      | Binop(b) -> binop_step b s.acc s.stack.(s.pc)
      | Const(i) -> s.acc <- Int(i) 
      | Str(s) -> s.acc <- Str(s)
      | Push -> s.stack <- Stk.push s.stack s.acc
      | Acc(i) -> s.acc <- Stk.peek s.stack i
      | Print -> print_acc s.acc
      | Apply -> m.pc <- m.pc 
      | Return(i) -> 
      | Pop(i) -> 
      | Makeblock(i1, i2) ->
      | Getblock(i) ->
      | Closure(i,l) ->
      | Branchif(l) -> 
      | Branch(l) -> 
  end; m.pc <- m.pc + 1; m
let exec ?(trace=false) s =
  print_string("Machine.exec\n");
  let rec star s =
    if trace then Printer.print (Printer.state s);
    try star (step s) with Finished -> ()
  in star s 


let init c = print_string(print_asm c 0 );
  {code =  c;
   pc = 0;
   acc = Int(0);
   stack = Stk.empty
  } (*failwith "(machine.init)Students, this is your job."*)

let print_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Eqi -> "Eqi"
  | Cat -> "Cat"
    
let rec print_asm c k = 
  if(k < Array.length c) then
    match c.(k) with 
      | Halt -> "Halt\n" ^ print_asm c (k+1)
      | Push -> "Push\n" ^ print_asm c (k+1)
      | Print -> "Print\n" ^ print_asm c (k+1)
      | Apply -> "Apply\n"
      | Acc i -> "Acc " ^ soi(i) ^ "\n" ^ print_asm c (k+1)
      | Const i -> "Const " ^ soi(i) ^ "\n" ^ print_asm c (k+1)
      | Return i -> "Return " ^ soi(i) ^ "\n" ^ print_asm c (k+1)
      | Pop i -> "Pop " ^ soi(i) ^ "\n" ^ print_asm c (k+1)
      | Branchif i -> "Branchif " ^ soi(i) ^ "\n" ^ print_asm c (k+1) 
      | Branch i -> "Branch " ^ soi(i) ^ "\n" ^print_asm c (k+1) 
      | Getblock i -> "Getblock " ^ soi (i) ^ "\n" ^ print_asm c (k+1)
      | Makeblock (i, j) -> "Makeblock" ^ soi(i) ^ ", " ^ soi(j) ^ "\n" ^ print_asm c (k+1)
      | Closure (i, j) -> "Closure" ^ soi(i) ^ ", " ^ soi(j) ^ "\n" ^ print_asm c (k+1)
      | Binop b -> "Binop" ^ print_binop b ^ "\n" ^ print_asm c (k+1)
      | Str s -> "String" ^ s ^ print_asm c (k+1)   
  else
    "\n"
