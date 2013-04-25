open Instr

exception Finished

let soi i = string_of_int(i)
(* getters *)
let gI = function
  | Int(i) -> i
  | _ -> failwith "get integer fail"
let gS = function
  | Ptr(String s) -> s
  | _ -> failwith "get string fail"
let gH s = 
  let (a,b) = Stk.pop s
  in b

let print_machin  = function
  | Int(_) -> print_string("int\n")
  | Ptr(_) -> print_string("Ptr\n")

let binop_step b a s = 
  print_string("binop_step");
  match b with 
  | Add -> Int(gI(a) + gI(s))
  | Sub -> Int(gI(a) - gI(s))
  | Mul -> Int(gI(a) * gI(s))
  | Div -> let x = gI(s) in 
	   if(x > 0) then Int(gI(a) / x) 
	   else failwith "division par zero"
  | Eqi -> if(a = s) then Int(1) else Int(0)
  | Cat -> Ptr(String ((gS a) ^ (gS s)))
    
let print_acc = function
  | Int(i) -> print_int(i)
  | Ptr(String s) -> print_string(s)
  | _ -> failwith "print_acc ne sait pas comment lire"

let rec depop l i = if(i > 0 ) then let (a,b) = Stk.pop l in depop a (i-1)  else l    

let rec recup l i r = 
  if(i > 0 ) then 
    let (a,b) = Stk.pop l 
    in recup a (i-1) (Stk.push r b) 
  else r   

(* call by: step *)
let copy_stat c p a s = {code = c; pc = p; acc = a; stack = s}
  
(*call by: step *)
let rec bloc l i r=  
  if(i > 0 ) then 
    let (a,b) = Stk.pop l 
    in bloc a (i-1) ([|b|]) 
  else r   

(* call by: step *)    
let rec closure l i tab =
  if(i > 0 ) then 
    let (a,b) = Stk.pop l 
    in bloc a (i-1) (Array.append tab [|b|]) 
  else tab

(* call by: exec*)
(*
  on applique pour les instructions de s 
*)

let step_acc pile acc code = 
  let rec recup pill k i l = 
    (*print_string(string_of_int(gI(l.(i)))^ "\n");*)
    if(k < i) then 
      recup (Stk.push pill (l.(k))) (k+1) i l
    else
      copy_stat code (gI(l.(0))+1) acc (Stk.push pill (Stk.peek pill (i)))
  in
  match acc with
    | Ptr(Block( _, t)) -> recup pile 1 ( Array.length t) t 
    | _ -> failwith "incomprehensible"
		 
let step s =
  match s.code.(s.pc) with
    | Halt -> raise Finished 
    | Binop(b) ->    copy_stat s.code (s.pc+1) (binop_step b s.acc (gH s.stack)) s.stack
    | Const(i) ->   copy_stat s.code (s.pc+1) (Int(i)) s.stack
    | Str(st) ->    copy_stat s.code (s.pc+1) (Ptr(String st)) s.stack
    | Push ->  copy_stat s.code (s.pc+1) (s.acc) (Stk.push s.stack s.acc)
    | Acc(i) -> copy_stat s.code (s.pc+1) (Stk.peek s.stack i) (s.stack)
    | Print -> print_acc s.acc; copy_stat s.code (s.pc+1) s.acc s.stack
    | Apply -> 
      let stack = Stk.push s.stack (Int(s.pc+1)) in
      step_acc stack s.acc s.code
    | Return(i) ->  
      let st= copy_stat s.code (s.pc+1) s.acc (depop s.stack (i+1)) in
      copy_stat st.code (gI(Stk.peek st.stack 0)) st.acc (depop st.stack 1)
    | Pop(i) ->copy_stat s.code (s.pc+1) s.acc (depop s.stack i)
    | Makeblock(t, n) ->
      copy_stat 
	s.code 
	(s.pc+1) 
	(Ptr(Block(t, (bloc s.stack n (Array.make (n+1) (Int(0))))))) 
	(depop s.stack n)
    | Getblock(n) ->   copy_stat s.code (s.pc+1) (Stk.peek s.stack n) s.stack
    | Closure(n, o) -> 
      let tab = Array.make 1 (Int (o+s.pc)) in
      let close = closure s.stack n tab in  
      copy_stat 
	s.code 
	(s.pc+1) 
	(Ptr(Block(88, close))) 
	(depop s.stack n)
    | Branchif(l) ->   
      if(gI(s.acc) = 0) then 
	copy_stat s.code (s.pc+l) s.acc s.stack 
      else 
	copy_stat s.code (s.pc+1) s.acc s.stack
    | Branch(l) -> copy_stat s.code (s.pc+l) s.acc s.stack
      
(* call by: ?*)
let exec ?(trace=false) s =
  let rec star s =
    if trace then Printer.print (Printer.state s);
    try star (step s) with Finished -> ()
  in star s 

(* call by: ?*)
let init c = (*print_string(print_asm c 0 );*)
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
