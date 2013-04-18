open Instr

let fun1 x = Seq [Closure (0, Seq (Acc 0 :: x))]
let fun2 x = Seq [Closure (0, Seq
  [Acc 0; Push; Closure (1, Seq (Acc 0 :: Push :: Acc 2 :: x))])]

let prim = [
  "+", fun2 [Binop Add];
  "-", fun2 [Binop Sub];
  "*", fun2 [Binop Mul];
  "/", fun2 [Binop Div];
  "=", fun2 [Binop Eqi];
  "%", fun2 [Binop Cat];
  "print", fun1 [Print; Const 0];
]

