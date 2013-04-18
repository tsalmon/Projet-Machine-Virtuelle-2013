open Instr

exception Finished


let step s = failwith "Students, this is your job."

let exec ?(trace=false) s =
  let rec star s =
    if trace then Printer.print (Printer.state s);
    try star (step s) with Finished -> ()
  in star s

let init code = failwith "Students, this is your job."
