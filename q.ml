type dir =
| Up  
| Down

let solve i =
  let rec aux (i, j, dir) count =                             (*Función de "movimiento el la matriz"*)
    if count >= i then
      () 
    else match (i, j, dir) with  
  | (1, _, Up) -> aux (i, j+1, Down) (count + 1)              (*(1, _, subiendo) ⭢*)
  | (_, 1, Down) -> aux (i+1, j, Up) (count + 1)              (*(_, 1, bajando) ⭣*)
  | (_, _, Up) -> aux (i+1, j+1, Up) (count + 1)              (*(_, _, subiendo) ↗*)
  | (_, _, Down) -> aux (i+1, j-1, Down) (count + 1)          (*(_, _, bajando) ↘*)
  in aux (1, 1, Up) 0                               
  

(*let parse =                                                 (*Parseador para el bash*)
    if Array.length (Sys.argv) = 2 
    then print_string(loop (int_of_string(Sys.argv.(1))) (1, 1, Up))
    else print_endline("Invalid number of arguments") ;*)