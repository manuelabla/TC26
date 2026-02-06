type dir =
| Up  
| Down

let solve n =
  let rec aux (i, j, dir) count =                             (*Función de "movimiento el la matriz"*)
    if count >= n then
      () 
    else match (i, j, dir) with  
  | (1, _, Up) ->begin Printf.printf "%d/%d " i j ; 
                 aux (i, j+1, Down) (count + 1)  end                  (*(1, _, subiendo) ⭢*)
  | (_, 1, Down) ->begin Printf.printf "%d/%d " i j ;
                   aux (i+1, j, Up) (count + 1) end                   (*(_, 1, bajando) ⭣*)
  | (_, _, Up) ->begin Printf.printf "%d/%d " i j ; 
                 aux (i-1, j+1, Up) (count + 1) end                   (*(_, _, subiendo) ↗*)
  | (_, _, Down) ->begin Printf.printf "%d/%d " i j ;
                   aux (i+1, j-1, Down) (count + 1) end               (*(_, _, bajando) ↘*)
  in aux (1, 1, Up) 0                               
  
let parse =                                                 (*Parseador para el bash*)
    if Array.length (Sys.argv) = 2 
    then solve (int_of_string(Sys.argv.(1)))
    else print_endline("Invalid number of arguments") ;