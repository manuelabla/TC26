type dir = Up | Down
let rec mcd x y =                                             (*Función mcd para comprobar si*)
  let rec aux a b =                                           (*una fracción es reducible    *)
    if a = 0 || b = 0 then max a b
    else aux (min a b) (a mod b)
  in aux x y
let n_pasos = ref 0                                           (*Referencia para el número de pasos*)
let u = ref false                                             (*Referencia para flag unique*)
let a = ref false                                             (*Referencia para flag -a*)
let solve n =
  let rec aux (i, j, dir) count =                             (*Función de "movimiento el la matriz"*)
    if count >= n then () 
    else  match (i, j, dir) with  
  | (1, _, Up) -> if !u && mcd (max i j) (min i j) > 1 
                  then aux (i, j+1, Down) count 
                  else begin Printf.printf "%d/%d " i j ;                   
                  aux (i, j+1, Down) (count + 1) end          (*(1, _, subiendo) ⭢*)
  | (_, 1, Down) -> if !u && mcd (max i j) (min i j) > 1 
                    then aux (i+1, j, Up) count 
                    else begin Printf.printf "%d/%d " i j ;
                    aux (i+1, j, Up) (count + 1) end          (*(_, 1, bajando) ⭣*)
  | (_, _, Up) -> if !u && mcd (max i j) (min i j) > 1 
                  then aux (i-1, j+1, Up) count 
                  else begin Printf.printf "%d/%d " i j ; 
                  aux (i-1, j+1, Up) (count + 1) end          (*(_, _, subiendo) ↗*)
  | (_, _, Down) -> if !u && mcd (max i j) (min i j) > 1 
                    then aux (i+1, j-1, Down) count 
                    else begin Printf.printf "%d/%d " i j ;
                    aux (i+1, j-1, Down) (count + 1) end      (*(_, _, bajando) ↙*)
  in aux (1, 1, Up) 0                           
  
let main =                                                    (*Función main*)
 let speclist = [                                             (*Lista de flags*)
  ("-a", Arg.Set a, "Activa modo a");
  ("-u", Arg.Set u, "Activa modo unique");
 ] in 
 let usage_msg = "Uso: ./q [-a] [-u] <numero de pasos>" in    (*Mensaje de help para uso*)
 let anon_fun s = n_pasos := int_of_string s in               (*Lectura de argumento*)
 Arg.parse speclist anon_fun usage_msg;                       (*Parseo de los datos*)
 if !n_pasos > 0 then begin 
  solve !n_pasos;                                             (*Llamada a la función solve*)
  print_newline()
 end else Arg.usage speclist usage_msg                        (*Mensaje de error / ayuda*)