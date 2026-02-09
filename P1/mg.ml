let n_pasos = ref 0                                                       (*Referencia para el número de pasos*)
let has_checked = ref false                                               (*Flag para comprobar si se ha hecho check*)      

let string_builder x y =                                                  (*Función de construcción de strings*)
  String.make x '~' ^ "m" ^
  String.make y '~' ^ "g" ^
  String.make (x + y) '~'

let solve n =                                                             (*Función de printado de strings mg*)
  let rec aux (x, y) accn =                                   
  if accn <= 0 then () 
  else match (x, y) with
  |(_ , 1) -> begin Printf.printf"%s" (string_builder x y);               (*(_, 1) -> z + 1 y se empieza por (0, z)*)
              print_newline();
              aux (0, x + y + 1) (accn - 1) end
  |(_, _) -> begin Printf.printf"%s" (string_builder x y);                (*Resto de casos -> (x + 1, y - 1)*)
         print_newline();
         aux (x + 1, y - 1) (accn - 1) end 
in aux (0, 1) n                                                           (*Caso Base*)

let check s =
  let l = Str.split_delim(Str.regexp "[mg]") s in
  match l with 
  |[x; y; z] -> if String.length x >= 0 
                && String.length x < String.length z 
                && String.length y > 0 
                && String.length y <= String.length z 
                && String.length z == (String.length x + String.length y) 
                then Printf.printf "yes" else Printf.printf "no"
  | _ -> Printf.printf "no"

  let main =                                                              (*Función main*)
 let usage_msg = "Uso: ./mg <numero de pasos | string xm yg z>"           (*Mensaje de help para uso*)
in let anon_fun s =                                                       (*Lectura de argumento*)
  try n_pasos := int_of_string s 
 with Failure _ -> begin check s;
                   has_checked := true;
                   print_newline() end
in Arg.parse [] anon_fun usage_msg;                                       (*Parseo de argumentos *)  
  if !n_pasos > 0 then begin 
    solve !n_pasos;                                                       (*Llamada a la función solve*)
    print_newline ()
  end else if not !has_checked 
           then print_endline usage_msg                                   (*Mensaje de error / ayuda*)