let n_pasos = ref 0                                           (*Referencia para el número de pasos*)

let string_builder x y =                                      (*Función de construcción de strings*)
  String.make x '~' ^ "m" ^
  String.make y '~' ^ "g" ^
  String.make (x + y) '~'

let solve n =                                                 (*Función de printado de strings mg*)
  let rec aux (x, y) accn =                                   
  if accn <= 0 then () 
  else match (x, y) with
  |(_ , 1) -> begin Printf.printf"%s" (string_builder x y);   (*(_, 1) -> z +1 y se empieza por (0, z)*)
              print_newline();
              aux (0, x + y + 1) (accn - 1) end
  |(_, _) -> begin Printf.printf"%s" (string_builder x y);    (*Resto de casos -> (x + 1, y - 1)*)
         print_newline();
         aux (x + 1, y - 1) (accn - 1) end 
in aux (0, 1) n                                               (*Caso Base*)

let main =                                                    (*Función main*)
 let usage_msg = "Uso: ./mg <numero de pasos>" in             (*Mensaje de help para uso*)
 let anon_fun s = n_pasos := int_of_string s in               (*Lectura de argumento*)
Arg.parse [] anon_fun usage_msg;                              (*Parseo de argumentos *)  
  if !n_pasos > 0 then begin 
    solve !n_pasos;                                           (*Llamada a la función solve*)
    print_newline ()
  end else print_endline usage_msg                            (*Mensaje de error / ayuda*)