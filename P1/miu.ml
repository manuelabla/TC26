let n_pasos = ref 0                                           (*Referencia para el número de pasos*)


let rule_1 s =
  if String.length s = 0 then []
  else if s.[String.length s - 1] = 'I' then [s ^ "U"] 
  else []  

let rule_2 s =
  [s ^ s]

let rule_3 s =
  let len = String.length s in
  let rec loop i acc =
    if i > len - 3 then List.rev acc  
    else if s.[i] = 'I' && s.[i+1] = 'I' && s.[i+2] = 'I' then
      let l = String.sub s 0 i in
      let r = String.sub s (i + 3) (len - (i + 3)) in
      let nueva_cadena = l ^ "U" ^ r in
      loop (i + 1) (nueva_cadena :: acc)
    else 
      loop (i + 1) acc
  in
  loop 0 [] 

  let rule_4 s =
  let len = String.length s in
  let rec loop i =
    if i > len - 2 then []
    else if s.[i] = 'U' && s.[i+1] = 'U' then
    let l = String.sub s 0 i in
    let r = String.sub s (i + 2) (len - (i + 2)) in
    [l ^ r]
    else loop (i + 1)
  in
  loop 0

  let miu_builder s = 
  "M" ^ s

let solve n =
  let rec loop queue i =
    if i = n then 
      () 
    else
      match queue with
      | [] -> () 
      | actual :: queue_r ->
          print_endline ("M" ^ actual);
          let sons = rule_1 actual @ rule_2 actual @ rule_3 actual @ rule_4 actual in
          let queue_n = queue_r @ sons in
          loop queue_n (i + 1)
  in
  loop ["I"] 0

(*
  Regla 1. Si se tiene una cadena cuya última letra sea I, se le puede agregar una U al final. Dicho en otras palabras, si xI es un teorema, también lo es xIU.
  En este caso x representa cualquier cadena arbitraria. Por ejemplo, si se tiene la cadena MII, entonces se puede obtener MIIU.

  Regla 2. Suponga que Mx es un teorema. En tal caso también Mxx es un teorema. Por ejemplo, si se tiene la cadena MIU se puede obtener la cadena MIUIU.

  Regla 3. Si en una de las cadenas de la colección aparece la secuencia III, puede elaborarse una nueva cadena sustituyendo III por U. Por ejemplo, si se tiene
  la cadena UMIIIMU se puede elaborar UMUMU. Observe que las tres III deben ser consecutivas.

  Regla 4. Si aparece UU en el interior de una de las cadenas, está permitida su eliminación. Por ejemplo, dado MUUUIII se puede obtener MUIII.
*)
let main =                                                    (*Función main*)
 let usage_msg = "Uso: ./miu <numero de pasos>" in            (*Mensaje de help para uso*)
 let anon_fun s = n_pasos := int_of_string s in               (*Lectura de argumento*)
 Arg.parse [] anon_fun usage_msg;                             (*Parseo de los datos*)
 if !n_pasos > 0 then begin 
  solve !n_pasos;                                             (*Llamada a la función solve*)
  print_newline()
 end else Arg.usage [] usage_msg                              (*Mensaje de error / ayuda*)
