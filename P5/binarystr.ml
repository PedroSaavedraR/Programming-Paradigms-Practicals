let rec binstr_of_int n = 
  if n = 0 then ""
  else binstr_of_int (n / 2) ^ string_of_int (n mod 2)

  let int_of_binstr s =
    let n = String.length s in
    let modulo = 1 lsl (Sys.int_size - 1) in
    let rec aux i suma =
      if i >= n then suma
      else
        let bit = if s.[i] = '1' then 1 else 0 in
        let suma = (suma * 2 + bit) mod modulo in
        aux (i + 1) suma
    in
    aux 0 0

let int_of_binstr' n =
  let rec quitarespacios n =
    if String.contains n ' ' then
      let i = String.index n ' ' in
      let izquierda = String.sub n 0 i in
      let derecha = String.sub n (i + 1) (String.length n - i - 1) in
      quitarespacios (izquierda ^ derecha)
    else
      n
  in
  let rec aux s i suma =
    if i >= String.length s then suma
    else
      let digito = if s.[i] = '1' then 1 else 0 in
      aux s (i + 1) (suma * 2 + digito)
  in
  let limpiar = quitarespacios n in
  aux limpiar 0 0
    
    