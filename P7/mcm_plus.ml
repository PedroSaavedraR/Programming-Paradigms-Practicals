let rec mcd a b = if b=0 then a else mcd b (a mod b);;

let mcm' x y =
  if x <= 0 || y <= 0 then
    raise (Invalid_argument "mcm':  argumento  inválido  o  mcm  excede  max_int" )
  else
    let g = mcd x y in
    if x > max_int/(y/g) then
      raise (Invalid_argument "mcm':  argumento  inválido  o  mcm  excede  max_int" ) 
    else
      x/g*y;;