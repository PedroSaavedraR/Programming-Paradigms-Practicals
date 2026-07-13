let rec mcd a b = if b=0 then a else mcd b (a mod b);;

let mcm x y =
  if x <= 0 || y <= 0 then
    raise (Failure "Los numeros deben ser mayores que 0")
  else
    let g = mcd x y in
    x/g*y;;


let mcm' x y =
  if x <= 0 || y <= 0 then
    -1
  else
    let g = mcd x y in
    if x > max_int/(y/g) then
      -1 
    else
      x/g*y;;