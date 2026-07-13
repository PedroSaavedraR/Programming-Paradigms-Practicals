let mcd_pasos a b =
  let rec aux a b i =
    if b = 0 then
      (a, i)
    else if a = b then
      (a, i + 1)
    else
      aux b (a mod b) (i + 1)
  in
  aux a b 0;;