let length'n'top n =
  let rec aux n orbita altura =
    if n = 1 then
      (orbita + 1, altura) 
    else
      let siguiente =
        if n mod 2 = 0 then n/2
        else 3 *n+1
      in
      aux siguiente (orbita + 1) (max altura siguiente)
  in
  aux n 0 n;;