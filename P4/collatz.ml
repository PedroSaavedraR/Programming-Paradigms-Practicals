let f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec verify n = n = 1 || verify (f n);;

let rec verify_to n = if n=0 then true else verify n && verify_to (n-1);;

let rec orbit n = let x = f n in if n > 0 && x <> 1 then (string_of_int (x)) ^ ", " ^ orbit (x) else string_of_int 1;;

let rec length n = if n = 1 then 1 else 1 + length (f n);;

let rec f n = if n mod 2 = 0 then n / 2 else 3 * n + 1;;

let rec top n = if n = 1 then 1 else max n (top (f n));;
