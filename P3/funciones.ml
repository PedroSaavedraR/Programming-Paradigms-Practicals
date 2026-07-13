let rec sumto n = if n < 1 then 0 else n + sumto (n - 1);;

let rec exp2 n = if n = 0 then 1 else 2 * exp2 (n - 1);;

let rec num_cifras n = let n = abs n in if n < 10 then 1 else 1 + num_cifras (n / 10);;

let rec sum_cifras n = let n = abs n in if n = 0 then 0 else (n mod 10) + sum_cifras (n / 10);;
