let pi = 22./.7.;;

let e = 2. +. 1./.2. +. 1./.6. +. 1./.24. +. 1./.120.;;

let max_int_f =  float_of_int max_int;;

let per x =  2.*.pi*.x;;

let area x = pi *. x*.x;;

let next_char x = char_of_int (int_of_char x + 1);;

let absf = abs_float;; 

let odd x = x mod 2 <> 0;;

let next5mult x = if(x mod 5 = 0) then x+5 else (x+5-x mod 5);;

let is_letter x = let y = int_of_char x in (y > 64 && y < 91) || (y > 96 && y < 123);;

let string_of_bool x = if x then "verdadero" else "falso";;
