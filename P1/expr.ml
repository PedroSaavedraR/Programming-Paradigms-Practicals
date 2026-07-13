0;;
(* - : int = 0  *)

0.;;
(* - : float = 0. *)

2 + 5 * 3;;
(* - : int = 17 *)

(* 1.5 * 2;; *)
(* Error: This expression has type float but an expression was expected of type int *)
(* Error de tipo *)

 2 + (* 5 **) 3;;
(* - : int = 5 *)

 2 + (* 5 *(*esto es un comentario anidado*)*) 3;;
(* - : int = 5 *)

(* 1.5 * 2;; *)
(* Error: This expression has type float but an expression was expected of type int *)
(* Error de tipo *)

(* 1.5 *. 2;; *)
(* This expression has type int but an expression was expected of type float *)

1.5 *. 2.;;
(* : float = 3. *)

(* 2 - 0.5;; *)
(* This expression has type float but an expression was expected of type int *)

(* 2. - 0.5;; *)
(* This expression has type float but an expression was expected of type int *)

 2. -. 2.0;;
(* - : float = 0. *)

(* 2.5 - 0.5;; *)
(* Error: This expression has type float but an expression was expected of type int *)

 2.5 -. 0.5;;
(* - : float = 2. *)

5 / 2;;
(* - : int = 2 *)

 5. /. 2.;;
(* - : float = 2.5 *)

7 mod 3;;
(* - : int = 2 *)

(* 2 ** 3;; *)
(* This expression has type int but an expression was expected of type float *)

(* 2. ** 3;; *)
(* This expression has type int but an expression was expected of type float *)

 2. ** 3.;;
(* - : float = 8. *)

2. *. 3. ** 2.;;
(* - : float = 18. *)

(2. *. 3.) ** 2.;;
(* - : float = 36. *)

2. ** 3. ** 2.;;
(* - : float = 512. *)

(2. ** 3.) ** 2.;;
(* - : float = 64. *)

sqrt 2.;;
(* - : float = 1.41421356237309515 *)

(* sqrt 4;; *)
(* This expression has type int but an expression was expected of type float *)

 sqrt 2. *. 3.;;
(* - : float = 4.24264068711928566 *)

sqrt (2. *. 3.);;
(* - : float = 2.44948974278317788 *)

2 + 1 = 23 / 7;;
(* - : bool = true *)

 sin 1. ** 2. +. cos 1. ** 2. = 1.;;
(*  - : bool = true *)

sqrt 2. ** 2. = 2.;;
(* - : bool = false *)

1 + 2 <= 3;;
(* - : bool = true *)

0.1 +. 0.2 <= 0.3;;
(* - : bool = false *)

3.0 = float_of_int 3;;
(* - : bool = true *)

int_of_float 2.1;;
(* - : int = 2 *)

(* int_of_float -2.9;; *)
(* Error: This expression has type float -> int but an expression was expected of type int *) (*Type error*)

int_of_float 2.1 + int_of_float (-2.9);;
(* - : int = 0 *)

truncate 2.1 + truncate (-2.9);;
(* - : int = 0 *)

floor 2.1 +. floor (-2.9);;
(* - : float = -1. *)

ceil 2.1 +. ceil (-2.9);;
(* - : float = 1. *)

'A';;
(* - : char = 'A' *)

'0';;
(* - : char = '0' *)

int_of_char 'A';;
(* - : int = 65 *)

char_of_int 66;;
(* - : char = 'B' *)

 Char.code 'B';;
(* - : int = 66 *)

'\067';;
(* - : char = 'C' *)

'\n';;
(* - : char = '\n' *)

Char.code '\n';;
(* - : int = 10 *)

'\010';;
(* - : char = '\n' *)

'\t';;
(* - : char = '\t' *)

Char.chr (Char.code 'M' + Char.code 'a' - Char.code 'A');;
(* - : char = 'm' *)

Char.lowercase_ascii 'M';;
(* - : char = 'm' *)

Char.uppercase_ascii 'm';;
(* - : char = 'M' *)

Char.lowercase_ascii 'm';;
(* - : char = 'm' *)

Char.uppercase_ascii '0';;
(* - : char = '0' *)

Char.lowercase_ascii '0';;
(* - : char = '0' *)

"this is a string";;
(* - : string = "this is a string" *)

"A";;
(* - : string = "A" *)

"AB";;
(* - : string = "AB" *)

(*'AB';;*)
(* Error: This expression has type char but an expression was expected of type string *)

String.length "longitud";;
(* - : int = 8 *)

(* "1999" + "1";; *)
(* Error: This expression has type string but an expression was expected of type int *)

"1999" ^ "1";;
(* - : string = "19991" *)

int_of_string "1999" + 1;;
(* - : int = 2000 *)

"\065\066";;
(* - : string = "AB" *)

"\t\n";;
(* - : string = "\t\n" *)

010 = 10;;
(* - : bool = true *)

char_of_int 010;;
(* - : char = '\n' *)

string_of_int 10;;
(* - : string = "10" *)

string_of_int 010;;
(* - : string = "8" *)

not true;;
(* - : bool = false *)

true && false;;
(* - : bool = false *)

true || false;;
(* - : bool = true *)

(1 < 2) = false;;
(* - : bool = false *)

not (1 < 2);;
(* - : bool = true *)

'1' < '2';;
(* - : bool = true *)

"1" < "2";;
(* - : bool = true *)

2 < 12;;
(* - : bool = true *)

"2" < "12";;
(* - : bool = false *)

"uno" < "dos";;
(* - : bool = true *)

if 3 = 4 then 0 else 4;;
(* - : int = 4 *)

if 3 = 4 then "0" else "4";;
(* - : string = "4" *)

(* if 3 = 4 then 0 else "4";; *)
(* Error: This expression has type string but an expression was expected of type int *)

(if 3 < 5 then 8 else 10) + 4;;
(* - : int = 12 *)

2.3;;
(* - : float = 2.3 *)

2,3;;
(* - : int * int = (2, 3) *)

(2, 3);;
(* - : int * int = (2, 3) *)

('1', '2');;
(* - : char * char = ('1', '2') *)

(1, '2');;
(* - : int * char = (1, '2') *)

1, ('a', 'b');;
(* - : int * (char * char) = (1, ('a', 'b')) *)

(true, "not false");;
(* - : bool * string = (true, "not false") *)

1, 'a', 'b';;
(* Error: This expression has type char but an expression was expected of type int *)

(1, '1', "1");;
(* - : int * char * string = (1, '1', "1") *)

("1");;
(* - : string = "1" *)

();;
(* - : unit = () *)

(());;
(* - : unit = () *)

0, ();;
(* - : int * unit = (0, ()) *)

[1; 2; 3];;
(* - : int list = [1; 2; 3] *)

[1; 1; 1];;
(* - : int list = [1; 1; 1] *)

[3; 2; 1; 0];;
(* - : int list = [3; 2; 1; 0] *)

['a'; 'b'];;
(* - : char list = ['a'; 'b'] *)

(*['a'; 2];;*)
(* Error: This expression has type int but an expression was expected of type char *)

['0'];;
(* - : char list = ['0'] *)

[1; 2; 3] @ [1; -1];;
(* - : int list = [1; 2; 3; 1; -1] *)
