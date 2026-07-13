(* hd : 'a list -> 'a *)
let hd = function
  | [] -> raise (Failure "hd")  
  | h :: _ -> h

(* tl : 'a list -> 'a list *)
let tl = function
  | [] -> raise (Failure "tl")  
  | _ :: t -> t

(* last : 'a list -> 'a *)
let rec last = function
  | [] -> raise (Failure "last")
  | [x] -> x
  | _::t -> last t;;

(* length : 'a list -> int *)
let rec length l = 
  if l = [] then 0
  else 1 + length (List.tl l);;

(* length' : 'a list -> int *)
let length' l =
  let rec suma l i =
    match l with
    |[] -> i
    |_ :: tl -> suma tl(i+1)
  in
  suma l 0

(* compare_lengths : 'a list -> 'a list -> int *)
let rec compare_lengths l1 l2 =
  match l1, l2 with
    [], [] -> 0
    | [], _ -> -1
    | _ , [] -> 1
    | _::t1 , _::t2 -> compare_lengths t1 t2;;

(* append : 'a list -> 'a list -> 'a list *)
let rec append l1 l2 =
match l1 with 
[] -> l2 
| h::t -> h :: append t l2

(* rev_append : 'a list -> 'a list -> 'a list *)
let rev_append l1 l2 =
  let rec aux l1 acc =
    match l1 with
    | [] -> acc  
    | h :: tl -> aux tl (h :: acc) 
  in
  aux l1 l2

(* rev : 'a list -> 'a list *)
let rev l =
  let rec aux acc = function
  [] -> acc
  |h::t -> aux (h::acc) t
in
  aux [] l

(* concat : 'a list list -> 'a list *)
let rec concat = function
  | [] -> []    
  | h :: t -> h @ concat t;;

(* flatten : 'a list list -> 'a list *)
let rec flatten = function
  | [] -> []                     
  | h :: t -> h @ flatten t;;

(* init : int -> (int -> 'a) -> 'a list *)
let rec init len f = 
   if len < 0 then raise (Failure "Invalid argument")
   else if len = 0 then []
   else let rec aux n f =
    if n= len then []
    else (f n) :: aux (n+1) f
  in aux 0 f 

(* nth : 'a list -> int -> 'a *)
let nth lst n =
  if n < 0 then raise (Invalid_argument "Negative index") 
  else
    let rec aux i = function
      | [] -> raise (Failure "List is too short")  
      | h :: t -> if i = n then h               
                  else aux (i + 1) t           
    in
    aux 0 lst 

(* map : ('a -> 'b) -> 'a list -> 'b list *)
let rec map f l =
  match l with
  | [] -> []                    
  | h :: t -> f h :: map f t;;

(* rev_map : ('a -> 'b) -> 'a list -> 'b list *)
let rev_map f lst =
  let rec aux acc = function
    | [] -> acc                         
    | h :: t -> aux ((f h) :: acc) t     
  in
  aux [] lst                               

(* map2 : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list *)
let rec map2 f l1 l2 =
    match l1, l2 with
    | [], [] -> []                            
    | h1 :: t1, h2 :: t2 -> (f h1 h2) :: map2 f t1 t2  
    | _ -> raise (Invalid_argument "Lists have different lengths")

(* combine : 'a list -> 'b list -> ('a * 'b) list *)
let rec combine l1 l2 =
  match (l1, l2) with
  | ([], []) -> []
  | ([], _) | (_, []) -> raise (Invalid_argument "combine")
  | (h1::t1, h2::t2) -> (h1, h2) :: combine t1 t2;;

(* split : ('a * 'b) list -> 'a list * 'b list *)
let rec split = function
  | [] -> ([], [])
  | (x, y)::t ->
      let (l1, l2) = split t in
      (x::l1, y::l2);;

(* find : ('a -> bool) -> 'a list -> 'a *)
let rec find f = function
  | [] -> raise Not_found
  | h::t -> if f h then h else find f t;;

(* filter : ('a -> bool) -> 'a list -> 'a list *)
let rec filter f = function
  | [] -> []
  | h::t -> if f h then h :: filter f t else filter f t;;

(* filter' : ('a -> bool) -> 'a list -> 'a list *)
let filter' f l =
  let rec aux acc = function
    | [] -> rev acc
    | h::t -> aux (if f h then h::acc else acc) t
  in aux [] l;;

(* partition : ('a -> bool) -> 'a list -> 'a list * 'a list *)
let rec partition f = function
  | [] -> ([], [])
  | h::t ->
      let (l1, l2) = partition f t in
      if f h then (h::l1, l2) else (l1, h::l2);;

(* partition' : ('a -> bool) -> 'a list -> 'a list * 'a list *)
let partition' f l =
  let rec aux (l1, l2) = function
    | [] -> (List.rev l1, List.rev l2)
    | h::t -> if f h then aux (h::l1, l2) t else aux (l1, h::l2) t
  in aux ([], []) l;;

(* for_all : ('a -> bool) -> 'a list -> bool *)
let for_all f l =
  let rec aux acc = function
    | [] -> acc
    | h::t -> if f h then aux acc t else false
  in
  aux true l

(* exists : ('a -> bool) -> 'a list -> bool *)
let exists f l =
  let rec aux = function
    | [] -> false
    | h::t -> if f h then true else aux t
  in
  aux l

(* mem : 'a -> 'a list -> bool *)
let mem x l =
  let rec aux = function
    | [] -> false
    | h::t -> if h = x then true else aux t
  in
  aux l

(* fold_left : ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a *)
let rec fold_left f acc = function
  | [] -> acc
  | h::t -> fold_left f (f acc h) t;;

(* fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f l acc =
  match l with
  | [] -> acc
  | h::t -> f h (fold_right f t acc);;