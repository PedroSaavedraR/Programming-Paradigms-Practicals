
let from0to n =
  if n < 0 then raise (Failure "n debe ser >= 0")
  else 
    let rec aux acc i =
      if i = 0 then 0 :: acc
      else aux (i::acc) (i-1)
    in
    aux [] n

let to0from n =
  if n < 0 then raise (Failure "n debe ser >= 0")
  else
    let rec aux acc i =
      if i > n then acc
      else aux (i::acc) (i+1)
    in
    aux [] 0

    
let pair x l = List.map (fun y -> (x, y)) l
    
let rec remove_first x l =
  match l with
  | [] -> []                         
  | h :: t when h = x -> t
  | h :: t -> h :: remove_first x t

  
let rec remove_all x l =
  match l with
  | [] -> []
  | h :: t when h = x -> remove_all x t
  | h :: t -> h :: remove_all x t;;

  let ldif l1 l2 =
    let rec aux l1 l2 =
      match l1, l2 with
      | [], _ -> []                         
      | _, [] -> l1                         
      | h1 :: t1, h2 :: t2 when h1 = h2 -> aux t1 t2  
      | h1 :: t1, h2 :: t2 -> h1 :: aux t1 l2 
    in
    aux l1 l2
  


