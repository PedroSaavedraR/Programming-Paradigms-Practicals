
let concat l =
  let rec aux acc = function
    | [] -> List.rev acc
    | h :: t -> aux (List.rev_append h acc) t
  in
  aux [] l

(* append necesita recorrer una lista entera para realizar su funcion, mientras que rev_append combina dos listas invirtiendo la primera lista mientras la concatena a la segunda *)  


let front l =
  let rec aux acc = function
    | [] -> raise (Failure "front")
    | [_] -> List.rev acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] l


let compress l =
  let rec aux acc = function
    | [] -> List.rev acc
    | [h] -> List.rev (h :: acc)
    | h1 :: (h2 :: _ as t) ->
      if h1 = h2 then aux acc t
      else aux (h1 :: acc) t
  in
  aux [] l
  

  let ofo l =
    let rec aux seen l acc =
      match l with
      | [] -> List.rev acc
      | h :: t when List.mem h seen -> aux seen t acc
      | h :: t -> aux (h :: seen) t (h :: acc)
    in
    aux [] l []