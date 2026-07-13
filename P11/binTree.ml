type 'a bintree =
  | Empty
  | Node of 'a * 'a bintree * 'a bintree


type 'a t = 'a bintree


let empty = Empty

let is_empty tree =
  match tree with
  | Empty -> true
  | _ -> false

let leaftree value = Node (value, Empty, Empty)

let root tree =
  match tree with
  | Node (r, _, _) -> r
  | Empty -> failwith "root"

let left_b tree =
  match tree with
  | Node (_, i, _) -> i
  | Empty -> failwith "left_b"

let right_b tree =
  match tree with
  | Node (_, _, d) -> d
  | Empty -> failwith "right_b"

let root_replacement tree x =
  match tree with
  | Node (_, i, d) -> Node (x, i, d)
  | Empty -> failwith "root_replacement"

let left_replacement tree i =
  match tree with
  | Node (r, _, d) -> Node (r, i, d)
  | Empty -> failwith "left_replacement"

let right_replacement tree d =
  match tree with
  | Node (r, i, _) -> Node (r, i, d)
  | Empty -> failwith "right_replacement"

let rec size tree =
  match tree with
  | Empty -> 0
  | Node (_, i, d) -> 1 + size i + size d

let rec height tree =
  match tree with
  | Empty -> 0
  | Node (_, i, d) -> 1 + max (height i) (height d)

let rec preorder tree =
  match tree with
  | Empty -> []
  | Node (r, i, d) -> r :: (preorder i @ preorder d)

let rec inorder tree =
  match tree with
  | Empty -> []
  | Node (r, i, d) -> inorder i @ [r] @ inorder d

let rec postorder tree =
  match tree with
  | Empty -> []
  | Node (r, i, d) -> postorder i @ postorder d @ [r]

let breadth tree =
  let rec aux queue acc =
    match queue with
    | [] -> List.rev acc
    | Empty :: t -> aux t acc
    | Node (r, i, d) :: t -> aux (t @ [i; d]) (r :: acc)
  in
  aux [tree] []

let rec leaves tree =
  match tree with
  | Empty -> []
  | Node (r, Empty, Empty) -> [r]
  | Node (_, i, d) -> leaves i @ leaves d

let rec find_in_depth pred tree =
  match tree with
  | Empty -> raise Not_found
  | Node (r, i, d) ->
      if pred r then r
      else
        try find_in_depth pred i
        with Not_found -> find_in_depth pred d

let find_in_depth_opt pred tree =
  try Some (find_in_depth pred tree) with Not_found -> None

let rec exists pred tree =
  match tree with
  | Empty -> false
  | Node (r, i, d) -> pred r || exists pred i || exists pred d

let rec for_all pred tree =
  match tree with
  | Empty -> true
  | Node (r, i, d) -> pred r && for_all pred i && for_all pred d

let rec map f tree =
  match tree with
  | Empty -> Empty
  | Node (r, i, d) -> Node (f r, map f i, map f d)

let rec mirror tree =
  match tree with
  | Empty -> Empty
  | Node (r, i, d) -> Node (r, mirror d, mirror i)

let rec replace_when pred tree replacement =
  match tree with
  | Empty -> Empty
  | Node (r, i, d) ->
      if pred r then replacement
      else Node (r, replace_when pred i replacement, replace_when pred d replacement)

let rec cut_above pred tree =
  match tree with
  | Empty -> Empty
  | Node (r, i, d) -> if pred r then Empty else Node (r, cut_above pred i, cut_above pred d)

let rec cut_below pred tree =
  match tree with
  | Empty -> Empty
  | Node (r, i, d) ->
      if pred r then Node (r, Empty, Empty)
      else Node (r, cut_below pred i, cut_below pred d)
