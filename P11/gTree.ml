type 'a gtree = GT of 'a * 'a gtree list

type 'a t = 'a gtree

let leaftree x = GT (x, [])

let root (GT (x, _)) = x

let branches (GT (_, lr)) = lr

let rec size (GT (_, lr)) = 1 + List.fold_left (fun acc t -> acc + size t) 0 lr

let rec height (GT (_, lr)) =
  match lr with
  | [] -> 1
  | _ -> 1 + List.fold_left (fun acc t -> max acc (height t)) 0 lr

let rec preorder (GT (x, lr)) =
  x :: List.fold_left (fun acc t -> acc @ preorder t) [] lr

let rec postorder (GT (x, lr)) =
  List.fold_left (fun acc t -> acc @ postorder t) [] lr @ [x]

let breadth tree =
  let rec aux cola =
    match cola with
    | [] -> []
    | GT (x, lr) :: tail -> x :: aux (tail @ lr)
  in
  aux [tree]

let rec leaves (GT (x, lr)) =
  if lr = [] then [x]
  else List.fold_left (fun acc t -> acc @ leaves t) [] lr

let rec find_in_depth p = function
  | GT (x, []) -> 
      if p x then x
      else raise Not_found
  | GT (x, tList) ->
    if p x then x
    else 
      List.fold_left (fun acc t -> 
        try find_in_depth p t
        with Not_found -> acc
      ) (raise Not_found) tList


let rec find_in_depth_opt pred (GT (x, c)) =
  if pred x then Some x
  else 
    let rec aux = function
      | [] -> None
      | h :: t ->
          match find_in_depth_opt pred h with
          | Some result -> Some result
          | None -> aux t
    in
    aux c      
    

let rec breadth_find pred tree =
  let rec aux cola =
    match cola with
    | [] -> raise Not_found
    | GT (x, lr) :: tail ->
        if pred x then x
        else aux (tail @ lr)
  in
  aux [tree]

let rec breadth_find_opt pred tree =
  let rec aux cola =
    match cola with
    | [] -> None
    | GT (x, lr) :: tail ->
        if pred x then Some x
        else aux (tail @ lr)
  in
  aux [tree]

let exists pred tree =
  let rec aux (GT (x, lr)) =
    if pred x then true
    else List.exists aux lr
  in
  aux tree

let for_all pred tree =
  let rec aux (GT (x, lr)) =
    pred x && List.for_all aux lr
  in
  aux tree

let rec map f (GT (x, lr)) =
  GT (f x, List.map (map f) lr)

let rec mirror (GT (x, lr)) =
  GT (x, List.map mirror (List.rev lr))

let rec replace_when pred (GT (x, lr)) replacement =
  if pred x then replacement
  else GT (x, List.map (fun t -> replace_when pred t replacement) lr)

let rec cut_below pred (GT (x, lr)) =
  if pred x then GT (x, [])
  else GT (x, List.map (cut_below pred) lr)

let from_bin bt =
  match bt with
  | BinTree.Leaf -> raise (Failure "from_bin")
  | BinTree.Node (x, left, right) -> 
      GT (x, [from_bin left; from_bin right])

let from_stbin stbt =
  match stbt with
  | StBinTree.Leaf -> raise (Failure "from_stbin")
  | StBinTree.Node (x, left, right) -> 
      GT (x, [from_stbin left; from_stbin right])
