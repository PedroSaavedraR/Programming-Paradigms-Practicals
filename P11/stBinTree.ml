
module BinTree = struct
  type 'a t = Empty | Node of 'a * 'a t * 'a t
end

type 'a st_bintree = 
  Leaf of 'a 
  | SBT of 'a st_bintree * 'a * 'a st_bintree

type 'a t = 'a st_bintree

let leaftree x = Leaf x

let is_leaf = function
  | Leaf _ -> true
  | SBT (_, _, _) -> false

let comb x l r =
  SBT (l, x, r)

let root = function
  | SBT (_, r, _) -> r
  | Leaf x -> x

let left_b = function
  | SBT (l, _, _) -> l
  | Leaf _ -> raise (Failure "left_b: No left subtree")

let right_b = function
  | SBT (_, _, r) -> r
  | Leaf _ -> raise (Failure "right_b: No right subtree")

let root_replacement tree new_root =
  match tree with
  | SBT (l, _, r) -> SBT (l, new_root, r)
  | Leaf _ -> Leaf new_root

let left_replacement tree new_left =
  match tree with
  | SBT (_, x, r) -> SBT (new_left, x, r)
  | Leaf _ -> raise (Failure "left_replacement")

let right_replacement tree new_right =
  match tree with
  | SBT (l, x, _) -> SBT (l, x, new_right)
  | Leaf _ -> raise (Failure "right_replacement")

let size t =
  let rec aux acc = function
     [] -> acc
    | Leaf _ :: rest -> aux (acc + 1) rest
    | SBT (l, _, r) :: rest -> aux (acc + 1) (l :: r :: rest)
  in
  aux 0 [t]

let rec height = function
   Leaf _ -> 1
  | SBT (l, _, r) -> 1 + max (height l) (height r)

let rec preorder = function
   Leaf x -> [x]
  | SBT (l, x, r) -> x :: (preorder l @ preorder r)

let rec inorder = function
   Leaf x -> [x]
  | SBT (l, x, r) -> inorder l @ [x] @ inorder r

let rec postorder = function
  | Leaf x -> [x]
  | SBT (l, x, r) -> postorder l @ postorder r @ [x]

let breadth t =
  let rec aux acc = function
     [] -> acc
    | Leaf x :: rest -> aux (acc @ [x]) rest
    | SBT (l, x, r) :: rest -> aux (acc @ [x]) (rest @ [l; r])
  in
  aux [] [t]

let rec leaves = function
   Leaf x -> [x]
  | SBT (l, _, r) -> leaves l @ leaves r

let rec find_in_depth pred = function
  | Leaf x -> 
      if pred x then x
      else raise Not_found
  | SBT (l, r, d) -> 
      if pred r then r
      else 
        try 
          find_in_depth pred l
        with Not_found -> 
          find_in_depth pred d

let find_in_depth_opt pred tree =
  try Some (find_in_depth pred tree)
  with Not_found -> None

let rec exists pred = function
  | Leaf x -> pred x
  | SBT (l, r, d) -> pred r || exists pred l || exists pred d

let rec for_all pred = function
  | Leaf x -> pred x
  | SBT (l, r, d) -> pred r && for_all pred l && for_all pred d

let rec map f = function
   Leaf x -> Leaf (f x)
  | SBT (l, r, d) -> SBT (map f l, f r, map f d)

let rec mirror = function
   Leaf x -> Leaf x
  | SBT (l, r, d) -> SBT (mirror d, r, mirror l)

let rec replace_when pred replacement = function
   Leaf x -> if pred x then replacement else Leaf x
  | SBT (l, r, d) ->
      if pred r then replacement
      else SBT (replace_when pred replacement l, r, replace_when pred replacement d)

let rec cut_below pred = function
   Leaf x -> if pred x then Leaf x else raise (Failure "cut_below")
  | SBT (l, r, d) ->
      if pred r then SBT (l, r, d)
      else raise (Failure "cut_below")
      

let rec to_bin = function
   Leaf x -> BinTree.Node (x, BinTree.Empty, BinTree.Empty)
  | SBT (l, r, d) -> BinTree.Node (r, to_bin l, to_bin d)
    
let rec from_bin = function
  BinTree.Empty -> failwith "from_bin"
  |BinTree.Node (r, BinTree.Empty, BinTree.Empty) -> Leaf r
  |BinTree.Node (r, l, d) -> SBT (from_bin l, r, from_bin d)
