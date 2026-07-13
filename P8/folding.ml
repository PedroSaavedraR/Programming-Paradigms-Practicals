(* Implementaciones usando List.fold_left *)

let i_prod l =
  List.fold_left ( * ) 1 l


let f_prod l =
  List.fold_left ( *. ) 1.0 l


let lmin l =
  match l with
  | [] -> failwith "Empty list"
  | h :: t -> List.fold_left min h t


let min_max l =
  match l with
  | [] -> failwith "Empty list"
  | h :: t -> List.fold_left (fun (mi, ma) x -> (min mi x, max ma x)) (h, h) t


let rev l =
  List.fold_left (fun acc x -> x :: acc) [] l


let rev_append l1 l2 =
  List.fold_left (fun acc x -> x :: acc) l2 (rev l1)

let rev_map f l =
  List.fold_left (fun acc x -> (f x) :: acc) [] l


let concat sl =
  List.fold_left ( ^ ) "" sl
