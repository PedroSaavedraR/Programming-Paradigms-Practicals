let length l =
  let count = ref 0 in
  let current = ref l in
  while !current <> [] do
    count := !count + 1;
    current := List.tl !current
  done;
  !count

let last l =
  if l = [] then failwith "last"
  else
    let current = ref l in
    while List.tl !current <> [] do
      current := List.tl !current
    done;
    List.hd !current

let nth l n =
  if n < 0 then invalid_arg "nth"
  else
    let current = ref l in
    let index = ref 0 in
    while !index < n do
      match !current with
      | [] -> invalid_arg "nth"
      | _ :: t -> 
          current := t;
          index := !index + 1
    done;
    match !current with
     [] -> invalid_arg "nth"
    | h :: _ -> h

let rev l =
  let result = ref [] in
  let current = ref l in
  while !current <> [] do
    result := List.hd !current :: !result;
    current := List.tl !current
  done;
  !result

let append l1 l2 =
  let result = ref l2 in
  let current = ref (rev l1) in
  while !current <> [] do
    result := List.hd !current :: !result;
    current := List.tl !current
  done;
  !result

let concat l =
  let result = ref [] in
  let current = ref (rev l) in
  while !current <> [] do
    result := append (List.hd !current) !result;
    current := List.tl !current
  done;
  !result

let for_all p l =
  let result = ref true in
  let current = ref l in
  while !result && !current <> [] do
    result := p (List.hd !current);
    current := List.tl !current
  done;
  !result

let exists p l =
  let result = ref false in
  let current = ref l in
  while (not !result) && !current <> [] do
    result := p (List.hd !current);
    current := List.tl !current
  done;
  !result

let find_opt p l =
  let result = ref None in
  let current = ref l in
  while !result = None && !current <> [] do
    if p (List.hd !current) then
      result := Some (List.hd !current)
    else
      current := List.tl !current
  done;
  !result

let iter f l =
  let current = ref l in
  while !current <> [] do
    f (List.hd !current);
    current := List.tl !current
  done

let fold_left f acc l =
  let result = ref acc in
  let current = ref l in
  while !current <> [] do
    result := f !result (List.hd !current);
    current := List.tl !current
  done;
  !result