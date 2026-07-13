let append a1 a2 =
  let len1 = Array.length a1 in
  let len2 = Array.length a2 in
  let result = Array.make (len1 + len2) a1.(0) in
  for i = 0 to len1 - 1 do
    result.(i) <- a1.(i)
  done;
  for i = 0 to len2 - 1 do
    result.(len1 + i) <- a2.(i)
  done;
  result

let sub a start len =
  if start < 0 || len < 0 || start + len > Array.length a then
    invalid_arg "sub"
  else
    let result = Array.make len a.(0) in
    for i = 0 to len - 1 do
      result.(i) <- a.(start + i)
    done;
    result

let copy a =
  let len = Array.length a in
  let result = Array.make len a.(0) in
  for i = 0 to len - 1 do
    result.(i) <- a.(i)
  done;
  result

let fill a start len x =
  if start < 0 || len < 0 || start + len > Array.length a then
    invalid_arg "fill"
  else
    for i = start to start + len - 1 do
      a.(i) <- x
    done

let blit src src_pos dst dst_pos len =
  if src_pos < 0 || dst_pos < 0 || len < 0 ||
     src_pos + len > Array.length src ||
     dst_pos + len > Array.length dst then
    invalid_arg "blit"
  else
    for i = 0 to len - 1 do
      dst.(dst_pos + i) <- src.(src_pos + i)
    done

let to_list a =
  let result = ref [] in
  for i = Array.length a - 1 downto 0 do
    result := a.(i) :: !result
  done;
  !result

let iter f a =
  for i = 0 to Array.length a - 1 do
    f a.(i)
  done

let fold_left f acc a =
  let result = ref acc in
  for i = 0 to Array.length a - 1 do
    result := f !result a.(i)
  done;
  !result

let for_all p a =
  let result = ref true in
  let i = ref 0 in
  while !result && !i < Array.length a do
    result := p a.(!i);
    i := !i + 1
  done;
  !result

let exists p a =
  let result = ref false in
  let i = ref 0 in
  while (not !result) && !i < Array.length a do
    result := p a.(!i);
    i := !i + 1
  done;
  !result

let find_opt p a =
  let result = ref None in
  let i = ref 0 in
  while !result = None && !i < Array.length a do
    if p a.(!i) then
      result := Some a.(!i)
    else
      i := !i + 1
  done;
  !result