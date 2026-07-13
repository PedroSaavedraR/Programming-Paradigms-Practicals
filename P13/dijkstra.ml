open MinPrioQueue

let dijkstra w i j =
  let n = Array.length w in
  
  if n = 0 || Array.length w.(0) <> n then
    raise (Invalid_argument "dijkstra");
  if i < 0 || i >= n || j < 0 || j >= n then
    raise (Invalid_argument "dijkstra");
  
  Array.iter (fun row ->
    Array.iter (function
       Some weight when weight < 0 -> raise (Invalid_argument "dijkstra")
      | _ -> ()
    ) row
  ) w;
  
  let dist = Array.make n max_int in
  let prev = Array.make n None in
  let visited = Array.make n false in
  
  dist.(i) <- 0;
  let q = ref (insert empty 0 i) in
  
  let rec loop () =
    match extract !q with
     None -> ()
    | Some (_, u, q') ->
        q := q';
        if not visited.(u) then begin
          visited.(u) <- true;
          if u = j then () else
          for v = 0 to n - 1 do
            match w.(u).(v) with
             Some weight ->
                let alt = dist.(u) + weight in
                if alt < dist.(v) then begin
                  dist.(v) <- alt;
                  prev.(v) <- Some u;
                  q := insert !q alt v
                end
            | None -> ()
          done;
          loop ()
        end else loop ()
  in
  loop ();
  
  if not visited.(j) then None
  else
    let rec build_path node acc =
      let acc' = node :: acc in
      if node = i then acc'
      else build_path (Option.get prev.(node)) acc'
    in
    Some (dist.(j), build_path j [])
