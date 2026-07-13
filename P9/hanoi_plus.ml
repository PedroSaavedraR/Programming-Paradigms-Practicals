let is_stable (l1, l2, l3) =
  let rec is_sorted l =
    match l with
    | [] | [_] -> true
    | x :: y :: rest -> x < y && is_sorted (y :: rest)
  in
  let combined = List.sort compare (l1 @ l2 @ l3) in
  let rec secuencia l n =
    match l with
    | [] -> true
    | h :: t -> h = n && secuencia t (n + 1)
  in
  is_sorted l1 && is_sorted l2 && is_sorted l3 &&
  secuencia combined 1

  let all_states n =
    let rec partitions l =
      match l with
      | [] -> [ ([], [], []) ]
      | h :: t ->
        let rest_partitions = partitions t in
        List.flatten
          (List.map
             (fun (a, b, c) ->
               [ (h :: a, b, c); (a, h :: b, c); (a, b, h :: c) ])
             rest_partitions)
    in
  
    let discos = List.init n (fun i -> i + 1) in
    List.filter
      (fun (a, b, c) ->
        List.for_all (fun l -> l = List.sort compare l) [ a; b; c ])
      (partitions discos)
  
  
type move = LtoC | LtoR | CtoL | CtoR | RtoL | RtoC

let move (i, c, d) movimiento =
  let is_stable torre =
    torre = List.sort compare torre
  in
  if not (is_stable i && is_stable c && is_stable d) then
    raise (Invalid_argument "mover")
  else
    let mover_disco desde_hasta hacia_torre =
      match desde_hasta with
      | [] -> raise (Invalid_argument "mover")
      | disco_superior :: resto_desde ->
        (match hacia_torre with
         | [] -> (resto_desde, [disco_superior])
         | destino_superior :: _ ->
           if disco_superior < destino_superior then
             (resto_desde, disco_superior :: hacia_torre)
           else
             raise (Invalid_argument "mover"))
    in
    match movimiento with
    | LtoC ->
      let nueva_i, nueva_c = mover_disco i c in
      (nueva_i, nueva_c, d)
    | LtoR ->
      let nueva_i, nueva_d = mover_disco i d in
      (nueva_i, c, nueva_d)
    | CtoL ->
      let nueva_c, nueva_i = mover_disco c i in
      (nueva_i, nueva_c, d)
    | CtoR ->
      let nueva_c, nueva_d = mover_disco c d in
      (i, nueva_c, nueva_d)
    | RtoL ->
      let nueva_d, nueva_i = mover_disco d i in
      (nueva_i, c, nueva_d)
    | RtoC ->
      let nueva_d, nueva_c = mover_disco d c in
      (i, nueva_c, nueva_d)

let move_sequence ini movimientos =
  let rec aplicar_secuencia estado movimientos =
  match movimientos with
    | [] -> estado
    | movimiento :: resto ->
      let nuevo_estado = move estado movimiento in
      aplicar_secuencia nuevo_estado resto
  in
  aplicar_secuencia ini movimientos
      

