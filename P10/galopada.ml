exception No_encontrado
exception Argumento_invalido of string

let es_coordenada_valida n (x, y) =
  x >= 1 && x <= n && y >= 1 && y <= n


let movimientos_caballo (x, y) =
  [(x + 2, y + 1); (x + 2, y - 1); (x - 2, y + 1); (x - 2, y - 1);
   (x + 1, y + 2); (x + 1, y - 2); (x - 1, y + 2); (x - 1, y - 2)]


let rec encontrar_secuencia n peones visitados pos =
  match peones with
  | [] -> [pos]
  | _ ->
      let movimientos_validos =
        List.filter (fun m ->
          es_coordenada_valida n m && List.mem m peones && not (List.mem m visitados))
        (movimientos_caballo pos)
      in
      let rec intentar_movimientos movimientos =
        match movimientos with
        | [] -> raise No_encontrado
        | m :: ms ->
            try
              pos :: encontrar_secuencia n (List.filter ((<>) m) peones) (m :: visitados) m
            with No_encontrado -> intentar_movimientos ms
      in
      intentar_movimientos movimientos_validos


let galopada n peones =
  if n < 1 then raise (Argumento_invalido "galopada");
  if List.exists (fun p -> not (es_coordenada_valida n p)) peones then
    raise (Argumento_invalido "galopada");
  if List.length peones <> List.length (List.sort_uniq compare peones) then
    raise (Argumento_invalido "galopada");

  let rec buscar_posicion_inicial posiciones =
    match posiciones with
    | [] -> raise No_encontrado
    | pos :: ps ->
        try encontrar_secuencia n peones [] pos with No_encontrado -> buscar_posicion_inicial ps
  in
  buscar_posicion_inicial (List.init (n * n) (fun i -> (i / n + 1, i mod n + 1)))
