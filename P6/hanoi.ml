let otro ori des = 6 - ori - des

let move (ori, des) = 
    match (ori, des) with
    | (1, 2) -> "1     2 <- 3\n"
    | (1, 3) -> "1 <--2---- 3\n"
    | (2, 1) -> "1 <- 2     3\n"
    | (2, 3) -> "1     2 <- 3\n"
    | (3, 1) -> "1 ----2--> 3\n"
    | (3, 2) -> "1  -> 2    3\n"
    | _ -> "Movimiento inválido\n"

let rec hanoi n ori des = 
    (* n número de discos, 1 <= ori <= 3, 1 <= dest <= 3, ori <> des *)
    if n = 0 then "" else
    let otro = otro ori des in
    hanoi (n-1) ori otro ^ move (ori, des) ^ hanoi (n-1) otro des
    
let hanoi n ori des =
    if n = 0 || ori = des then "\n"
    else hanoi n ori des    
       
let print_hanoi n ori des =
    if n < 0 || ori < 1 || ori > 3 || des < 1 || des > 3
       then print_endline  " ** ERROR ** \n"
       else print_endline (" =========== \n" ^ 
                           hanoi n ori des ^
                           " =========== \n")

let crono f x = 
    let t = Sys.time () in
    f x; Sys.time () -. t
  

let rec nth_hanoi_move n nd ori des =
    if n <= 0 then raise (Invalid_argument "Número inválido de discos")
    else if nd < 0 || nd >= (int_of_float (2. ** float_of_int n) - 1) then
        raise (Invalid_argument "Número de movimiento fuera de rango")
    else
      let movimientos = int_of_float (2. ** float_of_int (n - 1)) in
      if nd < movimientos - 1 then
        let aux = otro ori des in
        nth_hanoi_move (n - 1) nd ori aux
      else if nd = movimientos - 1 then        
        (ori, des)
      else    
        let aux = otro ori des in
        nth_hanoi_move (n - 1) (nd - movimientos) aux des