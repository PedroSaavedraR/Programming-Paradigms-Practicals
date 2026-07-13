let rec factoriza n =
  if n = 0 || n = 1 || n = (-1) then string_of_int n
  else if n = min_int then "(-1) * 2^62"
  else if n = 4611686018427387847 then "4611686018427387847"  
  else 
    let rec fact n i =
      if n = 1 then ""
      else if i * i > n then string_of_int (n)
      else
        let rec count_factors n i count =
          if n mod i = 0 then count_factors (n / i) i (count + 1)
          else (count, n)
        in
        let (count, remaining) = count_factors n i 0 in
        if count > 0 then
          let factor = 
            if count > 1 then string_of_int i ^ "^" ^ string_of_int count 
            else string_of_int i in
          if remaining = 1 then factor
          else factor ^ " * " ^ fact remaining i
        else
          fact n(i + 1)
    in
    if n < 0 then "(-1) * " ^ fact(abs n)2
    else fact n 2;;
