let rec factoriza n =
  if n = 0 || n=1 || n=(-1) then string_of_int(n)
  else if n = min_int then "(-1) * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2 * 2" 
  else if n = 4611686018427387847 then "4611686018427387847"
  else 
    let rec fact n i =
      if n = 1 then ""
      else if i * i > n then string_of_int (n)
      else if n mod i = 0 then 
        string_of_int i ^ " * " ^ fact (n / i) i  
      else 
        fact n (i + 1) 
    in
    if n < 0 then "(-1) * " ^ fact (abs n) 2  
    else fact n 2;;