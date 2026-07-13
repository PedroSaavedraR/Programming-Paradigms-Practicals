let rec mcd a b = if  b=0 then a else if a=0 then b else if a > b then mcd (a-b) b else mcd (b-a) a;;

let rec mcd' a b = if b=0 then a else mcd b (a mod b);;