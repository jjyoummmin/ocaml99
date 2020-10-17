(* 31 *)
let is_prime n =
  let rec loop n m = 
    if n>m then true 
    else if m mod n = 0 then false
    else loop (n+1) m in
  if n=1 then false else loop 2 (int_of_float (sqrt (float_of_int n)));;  

(* 32 *)
let rec gcd a b = 
  if b=0 then a else gcd b (a mod b);;

(* 33 *)
let coprime a b = gcd a b = 1;;  

(* 34 *)
let phi m = 
  let rec loop count r m =
    if r=m then count
    else if coprime r m then loop (count+1) (r+1) m
    else loop count (r+1) m in
  if m=1 then 1 else loop 0 1 m;;
