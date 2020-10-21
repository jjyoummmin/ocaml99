(* 31 *)
let is_prime n =
  let rec loop n m = 
    if n*n > m then true 
    else if m mod n = 0 then false
    else loop (n+1) m in
  if n=1 then false else loop 2 n;; 

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

(* 35 *)
let factors n = 
  let rec loop n m acc = 
    if n=1 then acc
    else if n mod m = 0 then loop (n/m) m (m::acc) 
    else loop n (m+1) acc in
    List.rev (loop n 2 []);;  

(* 36 *)
let factors n = 
  let rec loop n m cnt acc = 
    if n=1 then if cnt<>0 then ((m,cnt)::acc) else acc 
    else match (n mod m) with
    |0 -> loop (n/m) m (cnt+1) acc
    |_ -> if cnt<>0 then loop n (m+1) 0 ((m,cnt)::acc) else loop n (m+1) 0 acc
    in
    List.rev (loop n 2 0 []);;    

(* 37 *)
let rec n_squared a b =
  if b=0 then 1
  else a*(n_squared a (b-1));;

let phi_improved n = 
  let l = factors n in
  let rec aux acc = function
  |[] -> acc
  |(p,m)::t -> aux ((p-1)*(n_squared p (m-1))*acc) t in
  aux 1 l;;      

(* 38 *)
let timeit f n = 
  let start = Sys.time () in 
  let _ = f n in
  let fin = Sys.time () in fin -. start;;

(* 39 *)
(* eratosthenes sieve *)
let rec make_list n m acc = if n>m then List.rev acc else make_list (n+1) m (n::acc);;

let all_primes a b = 
  let l = if a<2 then make_list 2 b [] else make_list a b [] in
  let rec seive acc = function
  |[] -> List.rev acc
  |h::t -> seive (h::acc) (List.filter (fun x -> x mod h <> 0) t)
  in seive [] l;;  

(* 40 *)
let goldbach n = 
  if n<=2 || n mod 2 <> 0 then failwith "invalid input!"
  else let rec loop m =
       if n<m then failwith "Failed..."
       else if is_prime m && is_prime (n-m) then (m, n-m)
       else loop (m+1) 
       in loop 2;; 

(* 41 *)
let goldbach_list a b = 
  let rec aux acc d = if d>b then List.rev acc else aux ((d, goldbach d)::acc) (d+2) in 
  if a<=2 then aux [] 4 
  else if a mod 2 <> 0 then aux [] (a+1) 
       else aux [] a;;

let goldbach_limit a b n =
  let l = goldbach_list a b in
  List.filter (fun (_, (x,y)) -> x>n && y>n) l;;         
