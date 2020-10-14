(* 21 *)
let rec insert_at x n l = 
match l with
|[]->[x]
|hd::tl as l-> if n=0 then x::l else hd::(insert_at x (n-1) tl);;

(* 22 *)
(* ver1 *)
let range n m = 
  let rec inc n m = if n=m then [n] else n::inc (n+1) m in
  if n<m then inc n m else List.rev (inc m n);;

(* ver2 - tail recursive *)
let range n m =
  let rec inc n m l = 
    if n=m then n::l else inc (n+1) m (n::l) in 
  if n<m then List.rev (inc n m []) else inc m n [];; 

(* 23 *)
(* ver1. the randomly picked n elements are ordered by index of original list
for example, the result is never ["c" ; "a"; "g"] but ["a" ;"c" ;"g"] by this version 1 function *)
let rand_select l n = 
  let rec pick_idx ub n m acc = 
    if n=0 then let l' = List.sort_uniq Pervasives.compare acc in 
                let k = List.length l' in if k = m then l' else pick_idx ub (m-k) m l' 
    else let x = Random.int ub in pick_idx ub (n-1) m (x::acc) in 
  let rand_idx = pick_idx (List.length l - 1) n n [] in 

  let rec loop l m now acc = match m with
  |[]->acc
  |h::t -> if h=now then loop (List.tl l) t (now+1) ((List.hd l)::acc)
           else loop (List.tl l) m (now+1) acc
  in List.rev (loop l rand_idx 0 []);;

(* ver2 *)
let rand_select l n = 
  let rec nth_elem n front = function 
  |[]->failwith "invalid"
  |h::t -> if n=0 then (h, front@t ) else nth_elem (n-1) (h::front) t in

  let rec loop l n acc = 
    if n=0 || l=[] then acc
    else let x = Random.int (List.length l)  in
         let (elem, lst) = nth_elem x [] l in
         loop lst (n-1) (elem::acc)
  in loop l n [] ;;      

(* 24 *)
let lotto_select n m = 
  let rec loop n m acc = 
    if n=0 then acc 
    else let x = Random.int m in 
         if (List.mem x acc) then loop n m acc else loop (n-1) m (x::acc)
  in loop n m [];; 

(* 25 *)
let permutation l = 
  let rec insert_nth x n= function
  |[]->[x]
  |h::t as l -> if n=0 then x::l else h::(insert_nth x (n-1) t) in
  
  let rec loop l acc = match l with
  |[]->acc
  |h::t -> let rand = Random.int (List.length acc + 1) in
           loop t (insert_nth h rand acc)
  in loop l [] ;;  

(* 26 - combination *)
  
let extract n l=
  let f a l = List.map (fun x -> a::x) l in
  let rec comb k = function
  |[] -> []
  |h::t as l -> if k=1 then List.map (fun x -> [x]) l
                else f h (comb (k-1) t) @ (comb k t) in
  comb n l;;   
  
(* 27 *)
(* for example, one [1;2;3] => [([1], [2; 3]); ([2], [1; 3]); ([3], [1; 2])] *)
let one l =  
  let rec part front acc = function
    |[]->List.rev acc
    |h::t -> part (front@[h]) (([h], front@t)::acc) t 
  in part [] [] l;;  
  
(* extract function result => (picked list , rest list) list *)
let extract n l = 
  let rec comb k = function
  |[] -> []
  |h::t as l -> if k=1 then one l
                else let l1 = List.map (fun (p,r) -> (h::p, r))  (comb (k-1) t) in    (* 1. add h to picked list*)
                     let l2 = List.map (fun (p,r) -> (p, h::r)) (comb k t) in         (* 2. add h to rest list*)     
                       l1@l2                                                            (* concat 1 and 2 *)  
  in comb n l;;
  
  
let rec group l = function
  |[] -> failwith "invalid"
  |[x] -> List.map (fun (p,r) -> [p]) (extract x l)
  |h::t -> List.fold_left (fun acc (p, r) -> 
           let l' = List.map (fun x -> p::x ) (group r t) in acc@l' ) [] (extract h l);; 
  