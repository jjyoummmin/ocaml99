
(* https://ocaml.org/learn/tutorials/99problems.html *)

(* 1 *)
let rec last = function
  |[] -> None
  |[x] -> Some x
  |_::tl -> last tl;;

(* 2 *)
let rec last_two = function
  |[]->None
  |[x]->None
  |[x;y] -> Some (x,y)
  |hd::tl -> last_two tl;;

(* 3 *)
 let rec at n l =
    match l with
    |[] -> None
    |h::t -> if n<1 then None
             else if n=1 then Some h
             else at (n-1) t;;

(* 4 *)
let length l = List.fold_left (fun a hd -> a+1) 0 l;;

(* 5 *)
let rec rev l = List.fold_left (fun a hd -> hd::a) [] l;;

(* 6 *)
let is_palindrome l = l = List.rev l;;

(* 7 *)
type 'a node =
  | One of 'a 
  | Many of 'a node list;;

let flatten l =
   let rec add l a =
   match l with
   |[] -> []
   |(One x)::tl -> add tl a@[x]
   |(Many l')::tl -> let y = add l'[] in add tl a@y
  in add l [];;

(* 8 *)
let compress l =
  let (list, last) = List.fold_left (fun (x,y) hd -> if hd=y then (x,y) else (x@[hd], hd)) ([List.hd l], List.hd l) l in
  list;;

(* 9 *)
let pack l=
    let rec loop l inner outer=
    match l with
    |a::(b::_ as t)-> if a=b then loop t (a::inner) outer
                      else loop t [] (outer@[a::inner])
    |[a] -> outer@[a::inner]
    |[] -> []
  in loop l [] [];;

(* 10 *)
let encode l =
    let rec loop l n a =
    match l with
    |h1::(h2::_ as tl) -> if h1=h2 then loop tl (n+1) a
                          else loop tl 0 (a@[(n+1, h1)])
    |[h] -> a@[(n+1,h)]
    |[] -> a
  in loop l 0 [];;

(* 11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a;;

let encode l = 
  let l' = pack l in
  List.map (fun x -> match (List.length x) with
                     |1 -> One (List.hd x)
                     |n -> Many (n, List.hd x)) l';;

(* 12 *)
let decode l = 
  let rec loop n a acc = if n=0 then acc else loop (n-1) a (a::acc) in
  let l' = List.fold_left (fun acc x -> match x with
  | One a -> a::acc
  | Many (n, a) -> loop n a acc
  ) [] l 
  in List.rev l';;

(* 13 *)
let encode l =
  let rec loop cnt acc l = 
    match l with
    |a::(b::_ as t) -> if a=b then loop (cnt+1) acc t
                       else if cnt = 0 then loop 0 ((One a)::acc) t
                       else loop 0 (Many (cnt+1, a)::acc) t
    |[a] -> if cnt=0 then ((One a)::acc) else (Many (cnt+1, a)::acc)
    |[] -> []
    in List.rev (loop 0 [] l);;   

(* 14 *)
let duplicate l =
  let l' = List.fold_left (fun acc x -> x::x::acc) [] l in
  List.rev l';;  

(* 15 *)
(* ver1 *)
let replicate l n = 
  let rec loop acc x n =
    if n=0 then acc else loop (x::acc) x (n-1) in
  let l' = List.fold_left (fun acc x -> loop acc x n) [] l in
  List.rev l';; 

(*ver2*)
let replicate l n = 
  let rec loop x n acc = if n=0 then acc else loop x (n-1) (x::acc) in
  match l with
  |[]->[]
  |hd::tl -> loop hd n (replicate tl n);;


(* 16 *)
let rec drop l n = match l with
|[] -> []
|hd::tl -> if n=1 then tl else hd::(drop tl (n-1));;

(* 17 *)
let split l n = 
  let rec split_list front back n = 
    match back with
    |[] -> (List.rev front, back)
    |hd::tl -> if n=1 then (List.rev (hd::front), tl)
               else split_list (hd::front) tl (n-1) 
  in split_list [] l n;;


(* 18 *)
let rec slice l a b = 
  match l with
  |[]->[]
  |hd::tl -> if a>0 then slice tl (a-1) (b-1) 
             else if b=0 then [hd]
             else hd::(slice tl (a) (b-1));;


(* 19 *)
let rotate l n= 
  let rec rot l acc n = 
    match l with
    |[]->[]
    |h::t as l-> if n=0 then l@(List.rev acc)
                 else rot t (h::acc) (n-1) in 
  if n<0 then rot l [] ((List.length l)+n)
  else rot l [] n;;                             

(* 20 *)
let rec remove_at n = function
|[]->[]
|h::t -> if n=0 then t else h::(remove_at (n-1) t);;