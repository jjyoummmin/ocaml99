(* 46 & 47 *)
let rec cartesian l1 l2 = match l1 with
  |[] -> []
  |[x] -> List.map (fun y -> [x,y]) l2
  |h::t -> let node_list = List.map (fun y -> h,y) l2 in
           let l' = List.map (fun node -> List.map (fun x -> node::x) (cartesian t l2)) node_list in
           List.flatten l';;

(* example of env => [("a", true); ("b", false)]*)
let rec eval env = function
| Var x -> List.assoc x env
| Not e -> not (eval env e)
| And (e1, e2) -> (eval env e1) && (eval env e2)
| Or (e1, e2) -> (eval env e1) || (eval env e2);;           

let table2 v1 v2 exp = 
  let env_list = cartesian [v1;v2] [true;false] in
  List.map (fun env -> match env with
                       (v1,b1)::(v2,b2)::[] -> (b1,b2, eval env exp)
                       |_ -> failwith "invalid env" ) env_list;;

(* 48 *)
let table var_list exp = 
  let env_list = cartesian var_list [true;false] in
  List.map (fun env -> env, eval env exp) env_list;;                       

(* 49 - gray code *)
let rec double = function
|[] -> []
|h::t -> h::h::(double t);;

let lst n=
  let rec aux n acc = if n=1 then acc else aux (n-1) (acc@acc) 
  in aux (n-1) ["0";"1";"1";"0"];;

let rec gray n = 
  if n = 1 then ["0"; "1"]
  else let l1 = double (gray (n-1)) in
       let l2 = lst n in
       List.map2 (fun x y -> x^y) l1 l2;;  

(* 50 *)
type htree = Leaf of int*string | Node of int*htree*htree;;

let huffman fs = 
  let num = function |Leaf (x,_) |Node (x,_,_) -> x in
  let sort = List.sort (fun x y -> Pervasives.compare (num x) (num y)) in
  let initial = List.map (fun (s,f) -> Leaf (f,s)) fs in 
  let rec make_tree = function
    |[] -> failwith "invalid"
    |[x] -> x
    |h1::h2::t-> let l = Node (num h1 + num h2, h1, h2)::t in make_tree (sort l) in
  let tree = make_tree (sort initial) in
  
  let rec encode acc = function
  |[] -> acc
  |(Leaf (_,s), c)::t -> encode ((s,c)::acc) t
  |(Node (_, t1, t2), c)::t -> encode acc ((t1, c^"0")::(t2, c^"1")::t)
  in List.rev (encode [] [(tree,"")]);;       