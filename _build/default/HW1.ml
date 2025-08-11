open Base

let a = [ 1; 3; 4 ]
let b = [ 3; 4; 5 ]

(* A basic model of sets using OCaml lists *)
let ( < ) el ls = List.exists ~f:(fun x -> x = el) ls
let subset a b = List.for_all a ~f:(fun el -> el < b)
let rec equal_sets a b = subset a b && subset b a

let rec set_union a b =
match a b with
|

let set_intersection a b =
  let rec go li acc =
    match li with
    | [] -> acc
    | hd :: tl -> if hd < b then go tl (hd :: acc) else go tl acc
  in
  go a []
