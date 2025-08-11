open Base

let a = [ 1; 3; 4 ]
let b = [ 3; 4; 5 ]

(* A basic model of sets using OCaml lists *)

(* set membership *)
let ( < ) el ls = List.exists ~f:(fun x -> x = el) ls
let subset a b = List.for_all a ~f:(fun el -> el < b)
let rec equal_sets a b = subset a b && subset b a

(* Not a very good approach *)
let set_union a b =
  let rec go a b acc =
    match (a, b) with
    | [], [] -> acc
    | hd :: tl, [] | [], hd :: tl -> go tl [] (hd :: acc)
    | hd1 :: tl1, hd2 :: tl2 -> go tl1 tl2 (hd1 :: hd2 :: acc)
  in
  go a b []

let set_intersection a b =
  let rec go li acc =
    match li with
    | [] -> acc
    | hd :: tl -> if hd < b then go tl (hd :: acc) else go tl acc
  in
  go a []

let set_diff a b =
  let rec go a b acc =
    match a with
    | [] -> acc
    | hd :: tl -> go tl b (if not (hd < b) then hd :: acc else acc)
  in
  go a b []

(* Computed Fixed Point: Take a predicate,
function and point, and keep applying f(point)
 until same result *)

(* In progress *)
(* let computed_fixed_point eq f x =
  let rec go eq f acc = 4 in
  match acc with
  | val1, val2 -> val1
 |  in go eq f (3, 3) *)
