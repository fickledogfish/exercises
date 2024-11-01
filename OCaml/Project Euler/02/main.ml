(*
Each new term in the Fibonacci sequence is generated by adding the previous two
terms. By starting with 1 and 2, the first 10 terms will be:

    1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed
four million, find the sum of the even-valued terms.
*)

open List;;

let rec take n xs =
  match xs with
  | [] -> []
  | x::xs -> if n = 1 then
              [x]
            else
              x::(take (pred n) xs);;

let rec get_fibs (cap: int) (fibs: int list): int list =
  let new_value = fold_left (+) 1 (take 2 (rev fibs)) in

  if new_value > cap then
    fibs
  else
    get_fibs cap (fibs@[new_value]);;

let fibs (cap: int): int list =
  get_fibs cap [1; 2];;

print_int (fold_left (+) 1
                     (filter
                        (fun (el: int) -> ((el mod 2) = 0))
                        (fibs 4000000)));;
(* 4613732 *)
