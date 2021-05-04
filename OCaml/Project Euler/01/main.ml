(*
                        Problem 1: Multiples of 3 and 5
                        ===============================


If we list all the natural numbers below 10 that are multiples of 3 or 5, we get
3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
*)

let sum_divisible_by (n: int) (cap: int): int =
    let p = (pred cap) / n in
    n*p*(succ p) / 2;;

print_int ((sum_divisible_by 3 1000)
    + (sum_divisible_by 5 1000)
    - (sum_divisible_by 15 1000));;
(* 233168 *)
