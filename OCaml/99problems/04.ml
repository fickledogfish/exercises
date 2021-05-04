(* Find the number of elements of a list. (easy)
 *
 * OCaml standard library has List.length but we ask that you reimplement it.
 * Bonus for a tail recursive solution.
 *
 *     # length [ "a" ; "b" ; "c"];;
 *     - : int = 3
 *     # length [];;
 *     - : int = 0
 *
 * Compile this with
 *
 *     $ ocamlbuild 04.native -pkg oUnit
 *
 * to run the tests.
 *)

let length lst =
    let rec len acc = function
        | [] -> acc
        | _::t -> len (succ acc) t
    in
    len 0 lst;;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal (length ["a"; "b"; "c"]) 3;

        and test2 ctxt =
            assert_equal (length []) 0;

        in
        "suite" >:::
            [ "test1" >:: test1
            ; "test2" >:: test2
            ];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;
