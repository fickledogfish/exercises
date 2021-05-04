(* Find the k'th element of a list. (easy)
 *
 *     # at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
 *     - : string option = Some "c"
 *     # at 3 [ "a" ];;
 *     - : string option = None
 *
 * REMARK: OCaml has List.nth which numbers elements from 0 and raises an
 * exception if the index is out of bounds.
 *
 *     # List.nth [ "a" ; "b"; "c"; "d"; "e" ] 2;;
 *     - : string = "c"
 *     # List.nth [ "a" ] 2;;
 *     Exception: Failure "nth".
 *
 * Compile this with
 *
 *     $ ocamlbuild 03.native -pkg oUnit
 * 
 * to run the tests.
 *)

(* My version of the solution: *)

(* let rec at k lst =
 *     match k, lst with
 *     | _, []    -> None
 *     | 1, x::_  -> Some x
 *     | k, _::tl -> at (pred k) tl;;
 *)

let rec at k = function
    | [] -> None
    | h :: t -> if k = 1 then Some h else at (k-1) t;;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal (at 3 ["a"; "b"; "c"; "d"; "e"]) (Some "c");

        and test2 ctxt =
            assert_equal (at 3 ["a"]) None;

        in
        "suite" >:::
            [ "test1" >:: test1
            ; "test2" >:: test2
            ];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;
