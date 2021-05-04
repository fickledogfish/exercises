(* Write a function last : 'a list -> 'a option that returns the last element
 * of a list. (easy)
 *
 *     # last [ "a" ; "b" ; "c" ; "d" ];;
 *     - : string option = Some "d"
 *     # last [];;
 *     - : 'a option = None
 *
 * Compile this with
 *
 *     $ ocamlbuild 01.native -pkg oUnit
 * 
 * to run the tests.
 *)

let rec last = function
    | [] -> None
    | x::[] -> Some x
    | _::xs -> last xs;;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal (last [ "a" ; "b" ; "c" ; "d" ]) (Some "d");

        and test2 ctxt =
            assert_equal (last []) None;

        in
        "suite" >:::
            [ "test1" >:: test1
            ; "test2" >:: test2
            ];;
end

let () = 
    OUnit2.run_test_tt_main Tests.suite;;
