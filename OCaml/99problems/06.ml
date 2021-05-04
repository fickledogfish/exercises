(* Find out whether a list is a palindrome. (easy)
 *
 * HINT: a palindrome is its own reverse.
 *
 *     # is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
 *     - : bool = true
 *     # not (is_palindrome [ "a" ; "b" ]);;
 *     - : bool = true
 *
 * Compile this with
 *
 *     $ ocamlbuild 06.native -pkg oUnit
 *
 * to run the tests.
 *)

let is_palindrome lst =
    List.rev lst = lst;;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_bool "" (is_palindrome ["x"; "a"; "m"; "a"; "x"]);

        and test2 ctxt =
            assert_bool "" (not (is_palindrome ["a"; "b"]));

        in
        "suite" >:::
            [ "test1" >:: test1
            ; "test2" >:: test2
            ];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;
