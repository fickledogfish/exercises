(* Find the last but one (last and penultimate) elements of a list. (easy)
 *
 *     # last_two [ "a" ; "b" ; "c" ; "d" ];;
 *     - : (string * string) option = Some ("c", "d")
 *     # last_two [ "a" ];;
 *     - : (string * string) option = None
 *
 * Compile this with
 *
 *     $ ocamlbuild 02.native -pkg oUnit
 * 
 * to run the tests.
 *)

let rec last_two = function
    | []
    | _::[] -> None
    | y::z::[] -> Some (y, z)
    | _::t -> last_two t;;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal (last_two ["a"; "b"; "c"; "d"]) (Some ("c", "d"));

        and test2 ctxt =
            assert_equal (last_two ["a"]) None;

        in
        "suite" >:::
            [ "test1" >:: test1
            ; "test2" >:: test2
            ];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;
