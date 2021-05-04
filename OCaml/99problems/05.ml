(* Reverse a list. (easy)
 *
 * OCaml standard library has List.rev but we ask that you reimplement it.
 *
 *     # rev ["a" ; "b" ; "c"];;
 *     - : string list = ["c"; "b"; "a"]
 *
 * Compile this with
 *
 *     $ ocamlbuild 05.native -pkg oUnit
 *
 * to run the tests.
 *)

let rev lst =
    let rec reverse new_lst lst =
        match lst with
        | [] -> new_lst
        | h::t -> reverse (h::new_lst) t
    in
    reverse [] lst;;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal (rev ["a"; "b"; "c"]) ["c"; "b"; "a"];

        in
        "suite" >:::
            ["test1" >:: test1];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;
