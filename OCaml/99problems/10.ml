(* If you need so, refresh your memory about run-length encoding.
 *
 * Here is an example:
 *
 *     # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 *     - : (int * string) list =
 *     [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
 *
 * Compile this with
 *
 *     $ ocamlbuild 02.native -pkg oUnit
 * 
 * to run the tests.
 *)

let encode lst =
    let rec enc acc cnt = function
        | [] -> acc
        | a::[] -> (cnt, a)::acc
        | a::(b::_ as t) ->
                if a = b then
                    enc acc (succ cnt) t
                else
                    enc ((cnt, a)::acc) 1 t
    in
    List.rev (enc [] 1 lst);;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal
                (encode [ "a";"a";"a";"a";"b";"c";"c"
                        ; "a";"a";"d";"e";"e";"e";"e"
                        ])
                [ (4, "a"); (1, "b"); (2, "c")
                ; (2, "a"); (1, "d"); (4, "e")
                ];
        in
        "suite" >:::
            [ "test1" >:: test1 ];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;
