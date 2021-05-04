(* Pack consecutive duplicates of list elements into sublists. (medium)
 * 
 *     # pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
 *     - : string list list =
 *     [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 *      ["e"; "e"; "e"; "e"]]
 *
 * Compile this with
 *
 *     $ ocamlbuild 09.native -pkg oUnit
 * 
 * to run the tests.
 *)

let pack lst =
    let rec pk curr acc = function
        | [] -> []
        | h::[] -> (h::curr)::acc
        | a::(b::_ as t) ->
                if a = b then
                    pk (a::curr) acc t
                else
                    pk [] ((a::curr)::acc) t;
    in
    List.rev (pk [] [] lst);;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal
                (pack [ "a";"a";"a";"a";"b";"c";"c"
                      ; "a";"a";"d";"d";"e";"e";"e";"e"
                      ])
                [ ["a"; "a"; "a"; "a"]
                ; ["b"]
                ; ["c"; "c"]
                ; ["a"; "a"]
                ; ["d"; "d"]
                ; ["e"; "e"; "e"; "e"]
                ];
        in
        "suite" >:::
            [ "test1" >:: test1 ];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;
