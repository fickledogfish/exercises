(* Flatten a nested list structure. (medium)
 *
 *     # (* There is no nested list type in OCaml, so we need to define one 
 *        * first. A node of a nested list is either an element, or a list of
 *        * nodes.
 *        *)

 *       type 'a node =
 *         | One of 'a
 *         | Many of 'a node list;;
 *     type 'a node = One of 'a | Many of 'a node list
 *
 *     # flatten [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]];;
 *     - : string list = ["a"; "b"; "c"; "d"; "e"]
 *
 * Compile this with
 *
 *     $ ocamlbuild 07.native -pkg oUnit
 *
 * to run the tests.
 *)

type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten lst =
    let rec flat acc = function
        | [] -> acc
        | (One h)::t -> flat (h::acc) t
        | (Many h)::t -> flat (flat acc h) t
    in
    List.rev (flat [] lst);;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal
                (flatten
                    [ One "a"
                    ; Many [ One "b"
                           ; Many [ One "c"
                                  ; One "d"
                                  ]
                           ; One "e"
                           ]
                    ; Many [ One "f"
                           ; One "g"
                           ]
                    ])
                ["a"; "b"; "c"; "d"; "e"; "f"; "g"];
        in
        "suite" >:::
            [ "test1" >:: test1 ];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;

