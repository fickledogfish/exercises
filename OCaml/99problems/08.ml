(* Eliminate consecutive duplicates of list elements. (medium)
 *
 *     # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 *     - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
 *
 * Compile this with
 *
 *     $ ocamlbuild 08.native -pkg oUnit
 * 
 * to run the tests.
 *)

(* My original version *)

(* let compress lst =
 *     let append el = function
 *         | [] -> [el]
 *         | h::t -> if el = h then h::t else el::h::t;
 *     in let rec cmp acc = function
 *         | [] -> acc
 *         | h::t -> cmp (append h acc) t;
 *     in
 *     List.rev (cmp [] lst);;
 *)

let rec compress = function
    | a::(b::_ as t) -> if a = b then compress t else a::(compress t)
    | l -> l;;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal
                (compress [ "a";"a";"a";"a";"b";"c";"c"
                          ; "a";"a";"d";"e";"e";"e";"e"
                          ])
                ["a"; "b"; "c"; "a"; "d"; "e"];
        in
        "suite" >:::
            [ "test1" >:: test1 ];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;
