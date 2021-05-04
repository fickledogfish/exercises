(* Modified run-length encoding. (easy)
 *
 * Modify the result of the previous problem in such a way that if an element
 * has no duplicates it is simply copied into the result list. Only elements
 * with duplicates are transferred as (N E) lists.
 *
 * Since OCaml lists are homogeneous, one needs to define a type to hold both
 * single elements and sub-lists.
 *
 *     # type 'a rle =
 *         | One of 'a
 *         | Many of int * 'a;;
 *     type 'a rle = One of 'a | Many of int * 'a
 *
 *     # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 *     - : string rle list =
 *     [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 *      Many (4, "e")]
 *)

type 'a run_len_enc =
    | One of 'a
    | Many of int * 'a;;

let encode lst =
    [];;

module Tests = struct
    open OUnit2;;

    let suite =
        let test1 ctxt =
            assert_equal
                (encode [ "a";"a";"a";"a";"b";"c";"c"
                        ; "a";"a";"d";"e";"e";"e";"e"
                        ])
                [ Many (4, "a"); One "b"; Many (2, "c")
                ; Many (2, "a"); One "d"; Many (4, "e")
                ];
        in
        "suite" >:::
            [ "test1" >:: test1 ];;
end

let () =
    OUnit2.run_test_tt_main Tests.suite;;
