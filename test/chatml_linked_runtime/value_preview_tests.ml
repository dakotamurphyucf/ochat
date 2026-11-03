open! Core
module Preview = Chatml_repl_execution.Value_preview
module L = Chatml.Chatml_lang

let limits ?(max_bytes = 4_096) ?(max_nodes = 256) ?(max_depth = 16) ?(columns = 76) () =
  Preview.Limits.create ~max_bytes ~max_nodes ~max_depth ~columns |> Or_error.ok_exn
;;

let%expect_test "REPL values quote strings and retain compact scalar structures" =
  let values =
    [ L.VString "a\n\"é\"\027\255"
    ; L.VString "\226\130"
    ; L.VArray [| L.VInt 1; L.VString "two" |]
    ; L.VTuple (Chatml.Chatml_tuple.create (L.VInt 1) (L.VBool true) ~rest:[])
    ; L.VVariant ("Some", [ L.VString "saved" ])
    ]
  in
  List.iter values ~f:(fun value ->
    print_endline (Preview.to_string ~limits:(limits ()) value));
  [%expect
    {|
    "a\n\"é\"\u{001b}\xff"
    "\xe2\x82"
    [1, "two"]
    (|1, true|)
    `Some("saved")
    |}]
;;

let%expect_test "larger nested structures use a readable bounded layout" =
  let value =
    L.VRecord
      (String.Map.of_alist_exn
         [ "count", L.VInt 2
         ; "entries", L.VArray [| L.VString "first value"; L.VString "second value" |]
         ])
  in
  print_endline (Preview.to_string ~limits:(limits ~columns:32 ()) value);
  [%expect
    {|
    { count = 2;
      entries =
        ["first value",
          "second value"] }
    |}]
;;

let%expect_test
    "preview budgets bound cycles, malformed strings and uncalled builtin values"
  =
  let cyclic = ref L.VUnit in
  cyclic := L.VRef cyclic;
  let calls = ref 0 in
  let builtin =
    L.VBuiltin
      (fun _ ->
        incr calls;
        L.VUnit)
  in
  let values =
    [ L.VRef cyclic
    ; L.VString (String.make 10_000 '\255')
    ; L.VString "\226\130"
    ; L.VArray (Array.init 1_000 ~f:(fun index -> L.VInt index))
    ; builtin
    ]
  in
  List.iter [ 1; 2; 3; 8; 32; 128 ] ~f:(fun max_bytes ->
    List.iter values ~f:(fun value ->
      let text =
        Preview.to_string ~limits:(limits ~max_bytes ~max_nodes:8 ~max_depth:4 ()) value
      in
      assert (String.length text <= max_bytes);
      assert (Stdlib.String.is_valid_utf_8 text);
      assert (not (String.contains text '\027'))));
  print_s [%sexp (calls : int ref)];
  print_endline (Preview.to_string ~limits:(limits ~max_depth:3 ()) (L.VRef cyclic));
  [%expect
    {|
    0
    ref(ref(ref(...))) ... [truncated]
    |}]
;;

let%expect_test "literal ellipses cannot hide a structural truncation marker" =
  let value = L.VArray [| L.VString "..."; L.VInt 1; L.VInt 2 |] in
  let text = Preview.to_string ~limits:(limits ~max_nodes:2 ()) value in
  print_s [%sexp (String.is_suffix text ~suffix:"... [truncated]" : bool)];
  [%expect {| true |}]
;;
