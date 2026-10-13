open Core
module Inferred = Chatml_typechecker
module Frozen = Chatml_typechecker.Type_snapshot
module Display = Chatml_type_display

let snapshot_limit () =
  Frozen.Limits.create ~max_nodes:2048 ~max_edges:4096 ~max_label_bytes:4096
  |> Or_error.ok_exn
;;

let display_limit
      ?(nodes = 4096)
      ?(edges = 8192)
      ?(depth = 128)
      ?(work = 100_000)
      ?(text = 4096)
      ?(output = 8192)
      ()
  =
  Display.Limits.create
    ~max_nodes:nodes
    ~max_edges:edges
    ~max_depth:depth
    ~max_work_steps:work
    ~max_text_bytes:text
    ~max_output_bytes:output
  |> Or_error.ok_exn
;;

let freeze groups =
  match
    Frozen.freeze
      (List.map groups ~f:Frozen.Input_group.create)
      ~limits:(snapshot_limit ())
      ~checkpoint:(fun () -> ())
  with
  | Ok snapshot -> snapshot
  | Error error -> failwith (Sexp.to_string (Frozen.Error.sexp_of_t error))
;;

let first_root snapshot =
  match Frozen.roots snapshot with
  | (root :: _) :: _ -> root
  | _ -> failwith "missing frozen root"
;;

let render ?(limits = display_limit ()) snapshot root =
  match
    Display.Session.render
      (Display.Session.create ~limits)
      snapshot
      ~root
      ~kind:Display.Kind.Type
      ~checkpoint:(fun () -> ())
  with
  | Ok output -> output
  | Error error -> failwith (Sexp.to_string (Display.Error.sexp_of_t error))
;;

let render_with_session session snapshot root =
  match
    Display.Session.render
      session
      snapshot
      ~root
      ~kind:Display.Kind.Type
      ~checkpoint:(fun () -> ())
  with
  | Ok output -> output
  | Error error -> failwith (Sexp.to_string (Display.Error.sexp_of_t error))
;;

let%expect_test "raw rows, argument arity and distinct variable identities" =
  let open Inferred in
  let shared = ref (Free { name = "a"; level = 0; equality = None }) in
  let different = ref (Free { name = "a"; level = 0; equality = None }) in
  let raw_row =
    Row (Env.singleton "value" TInt, Row (Env.singleton "value" Boolean, Empty_row))
  in
  let root = Fun ([ Var shared; Var shared; Var different ], Record raw_row) in
  let snapshot = freeze [ [ root ] ] in
  let output = render snapshot (first_root snapshot) in
  print_endline (Display.text output);
  print_s [%sexp (Display.completeness output : Display.Completeness.t)];
  [%expect
    {|
    fun(?a#1, ?a#1, ?a#2) -> record_raw(row_layer{value: int} over row_layer{value: bool} over {})
    Complete
    |}]
;;

let%expect_test "recursive binder and shared cyclic link remain finite" =
  let open Inferred in
  let cell = ref (Free { name = "x"; level = 0; equality = None }) in
  cell := Bound (Var cell);
  let snapshot = freeze [ [ Tuple [ Mu ("r", Rec_var "r"); Var cell; Var cell ] ] ] in
  let output = render snapshot (first_root snapshot) in
  print_endline (Display.text output);
  print_s [%sexp (Display.completeness output : Display.Completeness.t)];
  [%expect
    {|
    ((mu r#0. rec:r#0) * #3=(#3) * #3)
    Complete
    |}]
;;

let%expect_test "ordinary record, variant and nested function type help" =
  let open Inferred in
  let record = Record (Row (Env.of_list [ "x", TInt; "label", String ], Empty_row)) in
  let variant =
    Variant
      (Row
         ( Env.of_list
             [ "Ready", Variant_payload []; "Next", Variant_payload [ TInt; String ] ]
         , Empty_row ))
  in
  let input = Fun ([ record; Fun ([ TInt ], Boolean) ], variant) in
  let snapshot = freeze [ [ input ] ] in
  let output = render snapshot (first_root snapshot) in
  print_endline (Display.text output);
  let layered =
    Module
      (Row (Env.singleton "outer" Boolean, Row (Env.singleton "inner" String, Empty_row)))
  in
  let layered_snapshot = freeze [ [ layered ] ] in
  print_endline (Display.text (render layered_snapshot (first_root layered_snapshot)));
  [%expect
    {|
    fun({label: string; x: int}, (fun(int) -> bool)) -> [`Next(int, string) | `Ready]
    module{outer: bool; inner: string}
    |}]
;;

let valid_safe_utf8 text =
  Uutf.String.fold_utf_8
    (fun okay _ -> function
       | `Malformed _ -> false
       | `Uchar uchar ->
         let codepoint = Stdlib.Uchar.to_int uchar in
         okay
         && codepoint >= 0x20
         && codepoint <> 0x7F
         && not (codepoint >= 0x80 && codepoint <= 0x9F))
    true
    text
;;

let%test_unit "invalid and control label bytes are escaped; source nodes stay immutable" =
  let open Inferred in
  let raw_name = "x;:\000\127\194\128\255é\\\"" in
  let snapshot = freeze [ [ Con (raw_name, [ TInt ]) ] ] in
  let first = render snapshot (first_root snapshot) in
  let second = render snapshot (first_root snapshot) in
  [%test_result: bool] (valid_safe_utf8 (Display.text first)) ~expect:true;
  [%test_result: string] (Display.text first) ~expect:(Display.text second);
  [%test_result: bool]
    (String.is_substring
       (Display.text first)
       ~substring:"x\\x3B\\x3A\\x00\\x7F\\xC2\\x80\\xFF\\xC3\\xA9\\x5C\\x22")
    ~expect:true
;;

let%test_unit "separate quantification groups retain scheme-scoped display" =
  let open Inferred in
  let equality =
    Chatml.Chatml_equality_requirement.Row (String.Set.of_list [ "old"; "new" ])
  in
  let generic = Generic { name = "a"; equality = Some equality } in
  let snapshot = freeze [ [ generic ]; [ generic ] ] in
  let roots = Frozen.roots snapshot |> List.concat in
  let outputs = List.map roots ~f:(fun root -> Display.text (render snapshot root)) in
  match outputs with
  | [ left; right ] ->
    [%test_result: string] left ~expect:"'a {row_eq_except(new, old)}";
    [%test_result: string] right ~expect:left;
    List.iter outputs ~f:(fun text ->
      [%test_result: bool]
        (String.is_substring text ~substring:"{row_eq_except(new, old)}")
        ~expect:true)
  | _ -> failwith "expected two grouped roots"
;;

let%test_unit "all small budgets remain bounded and typed" =
  let open Inferred in
  let snapshot = freeze [ [ Fun ([ TInt; Boolean ], Array String) ] ] in
  let root = first_root snapshot in
  for cap = 1 to 30 do
    let output = render ~limits:(display_limit ~text:cap ~output:cap ()) snapshot root in
    [%test_result: bool] (String.length (Display.text output) <= cap) ~expect:true;
    [%test_result: bool] (valid_safe_utf8 (Display.text output)) ~expect:true;
    match Display.completeness output with
    | Complete -> ()
    | Truncated _ ->
      [%test_result: bool]
        (String.is_empty (Display.text output)
         || String.is_suffix (Display.text output) ~suffix:"...")
        ~expect:true
  done;
  let long_label = String.make 2000 'x' in
  let row = Record (Row (Env.singleton long_label TInt, Empty_row)) in
  let row_snapshot = freeze [ [ row ] ] in
  let bounded =
    render ~limits:(display_limit ~work:100 ()) row_snapshot (first_root row_snapshot)
  in
  [%test_result: bool]
    (Display.Completeness.equal (Display.completeness bounded) (Truncated Work_limit))
    ~expect:true
;;

let%test_unit "each independent traversal ceiling reports its own reason" =
  let snapshot = freeze [ [ Inferred.Array (Array TInt) ] ] in
  let root = first_root snapshot in
  let cases =
    [ display_limit ~nodes:1 (), Display.Completeness.Node_limit
    ; display_limit ~edges:1 (), Edge_limit
    ; display_limit ~depth:1 (), Depth_limit
    ; display_limit ~work:1 (), Work_limit
    ; display_limit ~text:1 (), Byte_limit
    ]
  in
  List.iter cases ~f:(fun (limits, reason) ->
    let output = render ~limits snapshot root in
    [%test_result: bool]
      (Display.Completeness.equal (Display.completeness output) (Truncated reason))
      ~expect:true)
;;

let%test_unit "generated finite trees obey complete and truncated display invariants" =
  let open Inferred in
  let random = Random.State.make [| 306; 13 |] in
  let pick bound = Random.State.int random bound in
  let rec reference ?(nested = false) = function
    | TInt -> "int"
    | TFloat -> "float"
    | Boolean -> "bool"
    | String -> "string"
    | Unit -> "unit"
    | Fun (arguments, result) ->
      let value =
        "fun("
        ^ String.concat ~sep:", " (List.map arguments ~f:(reference ~nested:true))
        ^ ") -> "
        ^ reference ~nested:true result
      in
      if nested then "(" ^ value ^ ")" else value
    | Tuple elements ->
      "(" ^ String.concat ~sep:" * " (List.map elements ~f:(reference ~nested:true)) ^ ")"
    | Array element -> reference ~nested:true element ^ " array"
    | Ref element -> "ref(" ^ reference ~nested:true element ^ ")"
    | Con (name, arguments) ->
      name
      ^ "<"
      ^ String.concat ~sep:", " (List.map arguments ~f:(reference ~nested:true))
      ^ ">"
    | _ -> failwith "generator produced a type outside the reference model"
  in
  let rec tree depth =
    if depth = 0
    then (
      match pick 5 with
      | 0 -> TInt
      | 1 -> TFloat
      | 2 -> Boolean
      | 3 -> String
      | _ -> Unit)
    else (
      match pick 5 with
      | 0 -> Fun ([ tree (depth - 1); tree (depth - 1) ], tree (depth - 1))
      | 1 -> Tuple [ tree (depth - 1); tree (depth - 1) ]
      | 2 -> Array (tree (depth - 1))
      | 3 -> Ref (tree (depth - 1))
      | _ -> Con ("g" ^ Int.to_string (pick 10), [ tree (depth - 1) ]))
  in
  for _ = 1 to 80 do
    let input = tree (pick 5) in
    let snapshot = freeze [ [ input ] ] in
    let root = first_root snapshot in
    let complete = render snapshot root in
    [%test_result: bool]
      (Display.Completeness.equal (Display.completeness complete) Complete)
      ~expect:true;
    [%test_result: bool] (valid_safe_utf8 (Display.text complete)) ~expect:true;
    [%test_result: string] (Display.text complete) ~expect:(reference input);
    let cap = 1 + pick 64 in
    let limited = render ~limits:(display_limit ~text:cap ~output:cap ()) snapshot root in
    [%test_result: bool] (String.length (Display.text limited) <= cap) ~expect:true;
    [%test_result: bool] (valid_safe_utf8 (Display.text limited)) ~expect:true;
    match Display.completeness limited with
    | Complete ->
      [%test_result: string] (Display.text limited) ~expect:(Display.text complete)
    | Truncated _ ->
      [%test_result: bool]
        (String.is_empty (Display.text limited)
         || String.is_suffix (Display.text limited) ~suffix:"...")
        ~expect:true
  done
;;

let%test_unit "session output bound spans roots and distinct snapshot owners" =
  let open Inferred in
  let repeated name =
    let cell = ref (Free { name; level = 0; equality = None }) in
    freeze [ [ Tuple [ Var cell; Var cell ] ] ]
  in
  let left = repeated "left" in
  let right = repeated "right" in
  let limits = display_limit ~output:100 () in
  let session = Display.Session.create ~limits in
  let render_in snapshot =
    match
      Display.Session.render
        session
        snapshot
        ~root:(first_root snapshot)
        ~kind:Display.Kind.Type
        ~checkpoint:(fun () -> ())
    with
    | Ok output -> output
    | Error error -> failwith (Sexp.to_string (Display.Error.sexp_of_t error))
  in
  let first = render_in left in
  let second = render_in right in
  [%test_result: bool]
    (String.length (Display.text first) + String.length (Display.text second) <= 100)
    ~expect:true;
  [%test_result: bool]
    (String.is_substring (Display.text second) ~substring:"right")
    ~expect:true;
  let narrow =
    Display.Session.create
      ~limits:(display_limit ~output:(String.length (Display.text first) + 1) ())
  in
  let first_narrow = render_with_session narrow left (first_root left) in
  let second_narrow = render_with_session narrow right (first_root right) in
  [%test_result: bool]
    (Display.Completeness.equal (Display.completeness first_narrow) Complete)
    ~expect:true;
  [%test_result: bool]
    (Display.Completeness.equal
       (Display.completeness second_narrow)
       (Truncated Byte_limit))
    ~expect:true;
  [%test_result: string] (Display.text second_narrow) ~expect:"";
  [%test_result: bool]
    (Result.is_error
       (Display.Session.render
          session
          left
          ~root:(first_root right)
          ~kind:Display.Kind.Type
          ~checkpoint:(fun () -> ())))
    ~expect:true
;;

let%test_unit "checkpoint interruption keeps exception identity" =
  let exception Interrupted in
  let snapshot = freeze [ [ Inferred.Fun ([ TInt ], TInt) ] ] in
  let session = Display.Session.create ~limits:(display_limit ()) in
  let result =
    try
      ignore
        (Display.Session.render
           session
           snapshot
           ~root:(first_root snapshot)
           ~kind:Display.Kind.Type
           ~checkpoint:(fun () -> raise Interrupted));
      false
    with
    | Interrupted -> true
    | _ -> false
  in
  [%test_result: bool] result ~expect:true
;;
