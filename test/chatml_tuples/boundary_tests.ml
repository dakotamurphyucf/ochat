open! Core
module L = Chatml.Chatml_lang
open L
module Product = Chatml.Chatml_tuple
module X = Chatml_execution
module Shell = Shell_runtime.Chatml_codec

let pair first second = L.VTuple (Product.create first second ~rest:[])

let%test_unit "existing shell bounds descend through tuple children and item counts" =
  let limits : Shell.limits =
    { max_value_bytes = 8; max_string_bytes = 4; max_array_items = 2; max_depth = 1 }
  in
  assert (Result.is_ok (Shell.validate limits (pair (VString "four") (VString "four"))));
  List.iter
    [ pair (VString "five!") VUnit
    ; pair (VInt 0) (VBool false)
    ; pair (pair VUnit VUnit) VUnit
    ; VTuple (Product.create VUnit VUnit ~rest:[ VUnit ])
    ; pair VUnit (VRef (ref VUnit))
    ]
    ~f:(fun value -> assert (Result.is_error (Shell.validate limits value)));
  assert (Result.is_ok (Shell.validate limits (pair VUnit VUnit)))
;;

let%test_unit "execution value checks bound tuple nodes, nested bytes, cycles and reuse" =
  Eio_main.run (fun env ->
    let wide = L.VTuple (Product.create VUnit VUnit ~rest:[ VUnit ]) in
    let deep = pair (pair VUnit VUnit) VUnit in
    let text = pair (VString (String.make 128 'x')) VUnit in
    let cell = ref L.VUnit in
    let cyclic = pair (VRef cell) VUnit in
    cell := cyclic;
    List.iter
      [ wide, { X.default_limits with max_array_items = 2 }
      ; deep, { X.default_limits with max_depth = 1 }
      ; text, { X.default_limits with max_value_bytes = 64 }
      ; cyclic, { X.default_limits with max_depth = 4 }
      ]
      ~f:(fun (value, limits) ->
        let rejected =
          X.with_control ~env ~policy:(Bounded limits) (fun control ->
            (Option.value_exn control).check_value value)
        in
        (match rejected with
         | Error error -> [%test_eq: string] error.code "chatml.value_limit"
         | Ok _ -> failwith "Oversized tuple admitted");
        assert (
          Result.is_ok
            (X.with_control ~env ~policy:(Bounded limits) (fun control ->
               (Option.value_exn control).check_value (pair VUnit VUnit))))))
;;

let%test_unit "tuple literal item admission happens before child effects" =
  Eio_main.run (fun env ->
    let touched = ref false in
    let outcome =
      X.with_control
        ~env
        ~policy:(Bounded { X.default_limits with max_array_items = 2 })
        (fun control ->
           let control = Option.value_exn control in
           let runtime = L.create_env ~control () in
           L.define_var
             runtime
             "print"
             (VBuiltin
                (fun _ ->
                  touched := true;
                  VUnit));
           let program =
             Support.parse "let result = (|print(\"touch\"), (), ()|)"
             |> Chatml_resolver.resolve_program
           in
           Chatml.Chatml_eval.eval_program runtime program)
    in
    assert (not !touched);
    match outcome with
    | Error error -> [%test_eq: string] error.code "chatml.value_limit"
    | Ok _ -> failwith "Oversized tuple literal evaluated")
;;

let%test_unit "moderator snapshot admission checks tuple arity and nesting before copying"
  =
  let module M = Chat_response.Moderator_invocation in
  let limits =
    { Chatmd_shell_spec.Chatmd_script_spec.default_limits with
      max_array_items = 2
    ; max_depth = 1
    }
  in
  assert (Result.is_ok (M.snapshot_state ~limits (pair (VInt 7) (VString "x"))));
  List.iter
    [ VTuple (Product.create VUnit VUnit ~rest:[ VUnit ]); pair (pair VUnit VUnit) VUnit ]
    ~f:(fun value -> assert (Result.is_error (M.snapshot_state ~limits value)))
;;

let%test_unit "Session preflight bounds nested tuple wire before generic conversion" =
  let rec wire depth =
    if depth = 0
    then `Object [ "kind", `String "unit" ]
    else
      `Object
        [ "kind", `String "tuple"
        ; "values", `Array [ wire (depth - 1); `Object [ "kind", `String "unit" ] ]
        ]
  in
  assert (Result.is_ok (Session.Snapshot.of_jsonaf (wire 4)));
  assert (Result.is_error (Session.Snapshot.of_jsonaf (wire 80)))
;;

let%test_unit "tuple debug previews are bounded even for cyclic mutable children" =
  let cell = ref L.VUnit in
  let cyclic = pair (VRef cell) VUnit in
  cell := cyclic;
  let shown =
    Chatml.Chatml_builtin_spec.value_to_debug_string
      ~max_bytes:48
      ~max_nodes:12
      ~max_depth:4
      cyclic
  in
  assert (String.length shown <= 48);
  assert (String.is_prefix shown ~prefix:"(|");
  assert (String.is_substring shown ~substring:"...")
;;
