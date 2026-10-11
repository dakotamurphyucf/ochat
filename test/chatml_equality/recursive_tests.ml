open Core
module T = Chatml_typechecker

let record fields = T.Record (T.Row (T.Env.of_list fields, T.Empty_row))

let accepts ty =
  try
    T.ensure_equality_type (T.create_state ()) ty;
    true
  with
  | T.Type_error _ -> false
;;

let%test_unit "equality traversal preserves cancellation" =
  let exception Cancelled in
  let polls = ref 0 in
  let state =
    T.create_state
      ~checkpoint:(fun () ->
        Int.incr polls;
        if !polls = 2 then raise Cancelled)
      ()
  in
  let ty = record (List.init 100 ~f:(fun index -> Int.to_string index, T.TInt)) in
  match T.ensure_equality_type state ty with
  | () -> failwith "equality traversal ignored cancellation"
  | exception Cancelled -> [%test_eq: int] 2 !polls
;;

let%test_unit "recursive equality is invariant under binder renaming and representation" =
  List.iter
    [ T.TInt, true; T.Array T.TInt, false; T.Fun ([ T.TInt ], T.TInt), false ]
    ~f:(fun (payload, expected) ->
      List.iter [ "outer"; "inner" ] ~f:(fun nested_binder ->
        let explicit =
          T.Mu
            ( "outer"
            , record
                [ "next", T.Rec_var "outer"
                ; ( "nested"
                  , T.Mu
                      ( nested_binder
                      , record [ "next", T.Rec_var nested_binder; "payload", payload ] ) )
                ] )
        in
        [%test_eq: bool] expected (accepts explicit));
      let cell = ref (T.Bound T.Unit) in
      cell := T.Bound (record [ "next", T.Var cell; "payload", payload ]);
      [%test_eq: bool] expected (accepts (T.Var cell)))
;;
