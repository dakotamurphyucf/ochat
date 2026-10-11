open Core
module T = Chatml_typechecker
module Alias = T.Type_alias
module Signature = Chatml.Chatml_module_signature

let annotation source =
  match Chatml.Chatml_parse.parse_interface ("val selected : " ^ source) with
  | Ok [ { Signature.desc = Value { annotation; _ }; _ } ] -> annotation
  | _ -> failwith "expected one value specification"
;;

let scheme state annotation ~equality_parameters =
  match
    Alias.elaborate_scheme
      Alias.empty
      annotation
      ~equality_parameters
      ~fresh_name:(fun () -> T.gensym state)
      ~checkpoint:ignore
  with
  | Ok scheme -> scheme
  | Error error -> failwith error.message
;;

let%test_unit "signature quantification preserves sharing and equality restrictions" =
  List.iter [ "a"; "item" ] ~f:(fun name ->
    let syntax = annotation ("fun('" ^ name ^ ") -> '" ^ name) in
    List.iter [ false; true ] ~f:(fun constrained ->
      let state = T.create_state () in
      let equality_parameters = if constrained then [ name ] else [] in
      let declared = scheme state syntax ~equality_parameters in
      List.iter
        [ T.TInt, true; T.Boolean, true; T.Fun ([ T.TInt ], T.TInt), false ]
        ~f:(fun (argument, comparable) ->
          List.iter [ false; true ] ~f:(fun same_result ->
            let result = if same_result then argument else T.String in
            let accepts =
              try
                T.unify
                  state
                  (T.instantiate state declared)
                  (T.Fun ([ argument ], result));
                true
              with
              | T.Type_error _ -> false
            in
            [%test_eq: bool] accepts (same_result && ((not constrained) || comparable))));
      (* Instantiating a specification must not consume or specialize its
         universally quantified parameter, nor share it with another claim. *)
      let other = scheme state syntax ~equality_parameters in
      T.unify
        state
        (T.instantiate state (T.Tuple [ declared; other ]))
        (T.Tuple [ T.Fun ([ T.TInt ], T.TInt); T.Fun ([ T.Boolean ], T.Boolean) ]);
      match declared, other with
      | Fun ([ first ], result), Fun ([ second ], _) ->
        assert (phys_equal first result);
        assert (not (phys_equal first second))
      | _ -> failwith "lost universal function type"))
;;

let%test_unit
    "signature constraints cannot introduce parameters or ordinary annotation \
     polymorphism"
  =
  let syntax = annotation "fun('a) -> 'a" in
  let state = T.create_state () in
  List.iter
    [ [ "missing" ]; [ "a"; "a" ] ]
    ~f:(fun equality_parameters ->
      match
        Alias.elaborate_scheme
          Alias.empty
          syntax
          ~equality_parameters
          ~fresh_name:(fun () -> T.gensym state)
          ~checkpoint:ignore
      with
      | Error _ -> ()
      | Ok _ -> failwith "invalid constraint admitted");
  (match Alias.elaborate Alias.empty syntax ~checkpoint:ignore with
   | Error _ -> ()
   | Ok _ -> failwith "ordinary annotation became implicitly universal");
  let exception Cancelled in
  match
    Alias.elaborate_scheme
      Alias.empty
      syntax
      ~equality_parameters:[]
      ~fresh_name:(fun () -> T.gensym state)
      ~checkpoint:(fun () -> raise Cancelled)
  with
  | exception Cancelled -> ()
  | _ -> failwith "signature elaboration swallowed cancellation"
;;
