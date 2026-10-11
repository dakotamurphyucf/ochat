open Core
module L = Chatml.Chatml_lang
module Linked = Chatml_linked_program
module V = Chatml.Chatml_module_visibility
module Ref = Chatml.Chatml_module_reference
module P = Chatml_prepared_program
module S = Chatml_source
module C = Chatml.Chatml_compiler_contract

let result contract input =
  Linked.compile input ~contract ~max_dependency_depth:16 ~checkpoint:(fun () -> ())
;;

let failure contract input =
  match result contract input with
  | Ok _ -> failwith "Invalid program admitted"
  | Error failure -> Linked.Failure.diagnostic failure
;;

let%test_unit "companions select actual diamond exports using their own alias scopes" =
  let contract =
    Support.native_contract ~requirements:[ "answer", Chatml.Chatml_builtin_spec.TInt ] ()
  in
  let input =
    Support.input
      contract
      ~interfaces:
        [ "common.chatml", "type token = int val number : token", []
        ; ( "left.chatml"
          , "type token = Types.token val left : token"
          , [ "Types", "common.chatml" ] )
        ]
      [ ( "entry.chatml"
        , "let answer = Left.left + Right.right"
        , [ "Left", "left.chatml"; "Right", "right.chatml" ] )
      ; ( "left.chatml"
        , "type token = Base.token let left = Base.number"
        , [ "Base", "common.chatml" ] )
      ; "right.chatml", "let right = Common.number", [ "Common", "common.chatml" ]
      ; "common.chatml", "type token = int let secret = ref(0) let number = 7", []
      ]
  in
  let linked = Support.compiled contract input in
  [%test_eq: string list]
    (Support.paths (Chatml_dependency_graph.nodes (Linked.graph linked)))
    [ "common.chatml"; "left.chatml"; "right.chatml"; "entry.chatml" ];
  [%test_eq: string list]
    (Support.paths (Linked.runtime_order linked))
    [ "common.chatml"; "left.chatml"; "right.chatml"; "entry.chatml" ];
  let common =
    Linked.find_unit
      linked
      (P.Module_id.of_implementation
         (let source =
            List.find_exn (P.sources input) ~f:(fun s ->
              String.equal (S.Id.relative_path (S.id s)) "common.chatml")
          in
          S.id source))
    |> Option.value_exn
  in
  assert (
    Sexp.equal
      (V.sexp_of_t (Linked.Unit.visibility common))
      (V.sexp_of_t [ V.Value "number" ]))
;;

let%test_unit "implementation and companion failures retain their own captured owners" =
  let contract = Support.native_contract () in
  let make body interface =
    Support.input
      contract
      ~interfaces:[ "catalog.chatml", interface, [] ]
      [ "entry.chatml", "let answer = Catalog.number", [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", body, []
      ]
  in
  [%test_eq: string]
    (Support.owner (failure contract (make "let number = true" "val number : int")))
    "catalog.chatmli";
  [%test_eq: string]
    (Support.owner (failure contract (make "let number = 1 + true" "val number : int")))
    "catalog.chatml";
  let hidden =
    Support.input
      contract
      ~interfaces:[ "catalog.chatml", "val number : int", [] ]
      [ "entry.chatml", "let answer = Catalog.secret", [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "let number = 7 let secret = 9", []
      ]
  in
  [%test_eq: string] (Support.owner (failure contract hidden)) "entry.chatml"
;;

let%test_unit "runtime closure excludes value uses in compiler-only checked subtrees" =
  let contract = Support.native_contract () in
  let input =
    Support.input
      contract
      [ ( "entry.chatml"
        , "type token = Types.token open Available let answer = 1"
        , [ "Types", "types.chatml"; "Available", "available.chatml" ] )
      ; ( "types.chatml"
        , "type token = int let ignored = Effect.number"
        , [ "Effect", "effect.chatml" ] )
      ; "effect.chatml", "let number = 7", []
      ; "available.chatml", "let extra = 8", []
      ]
  in
  let linked = Support.compiled contract input in
  [%test_eq: string list] (Support.paths (Linked.runtime_order linked)) [ "entry.chatml" ];
  [%test_eq: int] (List.length (Linked.units linked)) 4;
  assert (
    not
      (List.exists (Linked.Unit.program (Linked.entry linked)).stmts ~f:(fun node ->
         match node.value with
         | L.RSOpen _ -> true
         | _ -> false)))
;;

let%test_unit
    "selected host bindings use final private scope and canonical imported targets"
  =
  let contract =
    Support.native_contract
      ~requirements:[ "main", Chatml.Chatml_builtin_spec.TFun ([ TInt ], TInt) ]
      ()
  in
  let input =
    Support.input
      contract
      ~interfaces:[ "entry.chatml", "val answer : int", [] ]
      [ "entry.chatml", "open Catalog let answer = 1", [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "let main value = value", []
      ]
  in
  let linked = Support.compiled contract input in
  [%test_eq: string list]
    (Support.paths (Linked.runtime_order linked))
    [ "catalog.chatml"; "entry.chatml" ];
  (match Linked.entrypoints linked with
   | [ descriptor ] ->
     [%test_eq: string] (Linked.Entrypoint.name descriptor) "main";
     (match Linked.Entrypoint.target descriptor with
      | Ref.Binding_target.Configured_member reference ->
        [%test_eq: string]
          (S.Id.relative_path (Ref.implementation reference))
          "catalog.chatml";
        [%test_eq: string list] (Ref.member_path reference) [ "main" ]
      | Local_binding _ -> failwith "Imported entrypoint lost canonical origin")
   | _ -> failwith "Selected host descriptors changed");
  assert (
    Sexp.equal
      (V.sexp_of_t (Linked.Unit.visibility (Linked.entry linked)))
      (V.sexp_of_t [ V.Value "answer" ]))
;;

let%test_unit "tuple shadowing preserves prior closures and clears new local read origin" =
  let contract = Support.native_contract () in
  let input =
    Support.input
      contract
      [ ( "entry.chatml"
        , "open Catalog let before () = value let (|value, _|) = (|8, ()|) let after () \
           = value"
        , [ "Catalog", "catalog.chatml" ] )
      ; "catalog.chatml", "let value = 7", []
      ]
  in
  let linked = Support.compiled contract input in
  let read name =
    let stmt =
      List.find_exn (Linked.Unit.program (Linked.entry linked)).stmts ~f:(fun stmt ->
        match stmt.value with
        | L.RSLet ({ value = Name n; _ }, _) -> String.equal n name
        | _ -> false)
    in
    match stmt.value with
    | L.RSLet (_, { value = RELambda (_, _, body); _ }) -> body.value
    | _ -> failwith "Expected resolved closure"
  in
  (match read "before" with
   | L.REConfigured_read reference ->
     [%test_eq: string list] (Ref.member_path reference) [ "value" ]
   | _ -> failwith "Earlier closure retargeted");
  match read "after" with
  | L.REVarGlobal "value" -> ()
  | _ -> failwith "Local tuple binder retained configured origin"
;;

let%test_unit
    "new resolved reads fail with owned canonical unavailable registry diagnostics"
  =
  let contract = Support.native_contract () in
  let input =
    Support.input
      contract
      [ "entry.chatml", "let number = Catalog.number", [ "Catalog", "catalog.chatml" ]
      ; "catalog.chatml", "let number = 7", []
      ]
  in
  let linked = Support.compiled contract input in
  let unit = Linked.entry linked in
  let env = Chatml.Chatml_lang.create_env ~source:(Linked.Unit.implementation unit) () in
  match Chatml.Chatml_eval.eval_program env (Linked.Unit.program unit) with
  | () -> failwith "Configured read silently executed without a registry"
  | exception L.Runtime_error diagnostic ->
    [%test_eq: string] diagnostic.message "Configured module registry is unavailable";
    [%test_eq: string] (Support.owner diagnostic) "entry.chatml"
;;
