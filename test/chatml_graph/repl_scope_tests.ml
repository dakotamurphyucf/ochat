open Core
module F = Fixture
module S = Chatml_source
module P = Chatml_prepared_program
module Lookup = Chatml_configured_lookup
module Repl = Lookup.Repl_scope
module Reference = Lookup.Reference
module Origin = Lookup.Origin
module Parse = Chatml.Chatml_parse
module T = Chatml_typechecker
module Check = Chatml_configured_check
module Session = Chatml_dependency_session

let checkpoint () = ()

let id path =
  S.Id.create ~package_namespace:"graph-test" ~relative_path:path |> Or_error.ok_exn
;;

let capture path contents =
  S.create (id path) ~contents ~expected_digest:None ~max_bytes:None ~checkpoint
  |> Or_error.ok_exn
;;

let fixture () =
  F.input
    [ "entry.chatml", "let host = Catalog.value", [ "Catalog", "catalog.chatml" ]
    ; "catalog.chatml", "let value = 3", []
    ]
;;

let limits ?(max_snippets = 4) ?(max_identity_bytes = 128) () =
  Repl.Limits.create ~max_snippets ~max_identity_bytes |> Or_error.ok_exn
;;

let scope ?(limits = limits ()) input =
  Repl.create
    input
    ~anchor_owner:(P.Module_id.of_implementation (id "entry.chatml"))
    ~anchor_implementation:(id "entry.chatml")
    ~limits
    ~checkpoint
  |> function
  | Ok scope -> scope
  | Error _ -> failwith "authentic REPL scope anchor rejected"
;;

let submit scope path body =
  Repl.for_submission scope (capture path body) ~checkpoint
  |> function
  | Ok context -> context
  | Error error -> raise_s [%sexp (error : Repl.Error.t)]
;;

let resolve context =
  Lookup.resolve
    context
    ~name:"Catalog"
    ~reason:Lookup.Reason.Open
    ~span:(F.span (Lookup.source context))
    ~checkpoint
  |> F.ok
;;

let use context origin =
  Lookup.at_use
    context
    origin
    ~reason:Lookup.Reason.Value_reference
    ~span:(F.span (Lookup.source context))
    ~checkpoint
  |> F.ok
;;

let site_path site = S.Id.relative_path (S.id (Chatml_diagnostic.Site.source site))

let%expect_test "cross-snippet provenance names the actual use and original import" =
  let input = fixture () in
  let repl = scope input in
  let a = submit repl "a.chatml" "open Catalog" in
  let imported = resolve a |> Reference.origin in
  let b = submit repl "b.chatml" "let next = value" in
  let reference = use b imported in
  print_s
    [%sexp
      { import = (S.Id.relative_path (fst (Origin.import_source imported)) : string)
      ; use = (S.Id.relative_path (fst (Reference.use_source reference)) : string)
      ; use_site = (site_path (Reference.site reference) : string)
      ; binding_site = (site_path (Reference.binding_site reference) : string)
      }];
  assert (Result.is_ok (Repl.validate_reference repl ~context:b reference ~checkpoint));
  assert (
    Result.is_error
      (Lookup.resolve
         a
         ~name:"Catalog"
         ~reason:Open
         ~span:(F.span (Lookup.source a))
         ~checkpoint));
  Repl.revoke repl;
  assert (
    Result.is_error
      (Lookup.at_use
         b
         imported
         ~reason:Value_reference
         ~span:(F.span (Lookup.source b))
         ~checkpoint));
  [%expect
    {|
    ((import a.chatml) (use b.chatml) (use_site b.chatml)
     (binding_site project.json))
    |}]
;;

exception Cancelled

let%test_unit "failed and cancelled admission do not stale current lookup" =
  let input = fixture () in
  let repl = scope input in
  let a = submit repl "a.chatml" "open Catalog" in
  let first = resolve a in
  (match Repl.for_submission repl (capture "a.chatml" "open Catalog") ~checkpoint with
   | Error (Repl.Error.Reused_source _) -> ()
   | _ -> failwith "same source ID retry was re-admitted");
  (match Repl.for_submission repl (capture "a.chatml" "changed") ~checkpoint with
   | Error (Repl.Error.Stale_source_revision _) -> ()
   | _ -> failwith "reused source revision accepted");
  (match Repl.for_submission repl (capture "entry.chatml" "changed") ~checkpoint with
   | Error (Repl.Error.Catalog_source_collision _) -> ()
   | _ -> failwith "catalog collision accepted");
  let polls = ref 0 in
  let raised =
    try
      ignore
        (Repl.for_submission
           repl
           (capture "b.chatml" "let next = 1")
           ~checkpoint:(fun () ->
             Int.incr polls;
             if !polls = 2 then raise Cancelled));
      false
    with
    | Cancelled -> true
  in
  assert raised;
  [%test_eq: int] !polls 2;
  assert (Result.is_ok (Repl.validate_reference repl ~context:a first ~checkpoint));
  ignore (submit repl "b.chatml" "let next = 1" : Lookup.t);
  match Repl.validate_reference repl ~context:a first ~checkpoint with
  | Error Repl.Error.Stale_context -> ()
  | _ -> failwith "prior source remained current after successful admission"
;;

let%test_unit "limits and distinct epochs reject references despite equal source bytes" =
  let input = fixture () in
  let first = scope ~limits:(limits ~max_snippets:1 ()) input in
  let a = submit first "a.chatml" "open Catalog" in
  let reference = resolve a in
  (match Repl.for_submission first (capture "b.chatml" "") ~checkpoint with
   | Error Repl.Error.Snippet_limit -> ()
   | _ -> failwith "snippet limit not applied");
  let second = scope input in
  let same = submit second "a.chatml" "open Catalog" in
  (match Repl.validate_reference second ~context:same reference ~checkpoint with
   | Error Repl.Error.Foreign_reference -> ()
   | _ -> failwith "foreign epoch reference accepted");
  (match Repl.validate_reference second ~context:a reference ~checkpoint with
   | Error Repl.Error.Foreign_context -> ()
   | _ -> failwith "foreign epoch context accepted");
  let tiny = scope ~limits:(limits ~max_identity_bytes:1 ()) input in
  match Repl.for_submission tiny (capture "z.chatml" "") ~checkpoint with
  | Error Repl.Error.Identity_byte_limit -> ()
  | _ -> failwith "identity-byte limit not applied"
;;

let%test_unit "reentrant checkpoint cannot overwrite a newer admitted snippet" =
  List.iter [ 1; 2 ] ~f:(fun stop ->
    let repl = scope ~limits:(limits ~max_snippets:1 ()) (fixture ()) in
    let nested = ref None in
    let polls = ref 0 in
    let result =
      Repl.for_submission
        repl
        (capture "outer.chatml" "open Catalog")
        ~checkpoint:(fun () ->
          Int.incr polls;
          if !polls = stop
          then
            nested
            := Some
                 (Repl.for_submission
                    repl
                    (capture "inner.chatml" "open Catalog")
                    ~checkpoint))
    in
    (match result with
     | Error Repl.Error.Stale_context -> ()
     | _ -> failwith "outer admission overwrote reentrant admission");
    let inner =
      match Option.value_exn !nested with
      | Ok context -> context
      | Error _ -> failwith "nested admission was rejected"
    in
    let reference = resolve inner in
    assert (
      Result.is_ok (Repl.validate_reference repl ~context:inner reference ~checkpoint));
    match Repl.for_submission repl (capture "third.chatml" "") ~checkpoint with
    | Error Repl.Error.Snippet_limit -> ()
    | _ -> failwith "outer admission lost or replaced the inner ledger")
;;

let%test_unit "actual checker carries imported open from snippet A into B" =
  let input = fixture () in
  let checked = F.checked input |> F.ok in
  let catalog_id = P.Module_id.of_implementation (id "catalog.chatml") in
  let exports =
    Session.checked checked catalog_id |> Option.value_exn |> Check.Checked_unit.exports
  in
  let values =
    match Check.Public_module.value_scheme exports with
    | T.Module (T.Row (values, T.Empty_row)) -> values
    | _ -> failwith "checked catalog exports are not a completed module row"
  in
  let public =
    T.Configured_resolution.Public_module.create
      ~values
      ~types:(Check.Public_module.types exports)
  in
  let repl = scope input in
  let a = submit repl "a.chatml" "open Catalog" in
  let machine = T.create_state ~checkpoint () in
  let observed = ref [] in
  let require ~source reference =
    match Repl.validate_reference repl ~context:source reference ~checkpoint with
    | Ok () ->
      observed := reference :: !observed;
      Ok public
    | Error _ -> Error (Chatml_diagnostic.create "REPL reference rejected")
  in
  let check context env types =
    let state = T.for_source machine (Lookup.source context) in
    let configured = T.Configured_resolution.create context ~require ~checkpoint in
    let state = { state with configured = Some configured } in
    let parsed = Parse.parse_captured (Lookup.source context) ~checkpoint |> F.ok in
    T.with_unit_level state ~f:(fun () ->
      List.fold
        (Parse.Unit.program parsed).stmts
        ~init:(env, types)
        ~f:(fun (env, types) node ->
          let env, types, _ =
            T.infer_stmt_with_exports ~span:node.span state env node.value types
          in
          env, types))
  in
  let env =
    T.init_env_with_surface ~checkpoint Chatml.Chatml_builtin_surface.core_surface
  in
  let types =
    T.init_types_with_surface ~checkpoint Chatml.Chatml_builtin_surface.core_surface
  in
  let env, types = check a env types in
  let imported = T.Value_scope.find env ~name:"value" |> Option.value_exn in
  let origin = T.Value_scope.Binding.origin imported |> Option.value_exn in
  [%test_eq: string] (S.Id.relative_path (fst (Origin.import_source origin))) "a.chatml";
  let b = submit repl "b.chatml" "let next = value" in
  let env, _types = check b env types in
  assert (Option.is_some (T.Value_scope.find env ~name:"next"));
  [%test_eq: string list]
    (List.rev_map !observed ~f:(fun event ->
       S.Id.relative_path (fst (Reference.use_source event))))
    [ "a.chatml"; "b.chatml" ];
  [%test_eq: string list]
    (List.rev_map !observed ~f:(fun event ->
       S.Id.relative_path (fst (Origin.import_source (Reference.origin event)))))
    [ "a.chatml"; "a.chatml" ];
  [%test_eq: string] (S.Id.relative_path (fst (Origin.import_source origin))) "a.chatml"
;;
