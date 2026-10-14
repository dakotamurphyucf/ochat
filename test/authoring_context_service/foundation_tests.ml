open! Core
module P = Agent_protocol
module Service = Agent_server.Authoring_context_service
module Testing = Agent_server.Authoring_context_service.For_testing
module Selection = Service.Semantic.Script_selection
module Ledger = Chatml_authoring_context

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected fixture admission failure"
;;

let with_supplied_owner
      ?(contents = "<developer>draft</developer>")
      ?(result_bytes = 4096)
      ?(on_context = fun (_ : P.Authoring.Context.t) -> ())
      f
  =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let host = P.Id.Server.create () in
      let principal =
        P.Principal.create
          ~id:(P.Id.Principal.of_string "pri_authoring_foundation" |> ok)
          ~authentication_kind:"test"
          ~scopes:P.Scope.Set.empty
          ~attributes:[]
        |> ok
      in
      let actor = Operator_authorization.trusted_local principal in
      let connection =
        Agent_server.Connection_context.create_authenticated
          ~actor
          ~connection_id:"foundation owner"
          ~transport:In_memory
          ~publish_notification:(fun _ -> ())
          ~max_attachments:1
      in
      let service =
        Service.create
          ~sw
          ~clock:env#mono_clock
          ~host
          ~sources:None
          ~upload:None
          ~supplied_document:
            (Some (Service.Supplied_document_policy.create ~authorize:(fun _ -> true)))
          ~selection_policy:
            (Service.Selection_policy.create
               ~project_contract:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
               ~document_static:(fun _ ~requested_environment:_ -> None))
          ~context_limits:
            (Ledger.Limits.create
               ~max_contexts:2
               ~max_sources:4
               ~max_open_documents:1
               ~max_document_bytes:1024
               ~max_effective_input_bytes:2048
               ~max_context_source_bytes:4096
               ~max_total_source_bytes:4096
             |> Or_error.ok_exn)
          ~analysis_policy:(Service_tests.analysis_policy ())
          ~protocol_limits:
            (P.Authoring.Limits.create
               ~max_sources:4
               ~max_source_bytes:1024
               ~max_context_source_bytes:4096
               ~max_request_bytes:4096
               ~max_result_bytes:result_bytes
               ~max_path_candidates:16
             |> ok)
          ~limits:
            (Service.Limits.create
               ~max_connections:1
               ~max_active_requests_per_connection:2
               ~max_roots_per_connection:1
               ~max_response_bytes:result_bytes
               ~max_provisional_results:8
             |> Or_error.ok_exn)
      in
      let response =
        Service_tests.initialize_response_with_features
          ~enabled_features:
            [ P.Authoring.Features.core
            ; P.Authoring.Features.semantic
            ; P.Authoring.Features.lexical
            ; P.Authoring.Features.assistance
            ]
          ~server_id:host
          ~principal
      in
      Service.publish_initialize_and_wait service ~connection ~response |> ok;
      let request =
        P.Authoring.Context.Open_request.create_supplied_document
          ~member:"draft.chatmd"
          ~default_environment:None
          ~contents
        |> ok
      in
      let context =
        match
          Service.handle_core
            service
            ~actor
            ~connection
            (P.Authoring.Wire.Request.Context_open request)
        with
        | Ok (Context_open context) -> context
        | Error error ->
          failwith (P.Error.code_to_string (Service.Error.to_protocol error).code)
        | Ok _ -> failwith "document context open returned wrong method"
      in
      on_context context;
      f service actor connection (P.Authoring.Context.Expected.of_view context) env))
;;

let%test_unit "feature checks, expired Current and final publication head CAS" =
  with_supplied_owner (fun service actor connection expected _env ->
    let published = ref 0 in
    let invoke
          ?(feature = Testing.Semantic)
          ?(after = fun _ -> ())
          ?(final_check = fun () -> Ok ())
          ()
      =
      Testing.with_publication_probe
        ~feature
        ~final_check
        ~publish:(fun () -> incr published)
        service
        ~actor
        ~connection
        ~expected
        ~checkpoint:ignore
        ~after_borrow:after
    in
    let expired = invoke () |> ok in
    assert (Testing.expired_borrow_rejects_inputs_index_and_capture expired);
    [%test_eq: int] !published 1;
    List.iter
      [ "project", Testing.Project_cache
      ; "document", Testing.Document_cache
      ; "analysis index", Testing.Analysis_index
      ; "provisional index", Testing.Provisional_index
      ]
      ~f:(fun (name, head) ->
        match invoke ~after:(fun probe -> Testing.replace_head probe head |> ok) () with
        | Error Testing.Stale_context -> ()
        | Ok _ | Error _ -> failwith (name ^ " cache head published stale result"));
    (match invoke ~final_check:(fun () -> Error Testing.Stale_context) () with
     | Error Testing.Stale_context -> ()
     | Ok _ | Error _ -> failwith "rejected final selection published");
    (match invoke ~feature:Testing.Bundle_upload () with
     | Error Testing.Unselected_feature -> ()
     | Ok _ | Error _ -> failwith "unselected sibling feature borrowed context");
    [%test_eq: int] !published 1;
    ignore (invoke ~feature:Testing.Lexical () |> ok : Testing.expired_borrow);
    ignore (invoke ~feature:Testing.Assistance () |> ok : Testing.expired_borrow);
    [%test_eq: int] !published 3)
;;

let%test_unit "script-selection limit admission is finite" =
  let module Fixtures = Chatml_project_build_tests.Fixtures in
  let module Contract = Chatml.Chatml_compiler_contract in
  let fixture = Fixtures.make () in
  let build = Fixtures.compose fixture |> ok in
  let contract = Fixtures.contract fixture in
  let limits =
    Selection.Limits.create ~max_identity_bytes:(4 * 1024 * 1024) ~max_work_steps:100_000
    |> Or_error.ok_exn
  in
  let selected =
    match Selection.Selected.admit ~build ~contract ~limits ~checkpoint:ignore with
    | Ok selected -> selected
    | Error error -> Error.raise_s (Selection.Error.sexp_of_t error)
  in
  assert (
    Chatml_source.Digest.equal
      (Selection.Selected.prepared_fingerprint selected)
      (Chatml_prepared_program.fingerprint (Chatml_project_build.prepared build)));
  let wrong = Contract.for_target Contract.Target.Tool_v1 ~checkpoint:ignore in
  (match Selection.Selected.admit ~build ~contract:wrong ~limits ~checkpoint:ignore with
   | Error Selection.Error.Contract_mismatch -> ()
   | Ok _ | Error _ -> failwith "foreign selected contract admitted");
  let tiny =
    Selection.Limits.create ~max_identity_bytes:1 ~max_work_steps:100_000
    |> Or_error.ok_exn
  in
  (match Selection.Selected.admit ~build ~contract ~limits:tiny ~checkpoint:ignore with
   | Error Selection.Error.Identity_limit -> ()
   | Ok _ | Error _ -> failwith "oversized contract identity admitted");
  let sentinel = Failure "selection checkpoint identity" in
  match
    Selection.Selected.admit ~build ~contract ~limits ~checkpoint:(fun () ->
      raise sentinel)
  with
  | exception exn when phys_equal exn sentinel -> ()
  | Ok _ | Error _ | (exception _) -> failwith "selection swallowed checkpoint exception"
;;
