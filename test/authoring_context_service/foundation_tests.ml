open! Core
module P = Agent_protocol
module Owner = Agent_server.Authoring_context_owner
module Service = Agent_server.Authoring_context_service_core
module Methods = Agent_server.Authoring_context_methods
module Selection = Agent_server.Authoring_script_selection
module Ledger = Chatml_authoring_context

let ok = function
  | Ok value -> value
  | Error _ -> failwith "unexpected fixture admission failure"
;;

let with_supplied_owner f =
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
               ~max_context_source_bytes:2048
               ~max_total_source_bytes:4096
             |> Or_error.ok_exn)
          ~analysis_policy:(Service_tests.analysis_policy ())
          ~protocol_limits:
            (P.Authoring.Limits.create
               ~max_sources:4
               ~max_source_bytes:1024
               ~max_context_source_bytes:2048
               ~max_request_bytes:4096
               ~max_result_bytes:4096
               ~max_path_candidates:16
             |> ok)
          ~limits:
            (Service.Limits.create
               ~max_connections:1
               ~max_active_requests_per_connection:2
               ~max_roots_per_connection:1
               ~max_response_bytes:4096
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
          ~contents:"<developer>draft</developer>"
        |> ok
      in
      let context =
        match
          Methods.handle_core
            service
            ~actor
            ~connection
            (P.Authoring.Wire.Request.Context_open request)
        with
        | Ok (Context_open context) -> context
        | Ok _ | Error _ -> failwith "document context open failed"
      in
      f service actor connection (P.Authoring.Context.Expected.of_view context)))
;;

let%test_unit "feature checks, expired Current and final publication head CAS" =
  with_supplied_owner (fun service actor connection expected ->
    let published = ref 0 in
    let borrowed = ref None in
    let invoke
          ?(feature = Service.Selected_feature.Semantic)
          ?(after = Fn.id)
          ?(final_check = fun () -> Ok ())
          ()
      =
      Owner.with_current
        ~feature
        ~final_check
        ~publish:(fun value ->
          incr published;
          Ok value)
        service
        ~actor
        ~connection
        ~expected
        ~checkpoint:ignore
        ~f:(fun current ~checkpoint:_ ->
          borrowed := Some current;
          after ();
          Ok 7)
    in
    [%test_eq: int] (invoke () |> ok) 7;
    [%test_eq: int] !published 1;
    (match !borrowed with
     | Some current ->
       (match Service.Current.document_inputs current ~checkpoint:ignore with
        | Error Service.Error.Stale_context -> ()
        | Ok _ | Error _ -> failwith "expired Current remained usable")
     | None -> failwith "request did not borrow Current");
    let contexts () = Service.Project.Project_cache.contexts service.project_cache in
    let changed_project_head () =
      Service.with_gate service (fun () ->
        service.project_cache
        <- Service.Project.Project_cache.transition
             service.project_cache
             ~contexts:(contexts ()))
    in
    let changed_document_head () =
      Service.with_gate service (fun () ->
        service.document_cache
        <- Service.Document.Document_cache.transition
             service.document_cache
             ~contexts:(contexts ()))
    in
    List.iter
      [ "project", changed_project_head; "document", changed_document_head ]
      ~f:(fun (name, after) ->
        match invoke ~after () with
        | Error Service.Error.Stale_context -> ()
        | Ok _ | Error _ -> failwith (name ^ " cache head published stale result"));
    (match invoke ~final_check:(fun () -> Error Service.Error.Stale_context) () with
     | Error Service.Error.Stale_context -> ()
     | Ok _ | Error _ -> failwith "rejected final selection published");
    (match invoke ~feature:Service.Selected_feature.Bundle_upload () with
     | Error Service.Error.Unselected_feature -> ()
     | Ok _ | Error _ -> failwith "unselected sibling feature borrowed context");
    [%test_eq: int] !published 1;
    [%test_eq: int] (invoke ~feature:Service.Selected_feature.Lexical () |> ok) 7;
    [%test_eq: int] (invoke ~feature:Service.Selected_feature.Assistance () |> ok) 7;
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
