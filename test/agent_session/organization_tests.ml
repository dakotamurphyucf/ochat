open! Core
open Fixtures
module P = Agent_protocol
module S = Agent_session
module V = P.Session_organization.Values
module Patch = P.Session_organization.Patch

let principal_with_scopes id scopes =
  P.Principal.create
    ~id:(P.Id.Principal.of_string id |> protocol_ok)
    ~authentication_kind:"test.local"
    ~scopes
    ~attributes:[]
  |> protocol_ok
;;

let patch project ~add ~remove =
  Patch.create ~project ~add_collections:add ~remove_collections:remove |> protocol_ok
;;

let%expect_test "membership shares metadata CAS and preserves unrelated execution state" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let project = P.Id.Project.of_string "prj_membership" |> protocol_ok in
    let collection = P.Id.Collection.of_string "col_membership" |> protocol_ok in
    let plan =
      S.Session_organization_transition.create
        initial
        ~expected_metadata_revision:0L
        ~patch:(patch (Set project) ~add:[ collection ] ~remove:[])
      |> protocol_ok
    in
    let recovered = restore_delta (S.Session_organization_transition.delta plan) in
    let next = S.Session_delta.apply initial recovered |> protocol_ok in
    let no_op =
      S.Session_organization_transition.create
        next
        ~expected_metadata_revision:1L
        ~patch:(patch (Set project) ~add:[ collection ] ~remove:[])
      |> protocol_ok
    in
    let stale =
      S.Session_metadata_transition.apply
        next
        ~expected_metadata_revision:0L
        ~patch:
          (P.Session_metadata.Patch.create
             ~name:(Set "stale")
             ~set_labels:[]
             ~remove_labels:[]
           |> protocol_ok)
    in
    print_s
      [%sexp
        (V.equal
           next.identity.organization
           (S.Session_organization_transition.values plan)
         : bool)
      , (Int64.equal next.identity.metadata_revision 1L : bool)
      , (P.Id.Workspace_instance.equal
           next.spec.workspace_instance.id
           initial.spec.workspace_instance.id
         : bool)
      , (not (S.Session_organization_transition.changed no_op) : bool)
      , (V.equal (S.Session_organization_transition.additions no_op) V.empty : bool)
      , ((match stale with
          | Error error -> P.Error.code_to_string error.code
          | Ok _ -> "unexpected")
         : string)];
    let exhausted =
      { next with identity = { next.identity with metadata_revision = Int64.max_value } }
    in
    let changed =
      S.Session_organization_transition.create
        exhausted
        ~expected_metadata_revision:Int64.max_value
        ~patch:(patch Clear ~add:[] ~remove:[])
    in
    let unchanged =
      S.Session_organization_transition.create
        exhausted
        ~expected_metadata_revision:Int64.max_value
        ~patch:(patch Keep ~add:[] ~remove:[])
    in
    print_s [%sexp (Result.is_error changed : bool), (Result.is_ok unchanged : bool)]);
  [%expect
    {|
    (true true true true true conflict)
    (true true)
    |}]
;;

let member json name =
  match Document_schema.Json.field json ~name with
  | Value value -> value
  | _ -> raise_s [%sexp "missing membership fixture field", (name : string)]
;;

let set json name value =
  match json with
  | `Object fields -> `Object (List.Assoc.add fields ~equal:String.equal name value)
  | _ -> raise_s [%sexp "expected membership fixture object"]
;;

let%expect_test "membership update preserves unknown canonical organization fields" =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let authored = state_document initial in
    let json = Document_schema.Document.json authored in
    let payload = member json "payload" in
    let identity = member payload "identity" in
    let organization =
      set (member identity "organization") "future_membership" (`String "retained")
    in
    let json =
      set
        json
        "payload"
        (set payload "identity" (set identity "organization" organization))
    in
    let document =
      Document_schema.Document.inspect ~limits:document_limits json |> document_ok
    in
    let previous =
      S.Session_state_document.decode ~limits:document_limits document |> document_ok
    in
    let project = P.Id.Project.of_string "prj_unknown_retention" |> protocol_ok in
    let plan =
      S.Session_organization_transition.create
        initial
        ~expected_metadata_revision:0L
        ~patch:(patch (Set project) ~add:[] ~remove:[])
      |> protocol_ok
    in
    let next =
      S.Session_delta.apply initial (S.Session_organization_transition.delta plan)
      |> protocol_ok
    in
    let encoded =
      S.Session_state_document.with_value previous next
      |> fun state ->
      S.Session_state_document.encode state ~limits:document_limits |> document_ok
    in
    let organization =
      member (member (Document_schema.Document.payload encoded) "identity") "organization"
    in
    print_s
      [%sexp
        (String.equal
           (Jsonaf.to_string (member organization "future_membership"))
           (Jsonaf.to_string (`String "retained"))
         : bool)
      , (V.equal (V.of_json organization |> protocol_ok) next.identity.organization
         : bool)]);
  [%expect {| (true true) |}]
;;

let%expect_test
    "cancelled caller does not undo admitted commit and publication follows unlock"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
        let initial =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let block_commit = ref false in
        let fail_commit = ref false
        and next_attachment = ref 0 in
        let inside_admission = ref false
        and publication_outside_admission = ref false in
        let committed, committed_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let actor =
          S.Session_actor.create
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~mailbox_capacity:32
            ~compaction_env:None
            ~initial_state:initial
            ~operation_worker:None
            ~persistence:
              { archive_reference
              ; commit =
                  (fun ~command_audit:_ ~previous:_ _ ->
                    if !fail_commit
                    then
                      Error
                        (P.Error.create
                           Persistence_error
                           ~message:"injected write failure"
                           ~retryable:false
                           ())
                    else (
                      if !block_commit
                      then (
                        Eio.Promise.resolve committed_u ();
                        Eio.Promise.await release);
                      Ok ()))
              }
            ~services:
              { now = (fun () -> timestamp)
              ; create_attachment_id =
                  (fun () ->
                    Int.incr next_attachment;
                    P.Id.Attachment.of_string
                      (sprintf "att_organization_%d" !next_attachment)
                    |> protocol_ok)
              ; create_reclaim_token = (fun () -> "organization-token")
              ; job_results = None
              ; monotonic_now = (fun () -> Mtime.min_stamp)
              ; schedule_limits = S.Staged_schedules.default_limits
              ; notification_limits = S.Staged_notifications.default_limits
              ; ingress_limits = S.Staged_ingress.default_limits
              ; subscription_limits = S.Staged_subscriptions.default_limits
              ; state_committed =
                  (fun _ _ -> publication_outside_admission := not !inside_admission)
              }
        in
        let admission =
          S.Session_organization_admission.create
            (fun ~principal:_ ~host_id:_ ~additions:_ ~commit ->
               inside_admission := true;
               Exn.protect ~f:commit ~finally:(fun () -> inside_admission := false))
        in
        S.Session_actor.set_organization_admission actor admission |> protocol_ok;
        let writer, _ =
          S.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
        in
        let principal =
          principal_with_scopes
            "pri_organization_actor"
            (P.Scope.Set.of_list [ Send_messages; Manage_organization ])
        in
        let project = P.Id.Project.of_string "prj_cancelled_commit" |> protocol_ok in
        let request =
          P.Session_organization.Request.
            { host_id = P.Id.Server.of_string "srv_organization_actor" |> protocol_ok
            ; session_id = initial.identity.session_id
            ; attachment_id = writer.id
            ; expected_metadata_revision = 0L
            ; patch = patch (Set project) ~add:[] ~remove:[]
            ; idempotency_key =
                P.Idempotency_key.of_string "organization-actor-cancel" |> protocol_ok
            }
        in
        block_commit := true;
        publication_outside_admission := false;
        let context, context_u = Eio.Promise.create () in
        let caller =
          Eio.Fiber.fork_promise ~sw (fun () ->
            try
              Eio.Cancel.sub (fun cancel ->
                Eio.Promise.resolve context_u cancel;
                ignore (S.Session_actor.update_organization actor ~principal request));
              false
            with
            | Eio.Cancel.Cancelled _ -> true)
        in
        let cancel = Eio.Promise.await context in
        Eio.Promise.await committed;
        Eio.Cancel.cancel cancel Exit;
        Eio.Promise.resolve release_u ();
        let cancelled =
          match Eio.Promise.await caller with
          | Ok value -> value
          | Error exn -> raise exn
        in
        let state = S.Session_actor.state actor |> protocol_ok in
        print_s
          [%sexp
            (cancelled : bool)
          , (Int64.equal state.identity.metadata_revision 1L : bool)
          , (Option.equal
               P.Id.Project.equal
               state.identity.organization.project_id
               (Some project)
             : bool)
          , (!publication_outside_admission : bool)];
        fail_commit := true;
        let rejected =
          S.Session_actor.update_organization
            actor
            ~principal
            { request with
              expected_metadata_revision = 1L
            ; patch = patch Clear ~add:[] ~remove:[]
            ; idempotency_key =
                P.Idempotency_key.of_string "organization-actor-failure" |> protocol_ok
            }
        in
        let after = S.Session_actor.state actor |> protocol_ok in
        print_s
          [%sexp
            (Result.is_error rejected : bool)
          , (V.equal state.identity.organization after.identity.organization : bool)
          , (Int64.equal after.identity.metadata_revision 1L : bool)];
        S.Session_actor.shutdown actor)));
  [%expect
    {|
    (true true true true)
    (true true true)
    |}]
;;

let%expect_test
    "legacy conversion introduces empty references but current omission rejects"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let current = state_document initial in
    let payload = Document_schema.Document.payload current in
    let identity = member payload "identity" in
    let identity =
      match identity with
      | `Object fields ->
        `Object (List.Assoc.remove fields "organization" ~equal:String.equal)
      | _ -> raise_s [%sexp "expected identity fixture"]
    in
    let identity = set identity "future_identity" (`String "kept") in
    let payload = set payload "identity" identity in
    let legacy =
      Document_schema.Document.create
        ~limits:document_limits
        ~kind:"session.state"
        ~version:5
        ~payload
      |> document_ok
    in
    let converted =
      S.Session_state_document.upgrade legacy ~limits:document_limits |> document_ok
    in
    let decoded =
      S.Session_state_document.decode ~limits:document_limits converted |> document_ok
    in
    let missing_current =
      Document_schema.Document.create
        ~limits:document_limits
        ~kind:"session.state"
        ~version:(Document_schema.Document.version current)
        ~payload
      |> document_ok
    in
    print_s
      [%sexp
        (V.equal (S.Session_state_document.value decoded).identity.organization V.empty
         : bool)
      , (String.equal
           (Jsonaf.to_string
              (member
                 (member (Document_schema.Document.payload converted) "identity")
                 "future_identity"))
           "\"kept\""
         : bool)
      , (Result.is_error
           (S.Session_state_document.decode ~limits:document_limits missing_current)
         : bool)]);
  [%expect {| (true true true) |}]
;;

let%expect_test
    "owning switch cancellation completes acknowledged membership publication before \
     ending lifetime"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun outer_sw ->
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
        let persisted = ref None
        and published = ref None in
        let inside_admission = ref false
        and admitted = ref false in
        let lifetime_ended = ref false in
        let owner =
          Eio.Fiber.fork_promise ~sw:outer_sw (fun () ->
            Exn.protect
              ~finally:(fun () -> lifetime_ended := true)
              ~f:(fun () ->
                Eio.Cancel.sub (fun owner_context ->
                  Eio.Switch.run (fun sw ->
                    let initial =
                      actor_state
                        ~workspace_instance
                        ~liveness:Detached
                        ~start_immediately:false
                    in
                    let next_attachment = ref 0 in
                    let actor =
                      S.Session_actor.create
                        ~sw
                        ~clock:(Eio.Stdenv.clock env)
                        ~mailbox_capacity:32
                        ~compaction_env:None
                        ~initial_state:initial
                        ~operation_worker:None
                        ~persistence:
                          { archive_reference
                          ; commit =
                              (fun ~command_audit:_ ~previous:_ transition ->
                                if !admitted
                                then (
                                  persisted := Some transition;
                                  Eio.Cancel.cancel owner_context Exit);
                                Ok ())
                          }
                        ~services:
                          { now = (fun () -> timestamp)
                          ; create_attachment_id =
                              (fun () ->
                                Int.incr next_attachment;
                                P.Id.Attachment.of_string
                                  (sprintf "att_metadata_%d" !next_attachment)
                                |> protocol_ok)
                          ; create_reclaim_token = (fun () -> "metadata-token")
                          ; job_results = None
                          ; monotonic_now = (fun () -> Mtime.min_stamp)
                          ; schedule_limits = S.Staged_schedules.default_limits
                          ; notification_limits = S.Staged_notifications.default_limits
                          ; ingress_limits = S.Staged_ingress.default_limits
                          ; subscription_limits = S.Staged_subscriptions.default_limits
                          ; state_committed =
                              (fun state events ->
                                if !admitted
                                then (
                                  Eio.Fiber.yield ();
                                  published := Some (state, events, not !inside_admission)))
                          }
                    in
                    let admission =
                      S.Session_organization_admission.create
                        (fun ~principal:_ ~host_id:_ ~additions:_ ~commit ->
                           inside_admission := true;
                           Exn.protect ~f:commit ~finally:(fun () ->
                             inside_admission := false))
                    in
                    S.Session_actor.set_organization_admission actor admission
                    |> protocol_ok;
                    let writer, _ =
                      S.Session_actor.attach actor ~mode:Read_write ~subscribe:false
                      |> protocol_ok
                    in
                    let principal =
                      principal_with_scopes
                        "pri_organization_owner_cancel"
                        (P.Scope.Set.of_list [ Send_messages; Manage_organization ])
                    in
                    let project =
                      P.Id.Project.of_string "prj_owner_cancel" |> protocol_ok
                    in
                    admitted := true;
                    ignore
                      (S.Session_actor.update_organization
                         actor
                         ~principal
                         P.Session_organization.Request.
                           { host_id =
                               P.Id.Server.of_string "srv_owner_cancel" |> protocol_ok
                           ; session_id = initial.identity.session_id
                           ; attachment_id = writer.id
                           ; expected_metadata_revision = 0L
                           ; patch = patch (Set project) ~add:[] ~remove:[]
                           ; idempotency_key =
                               P.Idempotency_key.of_string "organization-owner-cancel"
                               |> protocol_ok
                           })))))
        in
        let owner_cancelled =
          match Eio.Promise.await owner with
          | Error (Eio.Cancel.Cancelled _) -> true
          | Error exn -> raise exn
          | Ok () -> false
        in
        let transition = Option.value_exn !persisted in
        let state, events, outside_lock = Option.value_exn !published in
        let recovered =
          S.Session_delta.apply
            (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false)
            transition.S.Session_transition.delta
          |> protocol_ok
        in
        print_s
          [%sexp
            (owner_cancelled : bool)
          , (!lifetime_ended : bool)
          , (outside_lock : bool)
          , (Int64.equal state.S.Session_state.identity.metadata_revision 1L : bool)
          , (V.equal state.identity.organization recovered.identity.organization : bool)
          , (List.exists events ~f:(fun event ->
               match
                 P.Event.Durable.Payload.of_json
                   ~kind:event.P.Event.Durable.kind
                   event.payload
                 |> protocol_ok
               with
               | Session_updated summary ->
                 V.equal summary.organization state.identity.organization
               | _ -> false)
             : bool)])));
  [%expect {| (true true true true true true) |}]
;;
