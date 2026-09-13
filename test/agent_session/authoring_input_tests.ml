open Core
open Fixtures
module A = Agent_session.Session_actor
module W = Agent_session.Operation_worker
module M = Chat_response.Authoring_materialization
module P = Agent_protocol
module H = P.History
module Codec = Agent_session.History_codec

let canonical history =
  List.map history ~f:(fun entry ->
    Chat_response.Moderation.Effective_entry.{ entry; provenance = Canonical })
;;

let outcomes backend =
  Agent_session.Memory_backend.events_after backend 0L
  |> protocol_ok
  |> List.filter_map ~f:(fun event ->
    match event.P.Event.Durable.kind with
    | Operation_completed -> Some "completed"
    | Operation_failed -> Some "failed"
    | Operation_cancelled -> Some "cancelled"
    | _ -> None)
;;

let materialization
      ?(policy = Chatmd_shell_spec.Extension_spec.Auto)
      env
      (input : W.Input.t)
  =
  let module V = Chat_response.Authoring_validation in
  let module C = Chat_response.Tool_capability in
  let module Policy = Chat_response.Authoring_policy in
  let host =
    V.create_host
      ~runtime_identity:"actor-guidance-test"
      ~targets:[ One_off_script ]
      ~moderator_surface:Ordinary
      ~compilation:Chatml_compilation.default_limits
    |> Result.ok_or_failwith
  in
  let context =
    Chat_response.Authoring_context.create ~secret:"actor-guidance-test-key" ()
    |> Result.ok_or_failwith
  in
  let registrations =
    [ Agent_session.Run_chatml_tool.registration
        ~env
        ~policy:Chat_response.One_off_request.default_policy
        ~services:(fun () -> failwith "guidance must not execute tools")
    ; Agent_session.Authoring_validation_tool.registration ~env ~host
    ; Agent_session.Authoring_context_tool.registration ~host |> Result.ok_or_failwith
    ]
  in
  let ceiling =
    C.create
      ~metadata:
        (List.filter_map registrations ~f:(fun r ->
           Option.map r.Chat_response.Agent_runtime.authoring_metadata ~f:(fun metadata ->
             r.implementation.info.function_.name, metadata)))
      ~owner:(P.Id.Session.to_string input.session_id)
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "fixture")
      (List.map registrations ~f:(fun r -> r.implementation_revision, r.implementation))
    |> function
    | Ok x -> x
    | Error e -> raise_s [%sexp (e : C.error)]
  in
  let catalog = M.catalog context ~host |> Result.ok_or_failwith in
  let policy =
    Policy.resolve ~policy ~catalog ~ceiling ~selected_names:[ "run_chatml" ] ()
    |> function
    | Ok x -> x
    | Error e -> raise_s [%sexp (e : Policy.error)]
  in
  M.create
    ~context
    ~host
    ~policy
    ~capabilities:(Policy.capabilities policy)
    ~scope:
      (M.session_scope ~session_id:input.session_id ~generation:input.session_generation)
    ()
  |> Result.ok_or_failwith
;;

let guidance state =
  List.filter
    state.Agent_session.Session_state.conversation.canonical_history
    ~f:(fun e ->
      match e.H.provenance with
      | Runtime_authoring _ -> true
      | _ -> false)
;;

let finished actor =
  let rec loop () =
    let state = A.state actor |> protocol_ok in
    match state.active_operation with
    | None -> state
    | Some _ ->
      Eio.Fiber.yield ();
      loop ()
  in
  loop ()
;;

let worker_config env post_stream =
  Agent_session.Turn_worker.Config.
    { env
    ; response_dir = Eio.Path.(Eio.Stdenv.fs env / "/tmp")
    ; tools = []
    ; tool_tbl = String.Table.create ()
    ; temperature = None
    ; max_output_tokens = None
    ; reasoning = None
    ; moderator = None
    ; permission_profile =
        permission_policy
          ~tool_default:Deny
          ~fallback:Fallback_deny
          ~evaluator:None
          ~reviewer:None
    ; review_permission = (fun _ -> assert false)
    ; history_compaction = false
    ; parallel_tool_calls = true
    ; model = Openai.Responses.Request.O3
    ; prompt_cache_key = None
    ; prompt_cache_retention = None
    ; post_stream = Some post_stream
    ; agent_page_classifications = []
    ; delegated_permission_tools = String.Set.empty
    ; redact_tool_payload = (fun ~name:_ text -> text)
    }
;;

let%expect_test "provider input contains committed guidance once across foreground turns" =
  let requests = ref 0 in
  with_handoff_actor
    ~make_worker:(fun env ready ->
      let retained = ref None in
      let post_stream ~sw:_ ~inputs =
        incr requests;
        let actor = Eio.Promise.await ready in
        let saved = guidance (A.state actor |> protocol_ok) in
        assert (List.length saved = 1);
        List.iter saved ~f:(fun entry ->
          assert (
            List.exists inputs ~f:(fun item ->
              Jsonaf.exactly_equal entry.payload (Openai.Responses.Item.jsonaf_of_t item))));
        Stdlib.List.to_seq []
      in
      Agent_session.Turn_worker.create
        ~authoring_context:(fun ~input ->
          match !retained with
          | Some plan -> Ok plan
          | None ->
            let plan = materialization env input in
            retained := Some plan;
            Ok plan)
        (worker_config env post_stream))
    (fun _ actor writer backend ->
       let first = finished actor in
       let restored =
         Agent_session.Session_state.sexp_of_t first
         |> Sexp.to_string_mach
         |> Agent_session.Session_persistence.restore_snapshot
         |> store_ok
       in
       assert (List.equal H.equal_entry (guidance first) (guidance restored));
       let references =
         Agent_session.Session_state.authoring_references restored
         |> protocol_ok
         |> Chat_response.Authoring_reference_index.receipts
       in
       assert (
         List.equal
           H.Id.equal
           (List.map (guidance restored) ~f:(fun entry -> entry.H.id))
           (List.map references ~f:(fun receipt -> receipt.entry_id)));
       let id =
         History_entry.Id.create ~namespace:"user" ~sequence:1 |> Result.ok_or_failwith
       in
       A.submit_message
         actor
         ~attachment_id:writer.id
         (Codec.user_text ~id "follow up" |> Codec.to_protocol)
       |> protocol_ok
       |> ignore;
       let final = finished actor in
       assert (List.equal String.equal (outcomes backend) [ "completed"; "completed" ]);
       print_s [%sexp (!requests : int), (List.length (guidance final) : int)]);
  [%expect {| (2 1) |}]
;;

let%expect_test
    "failed guidance persistence prevents provider input and saves no history reservation"
  =
  let requests = ref 0 in
  let before = ref None in
  with_handoff_actor
    ~reject:(fun next -> not (List.is_empty (guidance next.state)))
    ~make_worker:(fun env ready ->
      Agent_session.Turn_worker.create
        ~authoring_context:(fun ~input ->
          before := Some (A.state (Eio.Promise.await ready) |> protocol_ok);
          Ok (materialization env input))
        (worker_config env (fun ~sw:_ ~inputs:_ ->
           incr requests;
           Stdlib.List.to_seq [])))
    (fun _ actor _ backend ->
       let final = finished actor in
       assert (List.equal String.equal (outcomes backend) [ "failed" ]);
       let before = Option.value_exn !before in
       assert (
         Int64.equal
           before.conversation.reserved_history_through
           final.conversation.reserved_history_through);
       assert (
         List.equal
           H.equal_entry
           before.conversation.canonical_history
           final.conversation.canonical_history);
       assert (
         Option.equal
           Jsonaf.exactly_equal
           before.conversation.authoring_reference_index
           final.conversation.authoring_reference_index);
       print_s [%sexp (!requests : int), (List.length (guidance final) : int)]);
  [%expect {| (0 0) |}]
;;

let%expect_test
    "manual model input rediscovers compacted preloads without reinserting prose"
  =
  let module Spec = Chatmd_shell_spec.Extension_spec in
  let module G = P.Authoring_guidance in
  let policy = ref (Spec.Preload [ "chatml.tasks" ]) in
  let requests = ref 0 in
  with_handoff_actor
    ~make_worker:(fun env ready ->
      let plans = ref [] in
      let post_stream ~sw:_ ~inputs =
        incr requests;
        let saved = guidance (A.state (Eio.Promise.await ready) |> protocol_ok) in
        List.iter saved ~f:(fun entry ->
          assert (
            List.exists inputs ~f:(fun item ->
              Jsonaf.exactly_equal
                entry.H.payload
                (Openai.Responses.Item.jsonaf_of_t item))));
        (match !policy with
         | Preload _ -> assert (List.length saved > 1)
         | Manual ->
           assert (List.length saved = 1);
           let entry = List.hd_exn saved in
           (match entry.provenance with
            | Runtime_authoring metadata ->
              assert (G.equal_purpose metadata.purpose Rediscovery);
              assert (List.for_all metadata.topics ~f:(fun topic -> not topic.G.complete));
              assert (
                List.exists metadata.topics ~f:(fun topic ->
                  String.equal topic.G.id "chatml.tasks"))
            | _ -> assert false);
           let encoded = Jsonaf.to_string entry.payload in
           assert (not (String.is_substring encoded ~substring:"ochat_authoring_context"));
           assert (not (String.is_substring encoded ~substring:"ochat_validate"))
         | Auto -> assert false);
        Stdlib.List.to_seq []
      in
      Agent_session.Turn_worker.create
        ~authoring_context:(fun ~input ->
          match List.Assoc.find !plans !policy ~equal:Spec.equal_policy with
          | Some plan -> Ok plan
          | None ->
            let plan = materialization ~policy:!policy env input in
            plans := (!policy, plan) :: !plans;
            Ok plan)
        (worker_config env post_stream))
    (fun _env actor writer _backend ->
       let first = finished actor in
       assert (!requests = 1);
       A.compact
         actor
         ~attachment_id:writer.id
         ~expected_revision:(Some first.counters.revision)
       |> protocol_ok
       |> ignore;
       let compacted = finished actor in
       assert (!requests = 1);
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (Agent_session.Session_state.sexp_of_t compacted |> Sexp.to_string_mach)
         |> store_ok
       in
       let known =
         Agent_session.Session_state.authoring_references restored
         |> protocol_ok
         |> Chat_response.Authoring_reference_index.receipts
       in
       assert (List.length known > 1);
       policy := Manual;
       List.iter [ 1; 2 ] ~f:(fun sequence ->
         let id =
           History_entry.Id.create ~namespace:"after-compaction" ~sequence
           |> Result.ok_or_failwith
         in
         A.submit_message
           actor
           ~attachment_id:writer.id
           (Codec.user_text ~id "continue authoring" |> Codec.to_protocol)
         |> protocol_ok
         |> ignore;
         ignore (finished actor : Agent_session.Session_state.t));
       assert (!requests = 3);
       print_endline
         "compaction makes no provider request; manual turns receive one committed, \
          deduplicated pointer and no primer");
  [%expect
    {| compaction makes no provider request; manual turns receive one committed, deduplicated pointer and no primer |}]
;;

let%expect_test
    "actor refresh distinguishes persisted moderator replacement and rejects stale \
     history"
  =
  with_handoff_actor
    ~make_worker:(fun env ready ->
      W.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await ready in
        let plan = materialization env input in
        let before_scope = A.state actor |> protocol_ok in
        let foreign =
          materialization
            env
            { input with session_generation = input.session_generation + 1 }
        in
        (match
           caps.prepare_authoring_input
             foreign
             ~history:input.history
             ~effective:(canonical input.history)
         with
         | Error error -> assert (P.Error.equal_code error.code Conflict)
         | Ok _ -> failwith "another generation supplied guidance");
        assert_same_session_snapshot before_scope (A.state actor |> protocol_ok);
        let appended =
          caps.prepare_authoring_input
            plan
            ~history:input.history
            ~effective:(canonical input.history)
          |> protocol_ok
        in
        let history = input.history @ appended in
        let before_stale = A.state actor |> protocol_ok in
        (match
           caps.prepare_authoring_input
             plan
             ~history:input.history
             ~effective:(canonical input.history)
         with
         | Error error -> assert (P.Error.equal_code error.code Conflict)
         | Ok _ -> failwith "stale input appended duplicate guidance");
        assert_same_session_snapshot before_stale (A.state actor |> protocol_ok);
        let old = List.hd_exn (guidance before_stale) in
        let snapshot = handoff_snapshot 0 in
        let snapshot =
          { snapshot with
            revision = 1
          ; next_change_id = 1
          ; replacements =
              [ Session.Moderator_state.Identity_snapshot.Replacement.
                  { target_id = old.id
                  ; change_id = 0
                  ; script_label = None
                  ; value =
                      Chatml.Chatml_value_codec.import_json old.payload
                      |> Session.Snapshot.of_value
                      |> Result.ok_or_failwith
                  }
              ]
          }
        in
        caps.commit_moderator
          (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
        |> protocol_ok;
        let effective =
          Chat_response.Moderator_manager.effective_entries_of_snapshot snapshot history
          |> Result.ok_or_failwith
        in
        let refreshed =
          caps.prepare_authoring_input plan ~history ~effective |> protocol_ok
        in
        assert (List.length refreshed = 1);
        print_s [%sexp (List.length (guidance (A.state actor |> protocol_ok)) : int)];
        W.Completed
          { final_history = history @ refreshed
          ; moderator_snapshot =
              Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)
          ; runtime_requests = []
          }))
    (fun _ actor _ backend ->
       ignore (finished actor : Agent_session.Session_state.t);
       assert (List.equal String.equal (outcomes backend) [ "completed" ]));
  [%expect {| 2 |}]
;;
