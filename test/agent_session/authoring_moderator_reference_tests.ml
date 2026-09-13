open Core
open Fixtures
module P = Agent_protocol
module I = P.Invocation
module A = Agent_session.Session_actor
module N = Agent_session.Native_tool_invocation
module Scope = Agent_session.Authoring_reference_scope
module C = Chat_response.Tool_capability
module V = Chat_response.Authoring_validation

let request =
  `Object
    [ "version", `Number "1"
    ; "operation", `String "topic"
    ; "task", `String "moderator_tool"
    ; "topic_id", `String "chatml.tasks"
    ; "query", `Null
    ; "features", `Null
    ; "cursor", `Null
    ; "max_tokens", `Null
    ]
;;

let%expect_test
    "moderator helper reads commit with state and survive observation without rereading"
  =
  List.iter [ `Complete; `Replaced; `Fail; `Reject_commit; `Raise ] ~f:(fun mode ->
    let annotated_attempts = ref 0 in
    let saved = ref None in
    let snapshot =
      { (handoff_snapshot 1) with script_source_hash = String.make 64 'a' }
    in
    with_handoff_actor
      ~reject:(fun next ->
        match mode with
        | `Reject_commit
          when List.exists
                 next.Agent_session.Session_transition.state.invocations
                 ~f:(fun invocation -> Option.is_some invocation.I.authoring_reference) ->
          Int.incr annotated_attempts;
          true
        | _ -> false)
      ~make_worker:(fun env actor_ready ->
        let selected =
          C.create
            ~owner:"moderator-reference-fixture"
            ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "no tool effects")
            []
          |> Result.map_error ~f:(fun error -> error.C.message)
          |> Result.ok_or_failwith
        in
        let host =
          V.create_host
            ~runtime_identity:"moderator-reference-fixture"
            ~targets:[ One_off_script; Standalone_tool; Moderator; Generated_chatmd ]
            ~moderator_surface:V.Ordinary
            ~compilation:Chatml_compilation.default_limits
          |> Result.ok_or_failwith
        in
        let services = Agent_session.Authoring_services.create ~env ~host in
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let parent = invocation_fixture () in
          let invocation =
            I.create
              ~observer:
                { script_id = snapshot.script_id
                ; source_sha256 = snapshot.script_source_hash
                }
              { parent.context with
                id = P.Id.Invocation.create ()
              ; parent_invocation = Some parent.context.id
              ; capability_fingerprint = C.fingerprint selected
              }
            |> protocol_ok
          in
          let run () =
            try
              caps.with_moderator_invocation ~invocation (fun ~dispatched ~commit ->
                N.with_dispatched_scope
                  ~execute:caps.with_invocation
                  ~selected
                  ~invocation:dispatched
                  (fun () ->
                     let borrowed = N.borrow () |> protocol_ok in
                     let response =
                       Agent_session.Authoring_services.reference
                         services
                         borrowed
                         request
                       |> Result.map_error ~f:(fun error -> error.I.message)
                       |> Result.ok_or_failwith
                     in
                     assert (
                       not
                         (List.Assoc.mem
                            (match response with
                             | `Object fields -> fields
                             | _ -> assert false)
                            "error"
                            ~equal:String.equal));
                     let before = A.state actor |> protocol_ok in
                     assert (Option.is_none before.moderator);
                     assert (
                       List.for_all before.invocations ~f:(fun invocation ->
                         Option.is_none invocation.I.authoring_reference));
                     let outcome =
                       match mode with
                       | `Complete | `Reject_commit -> I.Complete response
                       | `Replaced -> Complete (`String "disclosed replacement")
                       | `Fail ->
                         Fail
                           { code = "fixture.failed"
                           ; message = "handler failed"
                           ; retryable = false
                           ; details = `Null
                           }
                       | `Raise -> raise Exit
                     in
                     let resolved =
                       I.resolve
                         dispatched
                         ~session_id:input.session_id
                         ~generation:input.session_generation
                         outcome
                       |> protocol_ok
                     in
                     (* Dispatchers retain this same annotated value after commit. *)
                     let scope =
                       Scope.capture ~invocation_id:dispatched.context.id
                       |> Option.value_exn
                     in
                     let resolved = Scope.annotate scope resolved |> protocol_ok in
                     let open Result.Let_syntax in
                     let%map () = commit ~resolved ~snapshot in
                     saved := Some resolved))
            with
            | Exit -> Error (handoff_error "fixture exception")
          in
          let result = ref None in
          caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
            result := Some (run ());
            Ok (I.Complete `Null))
          |> protocol_ok
          |> ignore;
          let result = Option.value_exn !result in
          (match mode with
           | `Complete | `Replaced | `Fail -> result |> protocol_ok
           | `Reject_commit | `Raise -> assert (Result.is_error result));
          (match mode with
           | `Complete ->
             let committed =
               List.find_exn (A.state actor |> protocol_ok).invocations ~f:(fun current ->
                 P.Id.Invocation.equal current.I.context.id invocation.context.id)
             in
             assert (I.equal committed (Option.value_exn !saved));
             assert (Option.is_some committed.authoring_reference);
             caps.with_moderator_observation
               ~invocation_id:committed.context.id
               (fun ~observing ~commit ->
                  assert (
                    Option.is_none (Scope.capture ~invocation_id:observing.context.id));
                  let resolved = I.complete_observation observing |> protocol_ok in
                  assert (
                    Option.equal
                      P.Authoring_reference.equal
                      committed.authoring_reference
                      resolved.authoring_reference);
                  commit ~resolved ~snapshot)
             |> protocol_ok
           | _ -> ());
          let state = A.state actor |> protocol_ok in
          Completed
            { final_history = input.history
            ; runtime_requests = []
            ; moderator_snapshot = state.moderator
            }))
      (fun _env actor _writer backend ->
         let state = await_idle actor in
         let invocation =
           List.find_exn state.invocations ~f:(fun invocation ->
             Option.is_some invocation.I.context.parent_invocation)
         in
         (match mode with
          | `Complete ->
            assert (Option.is_some invocation.authoring_reference);
            assert (Option.is_some state.moderator)
          | `Replaced | `Fail ->
            assert (Option.is_none invocation.authoring_reference);
            assert (Option.is_some state.moderator)
          | `Reject_commit | `Raise ->
            assert (Option.is_none invocation.authoring_reference);
            assert (Option.is_none state.moderator);
            assert (Option.is_none !saved));
         (match mode with
          | `Reject_commit -> assert (!annotated_attempts = 1)
          | _ -> ());
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend)));
  print_endline
    "receipt and checkpoint commit together; disclosure replacement and failure omit \
     receipt; rejected save and exception roll back; observation preserves original read";
  [%expect
    {| receipt and checkpoint commit together; disclosure replacement and failure omit receipt; rejected save and exception roll back; observation preserves original read |}]
;;
