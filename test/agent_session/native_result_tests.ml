open Core
open Fixtures
module C = Chat_response.Tool_capability
module I = Agent_protocol.Invocation
module N = Agent_session.Native_tool_invocation
module A = Agent_session.Session_actor

let%expect_test
    "foreground native adapter preserves events and revalidates before effects"
  =
  List.iter [ `Returned; `Raised; `Denied; `Revoked ] ~f:(fun mode ->
    let calls = ref 0 in
    let selected =
      native_registry
        calls
        ~raises:
          (match mode with
           | `Raised -> true
           | _ -> false)
    in
    let current = ref selected in
    let events = ref [] in
    let finished = ref false in
    let outcome_code = ref "unresolved" in
    let adapted = ref 0 in
    with_handoff_actor
      ~make_worker:(fun _env actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
          let actor = Eio.Promise.await actor_ready in
          let call, invocation = publication_call caps () in
          let reference, invocation = native_context selected invocation in
          caps.commit_invocation_call ~invocation call |> protocol_ok;
          let resolved =
            N.run_in_driver
              ~run_native:(fun implementation ~payload ->
                incr adapted;
                (match N.current_scope () with
                 | Active active ->
                   assert (I.equal_context active.context invocation.context)
                 | Unbound | Expired -> failwith "driver lost the native invocation scope");
                Chat_response.Tool_call.run_tool
                  ~kind:Function
                  ~name:reference.name
                  ~payload
                  ~call_id:(Option.value_exn invocation.context.provider_call_id)
                  ~tool_tbl:
                    (Hashtbl.of_alist_exn
                       (module String)
                       [ ( reference.name
                         , fun ~invocation:_ _ -> failwith "stale driver table used" )
                       ])
                  ~runner:implementation.run_with_progress
                  ~on_fork:None
                  ~on_tool_execution:(fun event ->
                    let name =
                      match event with
                      | Started _ -> "started"
                      | Finished { outcome = Returned; _ } -> "returned"
                      | Finished { outcome = Raised; _ } -> "raised"
                      | Finished { outcome = Cancelled; _ } -> "cancelled"
                      | Progress _ -> "progress"
                      | Trace _ -> "trace"
                    in
                    events := name :: !events)
                  ())
              ~capabilities:caps
              ~registry:(fun () -> !current)
              ~reference
              ~invocation
              ~is_halted:(fun () -> false)
              ~authorize:(fun _ _ ->
                match mode with
                | `Returned | `Raised -> Ok ()
                | `Denied ->
                  Error (Agent_protocol.Error.invalid_request "private policy diagnostic")
                | `Revoked ->
                  current
                  := C.select selected ~names:[]
                     |> Result.map_error ~f:(fun error -> error.C.message)
                     |> Result.ok_or_failwith;
                  Ok ())
              ~prepare_output:(function
                | Text text -> Ok (`String text)
                | _ -> assert false)
            |> protocol_ok
          in
          let outcome =
            match resolved.status with
            | Resolved outcome -> outcome
            | _ -> assert false
          in
          (outcome_code
           := match outcome with
              | Complete _ -> "complete"
              | Fail error -> error.code
              | _ -> assert false);
          let output =
            publication_output
              caps
              ~text:(Jsonaf.to_string (I.outcome_to_json outcome))
              ()
          in
          caps.publish_invocation_output ~invocation_id:resolved.context.id output
          |> protocol_ok;
          let state = A.state actor |> protocol_ok in
          finished := true;
          Completed
            { final_history =
                Agent_session.History_codec.all_of_protocol
                  state.conversation.canonical_history
                |> protocol_ok
            ; moderator_snapshot = None
            ; runtime_requests = []
            }))
      (fun _env actor _writer backend ->
         let rec await () =
           let state = A.state actor |> protocol_ok in
           match state.active_operation, !finished with
           | None, true -> state
           | _ ->
             Eio.Fiber.yield ();
             await ()
         in
         let state = await () in
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
         print_s
           [%sexp
             (mode : [ `Returned | `Raised | `Denied | `Revoked ])
           , (!outcome_code : string)
           , (!adapted : int)
           , (!calls : int)
           , (List.rev !events : string list)]));
  [%expect
    {|
    (Returned complete 1 1 (started returned))
    (Raised invocation.handler_failed 1 1 (started raised))
    (Denied invocation.permission_denied 0 0 ())
    (Revoked invocation.stale_binding 0 0 ())
    |}]
;;

let%expect_test "native result contracts are host-bound, disclosed and published once" =
  List.iter
    [ `Opaque
    ; `Complete
    ; `Fail
    ; `Pending
    ; `Malformed
    ; `Redacted
    ; `Invalid_redaction
    ; `Managed
    ]
    ~f:(fun mode ->
      let calls = ref 0 in
      let base = native_registry calls ~raises:false in
      let implementation =
        C.find base ~name:"read_file"
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
        |> C.native_implementation
        |> Option.value_exn
      in
      let raw =
        match mode with
        | `Malformed -> "{broken"
        | `Fail ->
          I.outcome_to_json
            (Fail
               { code = "chatml.parse"
               ; message = "expected expression"
               ; retryable = false
               ; details = `Null
               })
          |> Jsonaf.to_string
        | `Pending ->
          I.outcome_to_json (Pending (Job (Agent_protocol.Id.Job.create ()), `Null))
          |> Jsonaf.to_string
        | `Opaque | `Complete | `Redacted | `Invalid_redaction | `Managed ->
          I.outcome_to_json (Complete (`String "private-value")) |> Jsonaf.to_string
      in
      let implementation =
        { implementation with
          run_with_progress =
            (fun ~invocation payload ->
              ignore
                (implementation.run_with_progress ~invocation payload
                 : Openai.Responses.Tool_output.Output.t);
              Text raw)
        }
      in
      let create result_contracts =
        C.create
          ~result_contracts
          ~owner:"native-result-fixture"
          ~resource_fingerprint:
            (Chatmd_shell_spec.Source_ref.digest "native-result-fixture")
          [ Chatmd_shell_spec.Source_ref.digest "native-result-v1", implementation ]
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      let opaque = create [] in
      let structured = create [ "read_file", Invocation_v1 ] in
      let binding registry =
        C.find registry ~name:"read_file"
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      assert (
        not
          (String.equal
             (C.permission_fingerprint (binding opaque))
             (C.permission_fingerprint (binding structured))));
      let selected =
        match mode with
        | `Opaque -> opaque
        | `Managed ->
          C.extend_managed
            (C.select opaque ~names:[]
             |> fun result ->
             Result.map_error result ~f:(fun error -> error.C.message)
             |> Result.ok_or_failwith)
            ~owner:"native-result-fixture"
            ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "managed fixture")
            [ { descriptor = implementation.info
              ; target = Standalone { script = "source"; entrypoint = "run" }
              ; implementation_revision =
                  Chatmd_shell_spec.Source_ref.digest "managed source"
              ; metadata = Chatmd_shell_spec.Authoring_metadata.empty
              }
            ]
          |> Result.map_error ~f:(fun error -> error.C.message)
          |> Result.ok_or_failwith
        | _ ->
          C.select structured ~names:[ "read_file" ]
          |> Result.map_error ~f:(fun error -> error.C.message)
          |> Result.ok_or_failwith
      in
      let finished = ref false in
      let summary = ref "not executed" in
      with_handoff_actor
        ~make_worker:(fun _env actor_ready ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
            let actor = Eio.Promise.await actor_ready in
            let call, invocation = publication_call caps () in
            let reference, invocation = native_context selected invocation in
            caps.commit_invocation_call ~invocation call |> protocol_ok;
            let resolved =
              N.run
                ~capabilities:caps
                ~registry:(fun () -> selected)
                ~reference
                ~invocation
                ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
                ~authorize:(fun _ _ ->
                  (match mode with
                   | `Managed -> failwith "managed target reached native authorization"
                   | _ -> ());
                  Ok ())
                ~prepare_output:(function
                  | Text text ->
                    Ok
                      (`String
                          (match mode with
                           | `Redacted ->
                             I.outcome_to_json (Complete (`String "redacted"))
                             |> Jsonaf.to_string
                           | `Invalid_redaction -> "[redacted]"
                           | _ -> text))
                  | _ -> failwith "expected text")
              |> protocol_ok
            in
            let outcome =
              match resolved.status with
              | Resolved outcome -> outcome
              | _ -> failwith "native result was not resolved"
            in
            (summary
             := match outcome with
                | Complete (`String text) when String.equal text raw -> "opaque text"
                | Complete (`String text) -> text
                | Fail error -> error.code
                | _ -> failwith "unexpected native result");
            let output =
              publication_output
                caps
                ~text:(Jsonaf.to_string (I.outcome_to_json outcome))
                ()
            in
            caps.publish_invocation_output ~invocation_id:resolved.context.id output
            |> protocol_ok;
            caps.publish_invocation_output ~invocation_id:resolved.context.id output
            |> protocol_ok;
            let state = A.state actor |> protocol_ok in
            finished := true;
            Completed
              { final_history =
                  Agent_session.History_codec.all_of_protocol
                    state.conversation.canonical_history
                  |> protocol_ok
              ; moderator_snapshot = None
              ; runtime_requests = []
              }))
        (fun _env actor _writer backend ->
           let rec await () =
             let state = A.state actor |> protocol_ok in
             match state.active_operation, !finished with
             | None, true -> state
             | _ ->
               Eio.Fiber.yield ();
               await ()
           in
           let state = await () in
           let invocation = List.hd_exn state.invocations in
           let output_id = Option.value_exn invocation.output_entry_id in
           assert (
             Int.equal
               1
               (List.count state.conversation.canonical_history ~f:(fun entry ->
                  Agent_protocol.History.Id.equal entry.id output_id)));
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           print_s
             [%sexp
               (mode
                : [ `Opaque
                  | `Complete
                  | `Fail
                  | `Pending
                  | `Malformed
                  | `Redacted
                  | `Invalid_redaction
                  | `Managed
                  ])
             , (!summary : string)
             , (!calls : int)
             , (List.length state.invocations : int)]));
  [%expect
    {|
    (Opaque "opaque text" 1 1)
    (Complete private-value 1 1)
    (Fail chatml.parse 1 1)
    (Pending invocation.invalid_output 1 1)
    (Malformed invocation.invalid_output 1 1)
    (Redacted redacted 1 1)
    (Invalid_redaction invocation.invalid_output 1 1)
    (Managed invocation.managed_dispatch_required 0 1)
    |}]
;;
