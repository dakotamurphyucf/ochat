open! Core
open Fixtures
module S = Agent_session
module A = S.Session_actor
module P = Agent_protocol
module C = Chat_response.Tool_capability

type t =
  { child : A.t
  ; request : S.Script_tool_calls.preparation
  ; delegation : P.Moderator_execution.delegation
  ; event : Chat_response.Moderation.Event.t
  }

let request t = t.request
let delegation t = t.delegation
let event t = t.event
let check_current t = A.check_delegated_policy_caller t.child t.request

let governance t =
  let open Result.Let_syntax in
  let%bind captured = A.Causal_governance.capture t.child ~request:t.request in
  A.Causal_governance.bind_standalone t.child captured
;;

(* Preserve the parent test's original assertion/cancellation if cleanup also
   fails. A successful test body still requires successful joined cleanup. *)
let protect_cleanup ~finally f =
  match f () with
  | value ->
    Eio.Cancel.protect finally;
    value
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    (try Eio.Cancel.protect finally with
     | _ -> ());
    Exn.raise_with_original_backtrace exn backtrace
;;

let with_request ~env ~parent f =
  let parent_state = A.state parent |> protocol_ok in
  let child_id = P.Id.Session.create () in
  let prepare_state (initial : S.Session_state.t) =
    S.Session_state.create
      ~identity:{ initial.identity with session_id = child_id }
      ~spec:initial.spec
      ~initial_history:[]
  in
  Job_fixtures.with_actor
    ~environment:(env, parent_state.spec.workspace_instance)
    ~prepare_state
    (fun env _sw child writer _backend ->
       let calls = ref 0 in
       let selected = native_registry calls ~raises:false in
       let reference =
         C.find selected ~name:"read_file"
         |> Result.map_error ~f:(fun error -> error.C.message)
         |> Result.ok_or_failwith
         |> C.reference
       in
       let ready, publish = Eio.Promise.create () in
       let exited, exit = Eio.Promise.create () in
       let worker_failure = ref None in
       let original =
         P.Principal.create
           ~id:principal_id
           ~authentication_kind:"local"
           ~scopes:(P.Scope.Set.of_list [ Send_messages ])
           ~attributes:[]
         |> protocol_ok
         |> Operator_authorization.nonexpiring_static
       in
       let worker =
         S.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
           Exn.protect
             ~finally:(fun () -> Eio.Promise.resolve exit ())
             ~f:(fun () ->
               match
                 let caller =
                   capabilities.S.Operation_worker.Capabilities.policy_caller ()
                   |> protocol_ok
                 in
                 Exn.protect
                   ~finally:(fun () -> S.Delegated_policy_caller.close caller)
                   ~f:(fun () ->
                     let invocation_id = P.Id.Invocation.create () in
                     let call_id =
                       History_entry.Id_source.allocate capabilities.id_source
                       |> Result.ok_or_failwith
                     in
                     let provider_call_id = P.Id.Invocation.to_string invocation_id in
                     let call =
                       Openai.Responses_history.create_with_id_exn
                         ~id:call_id
                         (Openai.Responses.Item.Function_call
                            { name = reference.name
                            ; arguments = "{}"
                            ; call_id = provider_call_id
                            ; _type = "function_call"
                            ; id = None
                            ; status = None
                            })
                     in
                     capabilities.commit_entry call |> protocol_ok;
                     let request : S.Script_tool_calls.preparation =
                       { invocation_id
                       ; session_id = input.session_id
                       ; generation = input.session_generation
                       ; owner = Model_call (input.operation.id, call_id)
                       ; caller = Some caller
                       ; selected
                       ; call =
                           { id = provider_call_id
                           ; name = reference.name
                           ; args = `Object []
                           ; kind = Function
                           ; payload_text = "{}"
                           ; meta =
                               `Object
                                 [ "origin", `String "model"
                                 ; ( "operation_id"
                                   , P.Id.Operation.to_json input.operation.id )
                                 ; "call_entry_id", History_entry.Id.jsonaf_of_t call_id
                                 ]
                           }
                       }
                     in
                     A.check_delegated_policy_caller child request |> protocol_ok;
                     let current = A.state child |> protocol_ok in
                     assert (P.Id.Session.equal current.identity.session_id child_id);
                     assert (
                       Option.exists current.active_operation ~f:(fun operation ->
                         P.Id.Operation.equal operation.id input.operation.id));
                     assert (
                       List.exists current.conversation.canonical_history ~f:(fun entry ->
                         P.History.equal_entry (S.History_codec.to_protocol call) entry));
                     (* This direct trusted-host fixture has no Factory reservation.
                        These relationship fields remain DATA; only the actual
                        child Actor's Caller and currentness checks lend custody. *)
                     let delegation : P.Moderator_execution.delegation =
                       { child_session_id = current.identity.session_id
                       ; child_generation = current.identity.generation
                       ; child_invocation_id = invocation_id
                       ; admission_sha256 = C.fingerprint selected
                       }
                     in
                     Eio.Promise.resolve
                       publish
                       { child; request; delegation; event = Pre_tool_call request.call };
                     Eio.Fiber.await_cancel ())
               with
               | value -> value
               | exception (Eio.Cancel.Cancelled _ as exn) ->
                 let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                 Exn.raise_with_original_backtrace exn backtrace
               | exception exn ->
                 let backtrace = Stdlib.Printexc.get_raw_backtrace () in
                 worker_failure := Some (exn, backtrace);
                 Exn.raise_with_original_backtrace exn backtrace))
       in
       A.set_operation_worker child (Some worker) |> protocol_ok;
       let message =
         S.History_codec.user_text ~id:history_id "actual delegated policy child"
         |> S.History_codec.to_protocol
       in
       let submitted =
         A.submit_message_authenticated
           child
           ~actor:original
           ~attachment_id:writer.id
           message
         |> protocol_ok
       in
       let operation = Option.value_exn submitted.operation_id in
       let cleanup () =
         Eio.Cancel.protect (fun () ->
           Eio.Time.with_timeout_exn env#clock 5. (fun () ->
             A.cancel_operation child ~attachment_id:writer.id ~operation_id:operation
             |> protocol_ok
             |> ignore;
             Eio.Promise.await exited;
             ignore (await_idle child)))
       in
       protect_cleanup ~finally:cleanup (fun () ->
         let actual =
           Eio.Fiber.first
             (fun () -> Eio.Promise.await ready)
             (fun () ->
                Eio.Promise.await exited;
                match !worker_failure with
                | Some (exn, backtrace) -> Exn.raise_with_original_backtrace exn backtrace
                | None -> failwith "actual child worker exited before Caller capture")
         in
         (match actual.request.owner with
          | Model_call (actual_operation, _) ->
            assert (P.Id.Operation.equal operation actual_operation)
          | Native_call _ | Moderator_event _ ->
            failwith "actual child producer changed preparation owner");
         check_current actual |> protocol_ok;
         let result = f actual in
         check_current actual |> protocol_ok;
         [%test_eq: int] 0 !calls;
         result))
;;
