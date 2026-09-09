open Core
open Fixtures

let%test_unit "streamed native and moderator services share pre and post routing" =
  List.iter
    [ `Success
    ; `Custom
    ; `Invalid
    ; `Rewrite_invalid
    ; `Redirect
    ; `Deny
    ; `Pre_reject
    ; `Pre_fail
    ; `Post_fail
    ; `Revoked
    ; `Halt_wait
    ; `Disclosure
    ; `Mixed
    ; `Pre_end
    ; `Pre_reject_end
    ; `Revoked_before
    ; `Kind_mismatch
    ; `Invalid_json
    ; `Redacted
    ; `Publish_rejected
    ; `Call_save_rejected
    ; `Dispatch_rejected
    ; `Observer_failed
    ; `Moderator_call_save_rejected
    ; `Moderator_dispatch_rejected
    ; `Moderator_observer_failed
    ; `Custom_call_save_rejected
    ; `Custom_dispatch_rejected
    ; `Custom_observer_failed
    ]
    ~f:(fun mode ->
      let calls = ref 0
      and admitted = ref 0
      and post_calls = ref 0
      and requests = ref 0 in
      let halted = ref false in
      let custom =
        match mode with
        | `Custom
        | `Custom_call_save_rejected
        | `Custom_dispatch_rejected
        | `Custom_observer_failed -> true
        | _ -> false
      in
      let mixed =
        match mode with
        | `Mixed -> true
        | _ -> false
      in
      let publication_rejected =
        match mode with
        | `Publish_rejected -> true
        | _ -> false
      in
      let redacted =
        match mode with
        | `Redacted -> true
        | _ -> false
      in
      let post_failed =
        match mode with
        | `Post_fail -> true
        | _ -> false
      in
      let call_save_rejected =
        match mode with
        | `Call_save_rejected | `Moderator_call_save_rejected | `Custom_call_save_rejected
          -> true
        | _ -> false
      in
      let dispatch_rejected =
        match mode with
        | `Dispatch_rejected | `Moderator_dispatch_rejected | `Custom_dispatch_rejected ->
          true
        | _ -> false
      in
      let observer_failed =
        match mode with
        | `Observer_failed | `Moderator_observer_failed | `Custom_observer_failed -> true
        | _ -> false
      in
      let moderator_target =
        match mode with
        | `Moderator_call_save_rejected
        | `Moderator_dispatch_rejected
        | `Moderator_observer_failed -> true
        | _ -> false
      in
      let original_name =
        match mode with
        | `Redirect -> "counter"
        | _ -> if moderator_target then "counter" else "read_file"
      in
      let before_execution_failure =
        call_save_rejected || dispatch_rejected || observer_failed
      in
      let registry = ref (native_registry ~custom calls ~raises:false) in
      with_handoff_actor
        ~reject:(fun next ->
          List.exists
            next.Agent_session.Session_transition.state.invocations
            ~f:(fun invocation ->
              (publication_rejected && Option.is_some invocation.output_entry_id)
              ||
              match invocation.status with
              | Admitted -> call_save_rejected
              | Dispatching -> dispatch_rejected
              | Resolved _ | Published _ -> false))
        ~make_worker:(fun env actor_ready ->
          let pre =
            match mode with
            | `Invalid | `Invalid_json | `Kind_mismatch ->
              "Task.fail(\"invalid input reached pre hook\")"
            | `Revoked_before ->
              "Task.bind(Tool.call(\"revoke\", `Null), fun ignored -> Task.pure(state))"
            | `Rewrite_invalid ->
              "Task.bind(Tool.rewrite_args(`Null), fun ignored -> Task.pure(state))"
            | `Redirect ->
              "Task.bind(Tool.redirect(\"read_file\", `Object([])), fun ignored -> \
               Task.pure(state))"
            | `Pre_reject ->
              "Task.bind(Tool.reject(\"private rejection\"), fun ignored -> \
               Task.pure(state))"
            | `Pre_reject_end ->
              "Task.bind(Tool.reject(\"private rejection\"), fun ignored -> \
               Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
               Task.pure(state)))"
            | `Pre_fail -> "Task.fail(\"private pre failure\")"
            | `Pre_end ->
              "Task.bind(Runtime.end_session(\"done\"), fun ignored -> Task.pure(state))"
            | _ -> "Task.pure(state)"
          in
          let events =
            "| `Pre_tool_call(c) -> "
            ^ pre
            ^ " | `Post_tool_response(r) -> Task.bind(Tool.call(\"observe\", `Null), fun \
               ignored -> "
            ^ (if post_failed
               then "Task.fail(\"private post failure\")"
               else "Task.pure(state)")
            ^ ")"
            ^ (if observer_failed
               then
                 " | `Item_appended(item) -> (match Json.get_field(item.value, \"type\") \
                  with | `Some(`String(\"function_call\")) -> Task.fail(\"private \
                  observer failure\") | `Some(`String(\"custom_tool_call\")) -> \
                  Task.fail(\"private observer failure\") | _ -> Task.pure(state))"
               else "")
            ^ " | _ -> Task.pure(state)"
          in
          let moderator_capabilities =
            { Chat_response.Moderation.Capabilities.default with
              on_tool_call =
                (fun ~name ~args:_ ->
                  if String.equal name "observe"
                  then Int.incr post_calls
                  else (
                    assert (String.equal name "revoke");
                    registry
                    := Chat_response.Tool_capability.select !registry ~names:[]
                       |> Result.map_error ~f:(fun error ->
                         error.Chat_response.Tool_capability.message)
                       |> Result.ok_or_failwith);
                  Ok (Tool_ok `Null))
            }
          in
          let manager, _, definition =
            handoff_definition ~events ~moderator_capabilities env
          in
          Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            let response_dir =
              Eio.Path.(
                Eio.Stdenv.fs env
                / state.spec.workspace_instance.canonical_root.native_path
                / "response")
            in
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
            let post_stream ~sw:_ ~inputs =
              Int.incr requests;
              if !requests > 1
              then (
                assert (
                  List.exists inputs ~f:(function
                    | Openai.Responses.Item.Function_call_output _
                    | Custom_tool_call_output _ -> true
                    | _ -> false));
                Stdlib.Seq.empty)
              else
                let open Openai.Responses.Response_stream in
                let call ~custom ~name ~payload ~index =
                  let item_id = "native-item-" ^ Int.to_string index in
                  let call_id = "native-call-" ^ Int.to_string index in
                  [ Output_item_added
                      { item =
                          (if custom
                           then
                             Custom_function
                               { name
                               ; input = ""
                               ; call_id
                               ; _type = "custom_tool_call"
                               ; id = Some item_id
                               }
                           else
                             Function_call
                               { name
                               ; arguments = ""
                               ; call_id
                               ; _type = "function_call"
                               ; id = Some item_id
                               ; status = None
                               })
                      ; output_index = index
                      ; type_ = "response.output_item.added"
                      }
                  ; (if custom
                     then
                       Custom_tool_call_input_done
                         { input = payload
                         ; item_id
                         ; output_index = index
                         ; type_ = "response.custom_tool_call_input.done"
                         }
                     else
                       Function_call_arguments_done
                         { arguments = payload
                         ; item_id
                         ; output_index = index
                         ; type_ = "response.function_call_arguments.done"
                         })
                  ]
                in
                let initial =
                  call
                    ~custom:
                      (match mode with
                       | `Kind_mismatch -> true
                       | _ -> custom)
                    ~name:original_name
                    ~payload:
                      (match mode with
                       | `Invalid -> "null"
                       | `Invalid_json -> "{"
                       | _ -> "{}")
                    ~index:0
                in
                Stdlib.List.to_seq
                  (initial
                   @
                   if mixed
                   then call ~custom:false ~name:"counter" ~payload:"null" ~index:1
                   else [])
            in
            let dispatch_tool ~input ~capabilities =
              let native =
                Agent_session.Native_tool_dispatch.create
                  ~input
                  ~capabilities
                  ~registry:(fun () -> !registry)
                  ~now:Agent_protocol.Timestamp.now
                  ~is_halted:(fun () ->
                    !halted
                    || Chat_response.Moderator_manager.is_halted manager
                       |> Result.ok_or_failwith)
                  ~admit:(fun _ _ ->
                    Int.incr admitted;
                    Eio.Fiber.yield ();
                    (match mode with
                     | `Revoked ->
                       registry
                       := Chat_response.Tool_capability.select !registry ~names:[]
                          |> Result.map_error ~f:(fun error ->
                            error.Chat_response.Tool_capability.message)
                          |> Result.ok_or_failwith
                     | `Halt_wait -> halted := true
                     | _ -> ());
                    Ok ())
                  ~prepare_output:(fun _ ->
                    match mode with
                    | `Disclosure -> Error (handoff_error "private disclosure")
                    | _ -> Ok (`String "disclosed"))
              in
              let moderator =
                Agent_session.Moderator_tool_dispatch.create
                  ~definition
                  ~manager
                  ~input
                  ~capabilities
                  ~available_tools:[]
                  ~session_meta:`Null
                  ~now:Agent_protocol.Timestamp.now
                  ~validate_work:(fun _ -> Error "pending disabled")
                  ~admit:(fun _ -> Ok ())
                  ~prepare_outcome:(fun _ -> Ok ())
                  ()
              in
              Chat_response.In_memory_stream.Tool_dispatch.chain [ moderator; native ]
            in
            let tool_tbl = String.Table.create () in
            Hashtbl.set tool_tbl ~key:"read_file" ~data:(fun ~invocation:_ _ ->
              failwith "native adapter fell through");
            let worker =
              Agent_session.Turn_worker.create
                ~dispatch_tool
                { env
                ; response_dir
                ; tools = []
                ; tool_tbl
                ; temperature = None
                ; max_output_tokens = None
                ; reasoning = None
                ; moderator =
                    Some
                      { manager
                      ; session_id = Agent_protocol.Id.Session.to_string input.session_id
                      ; session_meta = `Null
                      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
                      ; event_handlers = None
                      }
                ; permission_profile =
                    permission_policy
                      ~tool_default:
                        (match mode with
                         | `Deny -> Deny
                         | _ -> Allow)
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
                ; redact_tool_payload =
                    (fun ~name:_ value -> if redacted then "\"hidden\"" else value)
                }
            in
            Agent_session.Operation_worker.run worker ~sw ~input caps))
        (fun _env actor _writer backend ->
           let rec finished () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if Option.is_none state.active_operation
             then state
             else (
               Eio.Fiber.yield ();
               finished ())
           in
           let state = finished () in
           let expected_count =
             if call_save_rejected then 0 else if mixed then 2 else 1
           in
           if List.length state.invocations <> expected_count
           then
             failwithf
               "invocation count: expected %d, got %d (save=%b dispatch=%b observer=%b \
                moderator=%b requests=%d)"
               expected_count
               (List.length state.invocations)
               call_save_rejected
               dispatch_rejected
               observer_failed
               moderator_target
               !requests
               ();
           if call_save_rejected
           then (
             assert (!calls = 0 && !admitted = 0 && !post_calls = 0 && !requests = 1);
             assert (List.length state.conversation.canonical_history = 1);
             assert_same_session_snapshot
               state
               (Agent_session.Memory_backend.state backend))
           else (
             let native =
               List.find_exn state.invocations ~f:(fun invocation ->
                 String.equal
                   invocation.context.tool_name
                   (if moderator_target then "counter" else "read_file"))
             in
             let expected =
               match mode with
               | `Success
               | `Custom
               | `Redirect
               | `Post_fail
               | `Mixed
               | `Redacted
               | `Publish_rejected -> None
               | `Invalid | `Rewrite_invalid | `Invalid_json | `Kind_mismatch ->
                 Some "invocation.invalid_input"
               | `Deny -> Some "invocation.permission_denied"
               | `Pre_reject | `Pre_reject_end -> Some "invocation.pre_tool_rejected"
               | `Pre_fail -> Some "invocation.pre_tool_failed"
               | `Revoked | `Revoked_before -> Some "invocation.stale_binding"
               | `Halt_wait | `Pre_end -> Some "invocation.session_ended"
               | `Disclosure -> Some "invocation.disclosure_rejected"
               | `Call_save_rejected
               | `Moderator_call_save_rejected
               | `Custom_call_save_rejected -> assert false
               | `Dispatch_rejected
               | `Observer_failed
               | `Moderator_dispatch_rejected
               | `Moderator_observer_failed
               | `Custom_dispatch_rejected
               | `Custom_observer_failed -> Some "interrupted"
             in
             (match native.status, expected with
              | Published (Complete (`String "disclosed")), None -> ()
              | Resolved (Complete (`String "disclosed")), None ->
                assert publication_rejected
              | Published (Fail error), Some code -> assert (String.equal error.code code)
              | Published (Cancelled _), Some "interrupted" -> ()
              | _ -> assert false);
             let executed =
               match mode with
               | `Disclosure -> true
               | _ -> Option.is_none expected
             in
             assert (!calls = if executed then 1 else 0);
             assert (
               !admitted
               =
               if
                 before_execution_failure
                 ||
                 match mode with
                 | `Invalid
                 | `Invalid_json
                 | `Kind_mismatch
                 | `Revoked_before
                 | `Rewrite_invalid
                 | `Pre_reject
                 | `Pre_reject_end
                 | `Pre_fail
                 | `Pre_end -> true
                 | _ -> false
               then 0
               else 1);
             assert (
               !post_calls
               =
               if
                 before_execution_failure
                 ||
                 match mode with
                 | `Pre_end | `Pre_reject_end | `Publish_rejected -> true
                 | _ -> false
               then 0
               else if mixed
               then 2
               else 1);
             assert (
               !requests
               =
               if
                 before_execution_failure
                 ||
                 match mode with
                 | `Post_fail | `Publish_rejected | `Pre_end | `Pre_reject_end -> true
                 | _ -> false
               then 1
               else 2);
             assert (
               List.length state.conversation.canonical_history
               = if mixed then 5 else if publication_rejected then 2 else 3);
             let routing = Option.value_exn native.routing in
             assert (String.equal routing.original_name original_name);
             if redacted
             then (
               let canonical = Option.value_exn routing.canonical_payload in
               assert (
                 String.equal
                   canonical.sha256
                   (Chatmd_shell_spec.Source_ref.digest "\"hidden\""));
               assert (
                 String.equal
                   routing.final_payload.sha256
                   (Chatmd_shell_spec.Source_ref.digest "{}")));
             let failures =
               Agent_session.Memory_backend.events_after backend 0L
               |> protocol_ok
               |> List.count ~f:(fun event ->
                 Agent_protocol.Event.Durable.equal_kind event.kind Operation_failed)
             in
             assert (
               failures
               =
               if before_execution_failure || post_failed || publication_rejected
               then 1
               else 0);
             assert (Bool.equal (Option.is_some state.failure) publication_rejected);
             assert_same_session_snapshot
               state
               (Agent_session.Memory_backend.state backend))))
;;

let%test_unit
    "streamed moderator tools use actor publication and preserve post-hook failures"
  =
  List.iter
    [ `Success
    ; `Deny
    ; `Disclosure
    ; `Post_fail
    ; `Publish_rejected
    ; `Invalid_json
    ; `Redirect
    ; `Redirect_bad
    ; `Revoked
    ; `End_session
    ; `Unhandled
    ; `Duplicate
    ; `Wrong_id
    ; `Invalid_output
    ; `Forged_error
    ; `Result_rejected
    ; `Pre_reject
    ; `Pre_reject_end
    ; `Pre_reject_post_fail
    ; `Custom_success
    ; `Pre_reject_custom
    ; `Original_invalid
    ; `Custom_invalid
    ; `Rewrite_bad
    ; `Rewrite_ok
    ; `Redacted_input
    ; `Pre_fail
    ; `Pre_host_exception
    ; `Pre_invalid_action
    ; `Pre_custom_fail
    ; `Original_array_limit
    ; `Original_depth_limit
    ; `Original_bytes_limit
    ; `Custom_bytes_limit
    ; `Rewrite_limit
    ; `Redirect_limit
    ; `Pre_end_multi
    ; `Pre_reject_end_multi
    ; `End_session_multi
    ]
    ~f:(fun mode ->
      let request_count = ref 0 in
      let admitted = ref 0 in
      let host_calls = ref 0 in
      let live_snapshot = ref None in
      let native_calls = ref 0 in
      let multi =
        List.mem
          [ `Pre_end_multi; `Pre_reject_end_multi; `End_session_multi ]
          mode
          ~equal:Poly.equal
      in
      let pre_end = Poly.equal mode `Pre_end_multi in
      let implementation_end =
        Poly.equal mode `End_session || Poly.equal mode `End_session_multi
      in
      let pre_failed =
        List.mem
          [ `Pre_fail; `Pre_host_exception; `Pre_invalid_action; `Pre_custom_fail ]
          mode
          ~equal:Poly.equal
      in
      let redirected =
        List.mem [ `Redirect; `Redirect_bad; `Redirect_limit ] mode ~equal:Poly.equal
      in
      let rewritten =
        List.mem [ `Rewrite_bad; `Rewrite_ok; `Rewrite_limit ] mode ~equal:Poly.equal
      in
      let original_limit =
        List.mem
          [ `Original_array_limit
          ; `Original_depth_limit
          ; `Original_bytes_limit
          ; `Custom_bytes_limit
          ]
          mode
          ~equal:Poly.equal
      in
      let final_limit =
        Poly.equal mode `Rewrite_limit || Poly.equal mode `Redirect_limit
      in
      let over_array = `Array (List.init 257 ~f:(fun _ -> `Null)) in
      let array_expression =
        "`Array([" ^ String.concat ~sep:"," (List.init 257 ~f:(fun _ -> "`Null")) ^ "])"
      in
      let original_payload =
        match mode with
        | `Invalid_json -> "[broken"
        | `Original_invalid -> "\"wrong\""
        | `Original_array_limit -> Jsonaf.to_string over_array
        | `Original_depth_limit ->
          Jsonaf.to_string
            (List.fold (List.init 17 ~f:Fn.id) ~init:`Null ~f:(fun value _ ->
               `Array [ value ]))
        | `Original_bytes_limit ->
          Jsonaf.to_string (`String (String.make (256 * 1024) 'x'))
        | `Custom_bytes_limit -> String.make (256 * 1024) 'x'
        | _ -> if redirected then "{}" else "null"
      in
      let invalid_original =
        original_limit
        || List.mem
             [ `Invalid_json; `Original_invalid; `Custom_invalid ]
             mode
             ~equal:Poly.equal
      in
      let pre_rejected =
        List.mem
          [ `Pre_reject
          ; `Pre_reject_end
          ; `Pre_reject_post_fail
          ; `Pre_reject_custom
          ; `Pre_reject_end_multi
          ]
          mode
          ~equal:Poly.equal
      in
      let ends_session = implementation_end || Poly.equal mode `Pre_reject_end || multi in
      let custom =
        Poly.equal mode `Custom_success
        || Poly.equal mode `Pre_reject_custom
        || Poly.equal mode `Custom_invalid
        || Poly.equal mode `Pre_custom_fail
        || Poly.equal mode `Custom_bytes_limit
      in
      let post_fails =
        Poly.equal mode `Post_fail || Poly.equal mode `Pre_reject_post_fail
      in
      with_handoff_actor
        ~reject:(fun next ->
          List.exists
            next.Agent_session.Session_transition.state.invocations
            ~f:(fun inv ->
              (Poly.equal mode `Publish_rejected && Option.is_some inv.output_entry_id)
              || (Poly.equal mode `Result_rejected
                  &&
                  match inv.status with
                  | Resolved (Complete _) -> true
                  | _ -> false)))
        ~make_worker:(fun env actor_ready ->
          let events =
            if pre_end
            then
              "| `Pre_tool_call(c) -> Task.bind(Runtime.end_session(\"done\"), fun \
               ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else if pre_failed
            then
              "| `Pre_tool_call(c) -> let ignored = state[0] <- 99 in "
              ^ "Task.bind(Runtime.emit(`String(\"uncommitted\")), fun ignored -> "
              ^ "Task.bind(Turn.prepend_system(\"uncommitted\"), fun ignored -> "
              ^ (if Poly.equal mode `Pre_host_exception
                 then
                   "Task.bind(Tool.call(\"explode\", `Null), fun ignored -> \
                    Task.pure(state))"
                 else if Poly.equal mode `Pre_invalid_action
                 then
                   "Task.bind(Tool.reject(\"rejected\"), fun ignored -> \
                    Task.bind(Tool.redirect(\"counter\", `Null), fun ignored -> \
                    Task.pure(state)))"
                 else "Task.fail(\"private diagnostic\")")
              ^ ")) | _ -> Task.pure(state)"
            else if invalid_original
            then
              "| `Pre_tool_call(c) -> Task.fail(\"invalid input reached pre handler\") | \
               _ -> Task.pure(state)"
            else if rewritten
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.rewrite_args("
              ^ (if final_limit
                 then array_expression
                 else if Poly.equal mode `Rewrite_bad
                 then "`String(\"wrong\")"
                 else "`Null")
              ^ "), fun ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else if pre_rejected
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.reject(\"private diagnostic\"), fun \
               ignored -> "
              ^ (if
                   Poly.equal mode `Pre_reject_end
                   || Poly.equal mode `Pre_reject_end_multi
                 then
                   "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                    Task.pure(state)))"
                 else "Task.pure(state))")
              ^ (if post_fails
                 then
                   " | `Post_tool_response(r) -> let ignored = state[0] <- 99 in \
                    Task.fail(\"post hook failed\")"
                 else "")
              ^ " | _ -> Task.pure(state)"
            else if Poly.equal mode `Post_fail
            then
              "| `Post_tool_response(r) -> let ignored = state[0] <- 99 in \
               Task.fail(\"post hook failed\") | _ -> Task.pure(state)"
            else if redirected
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.redirect(\"counter\", "
              ^ (if final_limit
                 then array_expression
                 else if Poly.equal mode `Redirect_bad
                 then "`String(\"wrong\")"
                 else "`Null")
              ^ "), fun ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else "| _ -> Task.pure(state)"
          in
          let manager, _, definition =
            handoff_definition
              ~events
              ~script_limits:
                (if original_limit || final_limit
                 then {|max_array_items="256" max_depth="32" max_value="256KiB"|}
                 else "")
              ~moderator_capabilities:
                { Chat_response.Moderation.Capabilities.default with
                  on_tool_call =
                    (fun ~name:_ ~args:_ ->
                      Int.incr host_calls;
                      failwith "private diagnostic from host")
                }
              ~schema:
                (if original_limit || final_limit
                 then "true"
                 else if
                   redirected
                   || rewritten
                   || invalid_original
                   || Poly.equal mode `Invalid_output
                 then "{\"type\":\"null\"}"
                 else "true")
              ~resolve:
                (match mode with
                 | `Unhandled -> "Task.pure(())"
                 | `Duplicate ->
                   "Task.bind(Invocation.resolve(p.context.invocation_id, \
                    `Complete(`Null)), fun ignored -> \
                    Invocation.resolve(p.context.invocation_id, `Complete(`Null)))"
                 | `Wrong_id -> "Invocation.resolve(\"other\", `Complete(`Null))"
                 | `Invalid_output ->
                   "Invocation.resolve(p.context.invocation_id, \
                    `Complete(`String(\"wrong\")))"
                 | `Forged_error ->
                   "Task.fail(\"invocation.unhandled: private diagnostic\")"
                 | _ -> "Invocation.resolve(p.context.invocation_id, `Complete(`Null))")
              ~finish:
                (if implementation_end
                 then
                   "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                    Task.pure(state))"
                 else "Task.pure(state)")
              env
          in
          live_snapshot
          := Some
               (fun () ->
                 Chat_response.Moderator_manager.identity_snapshot manager
                 |> Result.ok_or_failwith);
          Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            let response_dir =
              Eio.Path.(
                Eio.Stdenv.fs env
                / state.spec.workspace_instance.canonical_root.native_path
                / "response")
            in
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
            let post_stream ~sw:_ ~inputs =
              Int.incr request_count;
              if !request_count = 1
              then (
                let initial =
                  Stdlib.List.to_seq
                    Openai.Responses.Response_stream.
                      [ Output_item_added
                          { item =
                              (if custom
                               then
                                 Custom_function
                                   { name = "counter"
                                   ; input = ""
                                   ; call_id = "counter-call"
                                   ; _type = "custom_tool_call"
                                   ; id = Some "counter-item"
                                   }
                               else
                                 Function_call
                                   { name = (if redirected then "alias" else "counter")
                                   ; arguments = ""
                                   ; call_id = "counter-call"
                                   ; _type = "function_call"
                                   ; id = Some "counter-item"
                                   ; status = None
                                   })
                          ; output_index = 0
                          ; type_ = "response.output_item.added"
                          }
                      ; (if custom
                         then
                           Custom_tool_call_input_done
                             { input = original_payload
                             ; item_id = "counter-item"
                             ; output_index = 0
                             ; type_ = "response.custom_tool_call_input.done"
                             }
                         else
                           Function_call_arguments_done
                             { arguments = original_payload
                             ; item_id = "counter-item"
                             ; output_index = 0
                             ; type_ = "response.function_call_arguments.done"
                             })
                      ]
                in
                if not multi
                then initial
                else
                  Stdlib.Seq.append initial (fun () ->
                    (* Force later provider items to arrive after the first handler
                     has halted, including the asynchronous implementation path. *)
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                      let rec halted () =
                        if
                          Chat_response.Moderator_manager.is_halted manager
                          |> Result.ok_or_failwith
                        then ()
                        else (
                          Eio.Fiber.yield ();
                          halted ())
                      in
                      halted ());
                    let open Openai.Responses.Response_stream in
                    let message =
                      match worker_output_item with
                      | Openai.Responses.Item.Output_message message -> message
                      | _ -> assert false
                    in
                    Stdlib.List.to_seq
                      [ Output_item_added
                          { item =
                              Custom_function
                                { name = "counter"
                                ; input = ""
                                ; call_id = "later-custom"
                                ; _type = "custom_tool_call"
                                ; id = Some "later-custom-item"
                                }
                          ; output_index = 1
                          ; type_ = "response.output_item.added"
                          }
                      ; Custom_tool_call_input_done
                          { input = "null"
                          ; item_id = "later-custom-item"
                          ; output_index = 1
                          ; type_ = "response.custom_tool_call_input.done"
                          }
                      ; Output_item_added
                          { item =
                              Function_call
                                { name = "native"
                                ; arguments = ""
                                ; call_id = "later-native"
                                ; _type = "function_call"
                                ; id = Some "later-native-item"
                                ; status = None
                                }
                          ; output_index = 2
                          ; type_ = "response.output_item.added"
                          }
                      ; Function_call_arguments_done
                          { arguments = "null"
                          ; item_id = "later-native-item"
                          ; output_index = 2
                          ; type_ = "response.function_call_arguments.done"
                          }
                      ; Output_item_done
                          { item = Output_message message
                          ; output_index = 3
                          ; type_ = "response.output_item.done"
                          }
                      ]
                      ()))
              else (
                assert (
                  List.exists inputs ~f:(function
                    | Openai.Responses.Item.Function_call_output _ -> not custom
                    | Custom_tool_call_output _ -> custom
                    | _ -> false));
                Seq.empty)
            in
            let dispatch_tool ~input ~capabilities =
              Agent_session.Moderator_tool_dispatch.create
                ~definition
                ~manager
                ~input
                ~capabilities
                ~available_tools:[]
                ~session_meta:`Null
                ~now:Agent_protocol.Timestamp.now
                ~validate_work:(fun _ -> Error "no pending work")
                ~admit:(fun request ->
                  Int.incr admitted;
                  assert (String.equal request.name "counter");
                  if redirected
                  then (
                    assert (String.equal request.original_name "alias");
                    assert (String.equal request.original_payload "{}");
                    assert (String.equal request.payload "null"));
                  if Poly.equal mode `Revoked
                  then Error "capability was revoked"
                  else Ok ())
                ~prepare_outcome:(fun _ ->
                  if Poly.equal mode `Disclosure then Error "blocked" else Ok ())
                ()
            in
            let worker =
              let tool_tbl = String.Table.create () in
              Hashtbl.set tool_tbl ~key:"native" ~data:(fun ~invocation:_ _ ->
                Int.incr native_calls;
                Openai.Responses.Tool_output.Output.Text "unexpected execution");
              Agent_session.Turn_worker.create
                ~dispatch_tool
                { env
                ; response_dir
                ; tools = []
                ; tool_tbl
                ; temperature = None
                ; max_output_tokens = None
                ; reasoning = None
                ; moderator =
                    Some
                      { manager
                      ; session_id = Agent_protocol.Id.Session.to_string input.session_id
                      ; session_meta = `Null
                      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
                      ; event_handlers = None
                      }
                ; permission_profile =
                    permission_policy
                      ~tool_default:(if Poly.equal mode `Deny then Deny else Allow)
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
                ; redact_tool_payload =
                    (fun ~name:_ value ->
                      if Poly.equal mode `Redacted_input then "\"redacted\"" else value)
                }
            in
            Agent_session.Operation_worker.run worker ~sw ~input caps))
        (fun _env actor writer backend ->
           let rec finished () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if Option.is_some state.active_operation
             then (
               Eio.Fiber.yield ();
               finished ())
             else state
           in
           let state = finished () in
           assert (!native_calls = 0);
           if Poly.equal mode `Publish_rejected
           then (
             assert (Option.is_some state.failure);
             assert (
               match state.lifecycle.observed with
               | Failed _ -> true
               | _ -> false);
             let entry =
               let id =
                 History_entry.Id.create ~namespace:"rejected-next-turn" ~sequence:0
                 |> Result.ok_or_failwith
               in
               Agent_session.History_codec.user_text ~id "must not start"
               |> Agent_session.History_codec.to_protocol
             in
             assert (
               Result.is_error
                 (Agent_session.Session_actor.submit_message
                    actor
                    ~attachment_id:writer.id
                    entry));
             let after = Agent_session.Session_actor.state actor |> protocol_ok in
             assert (Poly.equal state after));
           assert (List.length state.invocations = if multi then 2 else 1);
           let invocation =
             List.find_exn state.invocations ~f:(fun inv ->
               Option.equal
                 String.equal
                 inv.context.provider_call_id
                 (Some "counter-call"))
           in
           if multi
           then (
             let later =
               List.find_exn state.invocations ~f:(fun inv ->
                 Option.equal
                   String.equal
                   inv.context.provider_call_id
                   (Some "later-custom"))
             in
             assert (Option.is_some later.output_entry_id);
             assert (
               Poly.equal
                 (Option.value_exn later.routing).preparation
                 Agent_protocol.Invocation.Session_ended);
             match later.status with
             | Published (Fail error) ->
               assert (String.equal error.code "invocation.session_ended")
             | _ -> assert false);
           let routing = Option.value_exn invocation.routing in
           let final_payload =
             if final_limit
             then Jsonaf.to_string over_array
             else if Poly.equal mode `Redirect_bad || Poly.equal mode `Rewrite_bad
             then "\"wrong\""
             else if redirected || rewritten
             then "null"
             else original_payload
           in
           let fingerprint payload =
             Agent_protocol.Invocation.
               { sha256 = Chatmd_shell_spec.Source_ref.digest payload
               ; byte_length = String.length payload
               }
           in
           assert (
             String.equal
               routing.original_name
               (if redirected then "alias" else "counter"));
           assert (
             Poly.equal
               routing.kind
               (if custom then Agent_protocol.Invocation.Custom else Function));
           assert (Poly.equal routing.original_payload (fingerprint original_payload));
           assert (Poly.equal routing.final_payload (fingerprint final_payload));
           assert (
             Poly.equal
               routing.canonical_payload
               (Some
                  (fingerprint
                     (if Poly.equal mode `Redacted_input
                      then "\"redacted\""
                      else final_payload))));
           let expected_preparation =
             if pre_end
             then Agent_protocol.Invocation.Session_ended
             else if invalid_original
             then Agent_protocol.Invocation.Invalid_input
             else if pre_rejected
             then Pre_tool_rejected
             else if pre_failed
             then Pre_tool_failed
             else Passed
           in
           if not (Poly.equal routing.preparation expected_preparation)
           then
             failwithf
               "unexpected preparation for input %s/%d: %s, expected %s"
               (Chatmd_shell_spec.Source_ref.digest original_payload)
               (String.length original_payload)
               (Sexp.to_string
                  (Agent_protocol.Invocation.sexp_of_preparation routing.preparation))
               (Sexp.to_string
                  (Agent_protocol.Invocation.sexp_of_preparation expected_preparation))
               ();
           let expected_count =
             if
               Poly.equal mode `Success
               || Poly.equal mode `Custom_success
               || Poly.equal mode `Rewrite_ok
               || Poly.equal mode `Redacted_input
               || Poly.equal mode `Post_fail
               || Poly.equal mode `Publish_rejected
               || Poly.equal mode `Redirect
               || Poly.equal mode `End_session
               || Poly.equal mode `End_session_multi
             then 1
             else 0
           in
           let saved_snapshot =
             match state.moderator with
             | Some (`Object [ ("identity_snapshot_sexp", `String encoded) ]) ->
               Session.Moderator_state.Identity_snapshot.t_of_sexp
                 (Sexp.of_string encoded)
             | _ -> assert false
           in
           let live = (Option.value_exn !live_snapshot) () in
           assert (Poly.equal live.current_state saved_snapshot.current_state);
           assert (!host_calls = if Poly.equal mode `Pre_host_exception then 1 else 0);
           if pre_failed
           then (
             assert (List.is_empty live.prepended_items);
             assert (List.is_empty live.queued_internal_events));
           assert (
             Poly.equal
               saved_snapshot.current_state
               (Session.Snapshot.Array [ Int expected_count ]));
           assert (
             Bool.equal
               (Option.is_some invocation.output_entry_id)
               (not (Poly.equal mode `Publish_rejected)));
           assert (
             match invocation.status with
             | Published (Complete `Null) ->
               Poly.equal mode `Success
               || Poly.equal mode `Custom_success
               || Poly.equal mode `Rewrite_ok
               || Poly.equal mode `Redacted_input
               || Poly.equal mode `Post_fail
               || Poly.equal mode `Redirect
               || Poly.equal mode `End_session
               || Poly.equal mode `End_session_multi
             | Published (Fail error) ->
               let expected =
                 match mode with
                 | `Deny | `Revoked -> "invocation.permission_denied"
                 | `Disclosure -> "invocation.disclosure_rejected"
                 | `Invalid_json
                 | `Redirect_bad
                 | `Original_invalid
                 | `Custom_invalid
                 | `Original_array_limit
                 | `Original_depth_limit
                 | `Original_bytes_limit
                 | `Custom_bytes_limit
                 | `Rewrite_limit
                 | `Redirect_limit
                 | `Rewrite_bad -> "invocation.invalid_input"
                 | `Unhandled -> "invocation.unhandled"
                 | `Duplicate -> "invocation.duplicate_resolution"
                 | `Wrong_id -> "invocation.wrong_id"
                 | `Invalid_output -> "invocation.invalid_output"
                 | `Forged_error -> "invocation.handler_failed"
                 | `Result_rejected -> "invocation.commit_failed"
                 | `Pre_fail
                 | `Pre_host_exception
                 | `Pre_invalid_action
                 | `Pre_custom_fail -> "invocation.pre_tool_failed"
                 | `Pre_reject
                 | `Pre_reject_end
                 | `Pre_reject_post_fail
                 | `Pre_reject_custom -> "invocation.pre_tool_rejected"
                 | `Pre_reject_end_multi -> "invocation.pre_tool_rejected"
                 | `Pre_end_multi -> "invocation.session_ended"
                 | _ -> assert false
               in
               assert (not error.retryable);
               assert (Poly.equal error.details `Null);
               assert (
                 not (String.is_substring error.message ~substring:"private diagnostic"));
               String.equal error.code expected
             | Resolved (Complete `Null) -> Poly.equal mode `Publish_rejected
             | _ -> false);
           assert (
             List.length state.conversation.canonical_history
             = if multi then 8 else if Poly.equal mode `Publish_rejected then 2 else 3);
           let failed = post_fails || Poly.equal mode `Publish_rejected in
           assert (!request_count = if failed || ends_session then 1 else 2);
           if ends_session then assert saved_snapshot.halted;
           assert (
             !admitted
             =
             if
               pre_end
               || pre_rejected
               || pre_failed
               || invalid_original
               || final_limit
               || Poly.equal mode `Rewrite_bad
               || Poly.equal mode `Redirect_bad
             then 0
             else 1);
           let events =
             Agent_session.Memory_backend.events_after backend 0L |> protocol_ok
           in
           let failures =
             List.filter events ~f:(fun e ->
               Agent_protocol.Event.Durable.equal_kind e.kind Operation_failed)
           in
           assert (List.length failures = if failed then 1 else 0);
           if post_fails
           then (
             let event = List.hd_exn failures in
             match
               Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
               |> protocol_ok
             with
             | Operation_failed { state = Failed error; _ } ->
               assert (not error.retryable);
               assert (
                 String.is_substring
                   (Jsonaf.to_string error.data)
                   ~substring:"post_tool_response");
               assert (
                 String.is_substring
                   (Jsonaf.to_string error.data)
                   ~substring:
                     (History_entry.Id.to_string
                        (Option.value_exn invocation.output_entry_id)))
             | _ -> assert false);
           assert (
             Poly.equal
               state.invocations
               (Agent_session.Memory_backend.state backend).invocations)))
;;

let%test_unit
    "routed calls recheck revoked policy and release cancelled handlers and waits"
  =
  List.iter [ `Revoked; `Cancelled; `Active_cancel; `Session_ended ] ~f:(fun mode ->
    let done_, done_u = Eio.Promise.create () in
    with_handoff_actor
      ~make_worker:(fun env actor_ready ->
        let manager, _, definition =
          handoff_definition
            ~finish:
              (if Poly.equal mode `Session_ended
               then
                 "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                  Task.pure(state))"
               else "Task.pure(state)")
            env
        in
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let first_held, first_held_u = Eio.Promise.create () in
          let release, release_u = Eio.Promise.create () in
          let cancel, cancel_u = Eio.Promise.create () in
          let cancelled, cancelled_u = Eio.Promise.create () in
          let attempted, attempted_u = Eio.Promise.create () in
          let admitted = ref [] in
          let revoked = ref false in
          let prepared = ref 0 in
          let history = ref input.history in
          let allocate item =
            let id =
              History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
            in
            History_entry.create_with_id ~id item
          in
          let request call_id =
            let call =
              allocate
                (Openai.Responses.Item.Function_call
                   { name = "counter"
                   ; arguments = "null"
                   ; call_id
                   ; _type = "function_call"
                   ; id = None
                   ; status = None
                   })
            in
            caps.commit_entry call |> protocol_ok;
            history := !history @ [ call ];
            Chat_response.In_memory_stream.Tool_dispatch.
              { kind = Function
              ; original_name = "counter"
              ; original_payload = "null"
              ; name = "counter"
              ; payload = "null"
              ; rejection = None
              ; call
              ; history = !history
              ; source = None
              ; parent_call_id = None
              }
          in
          let dispatch =
            Agent_session.Moderator_tool_dispatch.create
              ~definition
              ~manager
              ~input
              ~capabilities:caps
              ~available_tools:[]
              ~session_meta:`Null
              ~now:Agent_protocol.Timestamp.now
              ~validate_work:(fun _ -> Error "no pending work")
              ~admit:(fun request ->
                let call_id =
                  match History_entry.item request.call with
                  | Function_call c -> c.call_id
                  | _ -> assert false
                in
                admitted := !admitted @ [ call_id ];
                if !revoked then Error "revoked while queued" else Ok ())
              ~prepare_outcome:(fun _ ->
                Int.incr prepared;
                if !prepared = 1
                then (
                  Eio.Promise.resolve first_held_u ();
                  Eio.Promise.await release);
                Ok ())
              ()
          in
          let run request =
            let result = dispatch.run request ~authorize:ignore |> Option.value_exn in
            let call_id =
              match History_entry.item request.call with
              | Function_call c -> c.call_id
              | _ -> assert false
            in
            let output =
              allocate
                (Openai.Responses.Item.Function_call_output
                   { output = result.output
                   ; call_id
                   ; _type = "function_call_output"
                   ; id = None
                   ; status = None
                   })
            in
            (Option.value_exn result.commit_output) output;
            history := !history @ [ output ]
          in
          let first = request "first" in
          let second = request "second" in
          Eio.Switch.run (fun sw ->
            Eio.Fiber.fork ~sw (fun () ->
              if Poly.equal mode `Active_cancel
              then (
                let result =
                  Eio.Fiber.first
                    (fun () ->
                       run first;
                       `Completed)
                    (fun () ->
                       Eio.Promise.await cancel;
                       `Cancelled)
                in
                assert (Poly.equal result `Cancelled);
                Eio.Promise.resolve cancelled_u ())
              else run first);
            Eio.Promise.await first_held;
            Eio.Fiber.fork ~sw (fun () ->
              if Poly.equal mode `Cancelled
              then (
                let result =
                  Eio.Fiber.first
                    (fun () ->
                       Eio.Promise.resolve attempted_u ();
                       run second;
                       `Completed)
                    (fun () ->
                       Eio.Promise.await cancel;
                       `Cancelled)
                in
                assert (Poly.equal result `Cancelled);
                Eio.Promise.resolve cancelled_u ())
              else (
                Eio.Promise.resolve attempted_u ();
                run second));
            Eio.Promise.await attempted;
            Eio.Fiber.yield ();
            (* These mailbox requests must remain responsive while the first
               handler owns the moderator and the second call waits. *)
            let queued = Agent_session.Session_actor.state actor |> protocol_ok in
            assert (List.length queued.invocations = 2);
            assert (Poly.equal !admitted [ "first" ]);
            if Poly.equal mode `Cancelled
            then (
              Eio.Promise.resolve cancel_u ();
              Eio.Promise.await cancelled;
              let after = Agent_session.Session_actor.state actor |> protocol_ok in
              assert (List.length after.invocations = 2))
            else if Poly.equal mode `Active_cancel
            then (
              Eio.Promise.resolve cancel_u ();
              Eio.Promise.await cancelled)
            else if Poly.equal mode `Revoked
            then revoked := true;
            Eio.Promise.resolve release_u ());
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          assert (!prepared = if Poly.equal mode `Active_cancel then 2 else 1);
          assert (
            Poly.equal
              state.moderator
              (Some
                 (Agent_session.Runtime_builder.encode_moderator_snapshot
                    (Chat_response.Moderator_manager.identity_snapshot manager
                     |> Result.ok_or_failwith))));
          assert (
            Poly.equal
              (Chat_response.Moderator_manager.identity_snapshot manager
               |> Result.ok_or_failwith)
                .current_state
              (Session.Snapshot.Array [ Int 1 ]));
          let failed =
            List.filter state.invocations ~f:(fun inv ->
              match inv.status with
              | Published (Fail error) ->
                assert (
                  String.equal
                    error.code
                    (if Poly.equal mode `Session_ended
                     then "invocation.session_ended"
                     else "invocation.permission_denied"));
                true
              | Published (Complete `Null) -> false
              | Resolved (Cancelled _) ->
                assert (Poly.equal mode `Active_cancel);
                false
              | Admitted ->
                (match mode with
                 | `Cancelled -> ()
                 | _ -> assert false);
                false
              | _ -> assert false)
          in
          assert (
            List.length failed
            = if Poly.equal mode `Revoked || Poly.equal mode `Session_ended then 1 else 0);
          assert (
            Poly.equal
              !admitted
              (if Poly.equal mode `Cancelled || Poly.equal mode `Session_ended
               then [ "first" ]
               else [ "first"; "second" ]));
          revoked := false;
          run (request "third");
          assert (
            !prepared
            =
            if Poly.equal mode `Session_ended
            then 1
            else if Poly.equal mode `Active_cancel
            then 3
            else 2);
          let snapshot =
            Chat_response.Moderator_manager.identity_snapshot manager
            |> Result.ok_or_failwith
          in
          assert (
            Poly.equal
              snapshot.current_state
              (Session.Snapshot.Array
                 [ Int (if Poly.equal mode `Session_ended then 1 else 2) ]));
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          Eio.Promise.resolve done_u ();
          Completed
            { final_history = !history
            ; runtime_requests = []
            ; moderator_snapshot = state.moderator
            }))
      (fun _env actor _writer backend ->
         Eio.Promise.await done_;
         let state = await_idle actor in
         assert (
           Poly.equal
             state.invocations
             (Agent_session.Memory_backend.state backend).invocations)))
;;
