open Core
open Fixtures
module P = Agent_protocol
module I = P.Invocation
module A = Agent_session.Session_actor
module C = Chat_response.Tool_capability
module Stream = Chat_response.In_memory_stream
module Routing = Agent_session.Stream_invocation
module Tools = Agent_session.Script_tool_calls

let%expect_test "moderator event tool preparation retains the actual event owner" =
  List.iter [ false; true ] ~f:(fun reject ->
    let calls = ref 0 in
    let policies = ref 0 in
    let registry = native_registry calls ~raises:false in
    Job_fixtures.with_actor (fun env _sw actor _writer backend ->
      let manager, _, definition =
        handoff_definition ~declare_tool:false ~capability_registry:registry env
      in
      let snapshot =
        Chat_response.Moderator_manager.identity_snapshot manager |> Result.ok_or_failwith
      in
      A.change_moderator
        actor
        (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot))
      |> protocol_ok
      |> ignore;
      let tools =
        Tools.create
          ~registry:(fun () -> registry)
          ~moderator_names:String.Set.empty
          ~now:P.Timestamp.now
          ~is_halted:(fun () -> false)
          ~requires_active_moderator:(fun _ -> false)
          ~authorize:(fun _ _ -> Ok ())
          ~prepare_output:(fun _ -> Ok (`String "parent event result"))
          ~defer_observation:(fun _ -> Ok ())
      in
      let event_id = ref None in
      let tools =
        Tools.with_preparation tools ~prepare:(fun request ->
          Int.incr policies;
          (match request.owner with
           | Moderator_event id ->
             assert (Option.equal P.Id.Moderator_execution.equal (Some id) !event_id)
           | _ -> failwith "moderator native call lost its event owner");
          let state = A.state actor |> protocol_ok in
          assert (P.Id.Session.equal request.session_id state.identity.session_id);
          [%test_eq: int] state.identity.generation request.generation;
          assert (
            not
              (List.exists state.invocations ~f:(fun invocation ->
                 P.Id.Invocation.equal request.invocation_id invocation.context.id)));
          Eio.Fiber.yield ();
          Ok
            (Some
               (if reject
                then Chat_response.Moderation.Tool_moderation.Reject "parent rule"
                else Approve)))
      in
      let claimed =
        A.with_current_moderator_event
          actor
          ~operation_id:None
          ~event:Session_start
          ~snapshot:(fun () -> Ok snapshot)
          (fun ~executing ~retirement_reason:_ ~event:_ ~execute ~commit ->
             event_id := Some executing.context.id;
             let result =
               Tools.with_event tools ~definition ~execute ~executing (fun call ->
                 call ~name:"read_file" ~args:(`Object []))
               |> Result.ok_or_failwith
             in
             (match reject, result with
              | true, Tool_error "invocation.pre_tool_rejected"
              | false, Tool_ok (`String "parent event result") -> ()
              | _ -> failwith "incorrect moderator native preparation result");
             commit
               ~snapshot
               ~requests:
                 { request_turn = false; request_compaction = false; end_session = None })
        |> protocol_ok
      in
      assert claimed;
      let state = Agent_session.Memory_backend.state backend in
      let invocation = List.hd_exn state.invocations in
      assert (
        Option.equal P.Id.Moderator_execution.equal invocation.parent_event !event_id);
      assert (Option.is_none invocation.context.parent_invocation);
      assert (I.equal_origin invocation.context.origin Moderator);
      [%test_eq: int] 1 !policies;
      print_s [%sexp (reject : bool), (!calls : int)]));
  [%expect
    {|
    (false 1)
    (true 0) |}]
;;

let%expect_test "host preparation routes canonical calls before owned admission" =
  List.iter
    [ `Rewrite
    ; `Redirect
    ; `Custom
    ; `Reject
    ; `Failure
    ; `Invalid
    ; `Revoked
    ; `Legacy
    ; `Save_fail
    ; `Repeated
    ]
    ~f:(fun mode ->
      let custom =
        match mode with
        | `Custom -> true
        | _ -> false
      in
      let effects = ref [] in
      let prepared_ids = ref [] in
      let make_tool name =
        let module Definition = struct
          type input = string

          let name = name
          let description = None
          let type_ = if custom then "custom" else "function"

          let parameters =
            `Object [ "type", `String (if custom then "string" else "object") ]
          ;;

          let input_of_string input = input
        end
        in
        Ochat_function.create_function
          (module Definition)
          (fun payload ->
             effects := !effects @ [ name, payload ];
             Openai.Responses.Tool_output.Output.Text "routed result")
      in
      let declared =
        C.create
          ~owner:"host-routing-fixture"
          ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "routing resources")
          (List.map [ "first"; "second" ] ~f:(fun name ->
             Chatmd_shell_spec.Source_ref.digest name, make_tool name))
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      let registry = ref declared in
      with_handoff_actor
        ~reject:(fun next ->
          match mode with
          | `Save_fail -> not (List.is_empty next.state.invocations)
          | _ -> false)
        ~make_worker:(fun env actor_ready ->
          Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let state = A.state actor |> protocol_ok in
            let response_dir =
              Eio.Path.(
                Eio.Stdenv.fs env
                / state.spec.workspace_instance.canonical_root.native_path
                / "response")
            in
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
            let requests = ref 0 in
            let post_stream ~sw:_ ~inputs:_ =
              Int.incr requests;
              let count =
                match mode with
                | `Repeated -> 2
                | _ -> 1
              in
              if !requests > count
              then Stdlib.Seq.empty
              else
                let open Openai.Responses.Response_stream in
                let payload =
                  match mode with
                  | `Invalid -> "null"
                  | _ -> "{}"
                in
                Stdlib.List.to_seq
                  [ Output_item_added
                      { item =
                          (if custom
                           then
                             Custom_function
                               { name = "first"
                               ; input = ""
                               ; call_id = "provider-call"
                               ; _type = "custom_tool_call"
                               ; id = Some "provider-item"
                               }
                           else
                             Function_call
                               { name = "first"
                               ; arguments = ""
                               ; call_id = "provider-call"
                               ; _type = "function_call"
                               ; id = Some "provider-item"
                               ; status = None
                               })
                      ; output_index = 0
                      ; type_ = "response.output_item.added"
                      }
                  ; (if custom
                     then
                       Custom_tool_call_input_done
                         { input = payload
                         ; item_id = "provider-item"
                         ; output_index = 0
                         ; type_ = "response.custom_tool_call_input.done"
                         }
                     else
                       Function_call_arguments_done
                         { arguments = payload
                         ; item_id = "provider-item"
                         ; output_index = 0
                         ; type_ = "response.function_call_arguments.done"
                         })
                  ]
            in
            let dispatch_tool ~input ~capabilities =
              let native =
                Agent_session.Native_tool_dispatch.create
                  ~input
                  ~capabilities
                  ~declared
                  ~registry:(fun () -> !registry)
                  ~now:P.Timestamp.now
                  ~is_halted:(fun () -> false)
                  ~admit:(fun _ _ -> Ok ())
                  ~prepare_output:(function
                    | Text text -> Ok (`String text)
                    | _ -> Error (handoff_error "expected text"))
              in
              let tools =
                Tools.create
                  ~registry:(fun () -> !registry)
                  ~moderator_names:String.Set.empty
                  ~now:P.Timestamp.now
                  ~is_halted:(fun () -> false)
                  ~requires_active_moderator:(fun _ -> false)
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(function
                    | Text text -> Ok (`String text)
                    | _ -> Error (handoff_error "expected text"))
                  ~defer_observation:(fun _ -> Ok ())
              in
              let tools =
                Tools.with_preparation tools ~prepare:(fun request ->
                  let id = request.invocation_id in
                  (match request.owner with
                   | Model_call (operation_id, call_id) ->
                     assert (P.Id.Operation.equal operation_id input.operation.id);
                     assert (
                       P.Id.Invocation.equal id (Routing.id_for_call ~input ~call_id))
                   | _ -> failwith "model preparation lost its owner");
                  let current = A.state actor |> protocol_ok in
                  assert (
                    not
                      (List.exists current.invocations ~f:(fun saved ->
                         P.Id.Invocation.equal saved.context.id id)));
                  prepared_ids := !prepared_ids @ [ id ];
                  assert (String.equal request.call.name "first");
                  assert (String.equal request.call.payload_text "{}");
                  Eio.Fiber.yield ();
                  let args = `Object [ "rewritten", `True ] in
                  match mode with
                  | `Reject -> Ok (Some (Reject "parent rule"))
                  | `Failure -> Error (handoff_error "private parent diagnostic")
                  | `Invalid -> failwith "invalid input reached host policy"
                  | `Revoked ->
                    registry
                    := C.select declared ~names:[]
                       |> Result.map_error ~f:(fun error -> error.C.message)
                       |> Result.ok_or_failwith;
                    Ok None
                  | `Legacy -> Ok (Some (Redirect ("legacy", args)))
                  | `Redirect -> Ok (Some (Redirect ("second", args)))
                  | `Custom -> Ok (Some (Rewrite_args (`String "rewritten custom")))
                  | `Rewrite | `Repeated | `Save_fail -> Ok (Some (Rewrite_args args)))
              in
              match mode with
              | `Legacy ->
                Stream.Tool_dispatch.with_preparation native ~prepare:(fun _ ->
                  Ok (Some (Redirect ("legacy", `Object []))))
              | _ -> Tools.with_model_preparation tools ~selected:declared ~input native
            in
            let tool_tbl = String.Table.create () in
            Hashtbl.set tool_tbl ~key:"legacy" ~data:(fun ~invocation:_ _ ->
              effects := !effects @ [ "legacy", "must not run" ];
              Openai.Responses.Tool_output.Output.Text "legacy result");
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
                ; moderator = None
                ; permission_profile =
                    permission_policy
                      ~tool_default:Allow
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
                ; redact_tool_payload = (fun ~name:_ _ -> "\"redacted\"")
                }
            in
            Agent_session.Operation_worker.run worker ~sw ~input caps))
        (fun _env actor _writer backend ->
           let rec terminal () =
             let state = A.state actor |> protocol_ok in
             match state.active_operation with
             | None -> state
             | Some _ ->
               Eio.Fiber.yield ();
               terminal ()
           in
           let state = terminal () in
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           let outcome =
             match mode, state.invocations with
             | (`Legacy | `Save_fail), [] -> "admission rejected"
             | ( `Invalid
               , [ { status = Published (Fail { code = "invocation.invalid_input"; _ })
                   ; _
                   }
                 ] ) ->
               [%test_eq: int] 0 (List.length !prepared_ids);
               "invalid input"
             | ( (`Reject | `Failure | `Revoked)
               , [ { status = Published (Fail failure); _ } ] ) -> failure.code
             | (`Rewrite | `Redirect | `Custom | `Repeated), (_ :: _ as invocations) ->
               List.iter invocations ~f:(fun invocation ->
                 assert (
                   List.mem
                     !prepared_ids
                     invocation.context.id
                     ~equal:P.Id.Invocation.equal);
                 let name, raw =
                   match mode with
                   | `Redirect -> "second", "{\"rewritten\":true}"
                   | `Custom -> "first", "rewritten custom"
                   | _ -> "first", "{\"rewritten\":true}"
                 in
                 assert (String.equal invocation.context.tool_name name);
                 let call =
                   List.find_exn state.conversation.canonical_history ~f:(fun entry ->
                     Option.exists
                       invocation.context.call_entry_id
                       ~f:(History_entry.Id.equal entry.id))
                   |> Agent_session.History_codec.of_protocol
                   |> protocol_ok
                 in
                 (match History_entry.item call with
                  | Function_call call ->
                    [%test_eq: string] name call.name;
                    [%test_eq: string] "\"redacted\"" call.arguments
                  | Custom_tool_call call ->
                    [%test_eq: string] name call.name;
                    [%test_eq: string] "\"redacted\"" call.input
                  | _ -> failwith "missing prepared canonical call");
                 let routing = Option.value_exn invocation.routing in
                 [%test_eq: string] "first" routing.original_name;
                 [%test_eq: string]
                   (Chatmd_shell_spec.Source_ref.digest "{}")
                   routing.original_payload.sha256;
                 [%test_eq: string]
                   (Chatmd_shell_spec.Source_ref.digest raw)
                   routing.final_payload.sha256;
                 [%test_eq: string]
                   (Chatmd_shell_spec.Source_ref.digest "\"redacted\"")
                   (Option.value_exn routing.canonical_payload).sha256;
                 assert (Option.is_some invocation.output_entry_id);
                 match invocation.status with
                 | Published (Complete (`String "routed result")) -> ()
                 | status ->
                   raise_s
                     [%sexp
                       "wrong routed outcome"
                     , (status : I.status)
                     , (!effects : (string * string) list)]);
               [%test_eq: int] (List.length invocations) (List.length !effects);
               let expected_effect =
                 match mode with
                 | `Redirect -> "second", "{\"rewritten\":true}"
                 | `Custom -> "first", "rewritten custom"
                 | _ -> "first", "{\"rewritten\":true}"
               in
               [%test_eq: (string * string) list]
                 (List.init (List.length invocations) ~f:(fun _ -> expected_effect))
                 !effects;
               [%test_eq: int]
                 (List.length invocations)
                 (List.dedup_and_sort !prepared_ids ~compare:P.Id.Invocation.compare
                  |> List.length);
               "routed"
             | _ -> failwith "unexpected host preparation result"
           in
           (match mode with
            | `Rewrite | `Redirect | `Custom | `Repeated -> ()
            | _ -> assert (List.is_empty !effects));
           print_s
             [%sexp
               (mode
                : [ `Rewrite
                  | `Redirect
                  | `Custom
                  | `Reject
                  | `Failure
                  | `Invalid
                  | `Revoked
                  | `Legacy
                  | `Save_fail
                  | `Repeated
                  ])
             , (outcome : string)
             , (List.length !effects : int)]));
  [%expect
    {|
    (Rewrite routed 1)
    (Redirect routed 1)
    (Custom routed 1)
    (Reject invocation.pre_tool_rejected 0)
    (Failure invocation.pre_tool_failed 0)
    (Invalid "invalid input" 0)
    (Revoked invocation.pre_tool_failed 0)
    (Legacy "admission rejected" 0)
    (Save_fail "admission rejected" 0)
    (Repeated routed 2) |}]
;;
