open Core
open Fixtures

let%expect_test "prepared one-off programs execute under their native caller's authority" =
  let module A = Agent_session.Session_actor in
  let module C = Chat_response.Tool_capability in
  let module P = Chat_response.One_off_script in
  let module N = Agent_session.Native_tool_invocation in
  let module X = Agent_session.One_off_execution in
  let module I = Agent_protocol.Invocation in
  let module D = Chat_response.In_memory_stream.Tool_dispatch in
  List.iter
    [ `Success
    ; `Outside
    ; `Denied
    ; `Revoked
    ; `Rewrite
    ; `Pre_reject
    ; `Unselected
    ; `Narrowed
    ; `Loop
    ; `Timeout
    ; `Output_limit
    ; `Input_budget
    ; `Output_budget
    ; `Cancel
    ; `Recursive_calls
    ; `Recursive_depth
    ; `Recursive_domain
    ; `Host_rewrite
    ; `Host_reject
    ; `Host_unselected
    ; `Host_invalid
    ; `Host_revoked
    ; `Host_denied
    ; `Host_after_local
    ]
    ~f:(fun mode ->
      let native_calls = ref 0
      and finished = ref false in
      let summaries = ref [] in
      let policy_calls = ref 0 in
      let waiting, waiting_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      with_handoff_actor
        ~make_worker:(fun env actor_ready ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let state = A.state actor |> protocol_ok in
            let root =
              Eio.Path.(
                Eio.Stdenv.fs env
                / state.spec.workspace_instance.canonical_root.native_path)
            in
            let data = Eio.Path.(root / "data") in
            Eio.Path.mkdir ~perm:0o700 data;
            Eio.Path.save
              ~create:(`Or_truncate 0o600)
              Eio.Path.(data / "report.txt")
              "approved report";
            Eio.Path.save
              ~create:(`Or_truncate 0o600)
              Eio.Path.(root / "private.txt")
              "private sentinel";
            let reader =
              Functions.get_contents_scoped
                ~fs:(Eio.Stdenv.fs env)
                ~dir:root
                ~roots:[ Functions.read_file_root ~id:"data" ~path:data () ]
                ()
            in
            let reader =
              { reader with
                run_with_progress =
                  (fun ~invocation payload ->
                    Int.incr native_calls;
                    let borrow = N.borrow () |> protocol_ok in
                    let selected = N.borrowed_capabilities borrow |> protocol_ok in
                    [%test_eq: string list]
                      [ "read_file" ]
                      (List.map (C.references selected) ~f:(fun reference ->
                         reference.name));
                    assert (
                      Result.is_error (N.select_tools borrow ~names:[ "run_chatml" ]));
                    reader.run_with_progress ~invocation payload)
              }
            in
            let invoke = ref (fun (_ : Jsonaf.t) -> failwith "runner not installed") in
            let module Definition = struct
              type input = Jsonaf.t

              let name = "run_chatml"
              let description = Some "qualified one-off execution fixture"
              let type_ = "function"

              let parameters =
                `Object
                  [ "type", `String "object"
                  ; ( "properties"
                    , `Object
                        [ "source", `Object [ "type", `String "string" ]
                        ; "input", `True
                        ; ( "tools"
                          , `Object
                              [ "type", `String "array"
                              ; "items", `Object [ "type", `String "string" ]
                              ] )
                        ] )
                  ; ( "required"
                    , `Array [ `String "source"; `String "input"; `String "tools" ] )
                  ; "additionalProperties", `False
                  ]
              ;;

              let input_of_string = Jsonaf.of_string
            end
            in
            let runner =
              Ochat_function.create_function
                (module Definition)
                (fun request -> !invoke request)
            in
            let all =
              C.create
                ~owner:"one-off fixture"
                ~resource_fingerprint:
                  (Chatmd_shell_spec.Source_ref.digest "scoped read roots")
                [ Chatmd_shell_spec.Source_ref.digest "reader", reader
                ; Chatmd_shell_spec.Source_ref.digest "one-off runner", runner
                ]
              |> Result.map_error ~f:(fun error -> error.C.message)
              |> Result.ok_or_failwith
            in
            let live = ref all in
            let tools =
              Agent_session.Script_tool_calls.create
                ~registry:(fun () -> !live)
                ~moderator_names:String.Set.empty
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () -> (A.state actor |> protocol_ok).halted)
                ~requires_active_moderator:(fun _ -> false)
                ~authorize:(fun _ _ ->
                  match mode with
                  | `Denied | `Host_denied -> Error (handoff_error "denied")
                  | `Revoked ->
                    live
                    := C.select all ~names:[ "run_chatml" ]
                       |> Result.map_error ~f:(fun error -> error.C.message)
                       |> Result.ok_or_failwith;
                    Ok ()
                  | _ -> Ok ())
                ~prepare_output:(function
                  | Text text -> Ok (`String text)
                  | _ -> assert false)
                ~defer_observation:(fun _ -> failwith "fixture has no moderator")
            in
            let tools =
              match mode with
              | `Host_rewrite
              | `Host_reject
              | `Host_unselected
              | `Host_invalid
              | `Host_revoked
              | `Host_denied
              | `Host_after_local
              | `Pre_reject ->
                Agent_session.Script_tool_calls.with_preparation
                  tools
                  ~prepare:(fun request ->
                    Int.incr policy_calls;
                    let current = A.state actor |> protocol_ok in
                    (match request.owner with
                     | Native_call parent_id ->
                       let parent =
                         List.find_exn current.invocations ~f:(fun i ->
                           Agent_protocol.Id.Invocation.equal i.context.id parent_id)
                       in
                       (match parent.status with
                        | Dispatching -> ()
                        | _ -> assert false)
                     | _ -> failwith "script preparation lost native owner");
                    assert (
                      not
                        (List.exists current.invocations ~f:(fun i ->
                           Agent_protocol.Id.Invocation.equal
                             i.context.id
                             request.invocation_id)));
                    [%test_eq: string list]
                      [ "read_file" ]
                      (List.map (C.references request.selected) ~f:(fun r -> r.name));
                    Eio.Fiber.yield ();
                    match mode with
                    | `Host_after_local ->
                      assert (
                        Jsonaf.exactly_equal
                          request.call.args
                          (`Object
                              [ "root", `String "data"; "file", `String "from-local.txt" ]));
                      Ok
                        (Some
                           (Rewrite_args
                              (`Object
                                  [ "root", `String "data"; "file", `String "report.txt" ])))
                    | `Host_reject -> Ok (Some (Reject "parent rule"))
                    | `Host_unselected -> Ok (Some (Redirect ("run_chatml", `Null)))
                    | `Host_invalid -> Ok (Some (Rewrite_args `Null))
                    | `Host_revoked ->
                      live
                      := C.select all ~names:[ "run_chatml" ]
                         |> Result.map_error ~f:(fun e -> e.C.message)
                         |> Result.ok_or_failwith;
                      Ok None
                    | `Pre_reject ->
                      failwith "locally rejected call reached parent policy"
                    | _ ->
                      Ok
                        (Some
                           (Rewrite_args
                              (`Object
                                  [ "root", `String "data"; "file", `String "report.txt" ]))))
              | _ -> tools
            in
            let source =
              match mode with
              | `Loop ->
                "let rec loop x = loop(x)\n\
                 let poison = loop(0)\n\
                 let main input = Task.pure(input)"
              | `Output_budget ->
                "let main input = Task.pure(`String(\"" ^ String.make 12_800 'x' ^ "\"))"
              | _ ->
                {|let count = [0]
let main input = Task.bind(Tool.call("|}
                ^ (match mode with
                   | `Unselected -> "run_chatml"
                   | _ -> "read_file")
                ^ {|", input), fun result -> match result with
  | `Ok(value) -> let ignored = count[0] <- count[0] + 1 in
    (match value with
     | `String(text) -> Task.pure(`String(to_string(count[0]) ++ ":" ++ text))
     | _ -> Task.fail("expected text"))
  | `Error(code) -> Task.pure(`String(code)))|}
            in
            let field fields name = List.Assoc.find_exn fields ~equal:String.equal name in
            (invoke
             := fun request ->
                  let fields =
                    match request with
                    | `Object fields -> fields
                    | _ -> assert false
                  in
                  let source =
                    match field fields "source" with
                    | `String text -> text
                    | _ -> assert false
                  in
                  let names =
                    match field fields "tools" with
                    | `Array names ->
                      List.map names ~f:(function
                        | `String name -> name
                        | _ -> assert false)
                    | _ -> assert false
                  in
                  let borrowed = N.borrow () |> protocol_ok in
                  let ceiling = N.borrowed_capabilities borrowed |> protocol_ok in
                  (match mode with
                   | `Narrowed ->
                     let broader =
                       P.prepare_in_domain ~env ~capabilities:all ~tools:names ~source ()
                       |> Result.map_error ~f:(fun _ -> "preparation failed")
                       |> Result.ok_or_failwith
                     in
                     let rejected =
                       X.run
                         ~env
                         ~prepared:broader
                         ~borrowed
                         ~script_tools:tools
                         ~input:(field fields "input")
                         ~limits:Chatmd_shell_spec.Chatmd_script_spec.default_limits
                         ~max_nested_calls:10
                         ~now:Agent_protocol.Timestamp.now
                         ~moderate_tool:(fun _ _ ->
                           failwith "broader program reached moderation")
                         ~prepare_outcome:(fun _ -> failwith "broader program executed")
                         ()
                     in
                     assert (Result.is_error rejected);
                     summaries := "borrow rejected" :: !summaries
                   | _ -> ());
                  let prepared =
                    P.prepare_in_domain ~env ~capabilities:ceiling ~tools:names ~source ()
                  in
                  match prepared with
                  | Error diagnostics ->
                    summaries := (List.hd_exn diagnostics).code :: !summaries;
                    Openai.Responses.Tool_output.Output.Text "compile rejected"
                  | Ok prepared ->
                    let limits =
                      { Chatmd_shell_spec.Chatmd_script_spec.default_limits with
                        fuel = 5000
                      ; wall_time =
                          Chatmd_shell_spec.Duration.parse
                            (match mode with
                             | `Timeout -> "10ms"
                             | _ -> "2s")
                          |> Result.ok_or_failwith
                      ; max_output_bytes =
                          Chatmd_shell_spec.Duration.parse_bytes
                            (match mode with
                             | `Output_limit -> "20B"
                             | _ -> "16KiB")
                          |> Result.ok_or_failwith
                      }
                    in
                    let root_call =
                      I.equal_origin (N.borrowed_invocation borrowed).context.origin Model
                    in
                    let execute () =
                      X.run
                        ~env
                        ~prepared
                        ~allocation_bytes:
                          (match mode with
                           | `Input_budget -> 128
                           | `Output_budget -> 8192
                           | _ -> Chatml_execution.default_limits.allocation_bytes)
                        ~borrowed
                        ~script_tools:tools
                        ~input:(field fields "input")
                        ~limits
                        ~max_nested_calls:
                          (match mode, root_call with
                           | (`Recursive_calls | `Recursive_domain), true -> 1
                           | _ -> 10)
                        ~max_invocation_depth:
                          (match mode, root_call with
                           | `Recursive_depth, true -> 1
                           | _ -> 32)
                        ~now:Agent_protocol.Timestamp.now
                        ~moderate_tool:(fun _ _ ->
                          match mode with
                          | `Timeout -> Eio.Promise.await never
                          | `Cancel ->
                            Eio.Promise.resolve waiting_u ();
                            Eio.Promise.await never
                          | `Pre_reject ->
                            Ok
                              (Some
                                 { Chat_response.Moderation.Outcome.empty with
                                   tool_moderation = Some (Reject "rejected")
                                 })
                          | `Rewrite | `Host_after_local ->
                            Ok
                              (Some
                                 { Chat_response.Moderation.Outcome.empty with
                                   tool_moderation =
                                     Some
                                       (Rewrite_args
                                          (`Object
                                              [ "root", `String "data"
                                              ; ( "file"
                                                , `String
                                                    (match mode with
                                                     | `Host_after_local ->
                                                       "from-local.txt"
                                                     | _ -> "report.txt") )
                                              ]))
                                 })
                          | _ -> Ok None)
                        ~prepare_outcome:(fun _ -> Ok ())
                        ()
                      |> protocol_ok
                    in
                    let result =
                      match mode, root_call with
                      | `Recursive_domain, false ->
                        Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) execute
                      | _ -> execute ()
                    in
                    assert (List.is_empty result.runtime_requests);
                    assert (
                      String.equal
                        result.resolved.context.implementation_revision
                        (P.fingerprint prepared));
                    let outcome =
                      match result.resolved.status with
                      | Resolved outcome -> outcome
                      | _ -> assert false
                    in
                    let summary =
                      match outcome with
                      | Complete (`String text) ->
                        assert (
                          not (String.is_substring text ~substring:"private sentinel"));
                        (match mode with
                         | `Success | `Rewrite | `Host_rewrite | `Host_after_local ->
                           assert (String.is_prefix text ~prefix:"1:");
                           assert (String.is_substring text ~substring:"approved report");
                           "read"
                         | `Outside ->
                           assert (
                             String.is_substring text ~substring:"error running read_file");
                           "path denied"
                         | _ -> text)
                      | Fail error -> error.code
                      | _ -> failwith "unexpected one-off outcome"
                    in
                    summaries := summary :: !summaries;
                    Openai.Responses.Tool_output.Output.Text
                      (Jsonaf.to_string (I.outcome_to_json outcome)));
            let selected =
              match mode with
              | `Narrowed ->
                C.select all ~names:[ "run_chatml" ]
                |> Result.map_error ~f:(fun error -> error.C.message)
                |> Result.ok_or_failwith
              | _ -> all
            in
            let dispatch =
              Agent_session.Native_tool_dispatch.create
                ~input
                ~capabilities:caps
                ~declared:selected
                ~registry:(fun () -> selected)
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () -> false)
                ~admit:(fun _ _ -> Ok ())
                ~prepare_output:(function
                  | Text text -> Ok (`String text)
                  | _ -> assert false)
            in
            let invoke_model call_id =
              let filename =
                match mode with
                | `Outside -> "../private.txt"
                | `Rewrite | `Host_rewrite | `Host_denied | `Host_after_local ->
                  "missing.txt"
                | _ -> "report.txt"
              in
              let file_input =
                `Object [ "root", `String "data"; "file", `String filename ]
              in
              let source, script_input, names =
                match mode with
                | `Recursive_calls | `Recursive_depth | `Recursive_domain ->
                  ( {|let main input = Task.bind(Tool.call("run_chatml", input), fun ignored -> Task.pure(`String("ignored")))|}
                  , `Object
                      [ "source", `String source
                      ; "input", file_input
                      ; "tools", `Array [ `String "read_file" ]
                      ]
                  , [ `String "read_file"; `String "run_chatml" ] )
                | _ -> source, file_input, [ `String "read_file" ]
              in
              let payload =
                Jsonaf.to_string
                  (`Object
                      [ "source", `String source
                      ; "input", script_input
                      ; "tools", `Array names
                      ])
              in
              let id =
                History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
              in
              let call =
                History_entry.create_with_id
                  ~id
                  (Chat_response.Tool_call.call_item
                     ~kind:Function
                     ~name:"run_chatml"
                     ~payload
                     ~call_id
                     ~id:None)
              in
              let request =
                D.
                  { kind = Function
                  ; original_name = "run_chatml"
                  ; original_payload = payload
                  ; name = "run_chatml"
                  ; payload
                  ; rejection = None
                  ; call
                  ; history = input.history @ [ call ]
                  ; source = None
                  ; parent_call_id = None
                  }
              in
              assert (dispatch.commit_call request);
              let result = dispatch.run request ~authorize:ignore |> Option.value_exn in
              let id =
                History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
              in
              let output =
                History_entry.create_with_id
                  ~id
                  (Chat_response.Tool_call.output_item
                     ~kind:Function
                     ~call_id
                     ~output:result.output)
              in
              Option.value_exn result.commit_output output
            in
            (match mode with
             | `Success ->
               Eio.Fiber.both
                 (fun () -> invoke_model "first")
                 (fun () -> invoke_model "second")
             | _ -> invoke_model "only");
            finished := true;
            let state = A.state actor |> protocol_ok in
            Completed
              { final_history =
                  Agent_session.History_codec.all_of_protocol
                    state.conversation.canonical_history
                  |> protocol_ok
              ; moderator_snapshot = None
              ; runtime_requests = []
              }))
        (fun _env actor writer backend ->
           (match mode with
            | `Cancel ->
              Eio.Promise.await waiting;
              A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore
            | _ -> ());
           let rec terminal () =
             let state = A.state actor |> protocol_ok in
             match state.active_operation with
             | None -> state
             | Some _ ->
               Eio.Fiber.yield ();
               terminal ()
           in
           let state = terminal () in
           (match mode with
            | `Cancel -> assert (not !finished)
            | _ -> assert !finished);
           assert (Option.is_none state.failure);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           let children =
             List.filter state.invocations ~f:(fun invocation ->
               Option.is_some invocation.context.parent_invocation)
           in
           List.iter children ~f:(fun invocation ->
             assert (I.equal_origin invocation.context.origin Script);
             assert (Option.is_none invocation.context.provider_call_id);
             assert (Option.is_none invocation.output_entry_id);
             assert (Option.is_some invocation.context.deadline));
           let cancelled =
             List.count children ~f:(fun invocation ->
               match invocation.status with
               | Resolved (Cancelled _) -> true
               | _ -> false)
           in
           let expected =
             match mode with
             | `Success -> [ "read"; "read" ]
             | `Outside -> [ "path denied" ]
             | `Denied -> [ "invocation.permission_denied" ]
             | `Revoked -> [ "invocation.stale_binding" ]
             | `Rewrite -> [ "read" ]
             | `Pre_reject -> [ "invocation.pre_tool_rejected" ]
             | `Unselected -> [ "invocation.unselected_tool" ]
             | `Narrowed -> [ "borrow rejected"; "capability.not_selected" ]
             | `Loop -> [ "chatml.execution_limit" ]
             | `Timeout -> [ "chatml.execution_timeout" ]
             | `Output_limit -> [ "invocation.output_limit" ]
             | `Input_budget | `Output_budget -> [ "chatml.allocation_limit" ]
             | `Cancel -> []
             | `Recursive_calls | `Recursive_domain ->
               [ "chatml.call_limit"; "chatml.call_limit" ]
             | `Recursive_depth ->
               [ "chatml.invocation_depth"; "chatml.invocation_depth" ]
             | `Host_rewrite | `Host_after_local -> [ "read" ]
             | `Host_reject -> [ "invocation.pre_tool_rejected" ]
             | `Host_unselected -> [ "invocation.unselected_tool" ]
             | `Host_invalid | `Host_revoked -> [ "invocation.pre_tool_failed" ]
             | `Host_denied -> [ "invocation.permission_denied" ]
           in
           (match mode with
            | `Host_rewrite
            | `Host_reject
            | `Host_unselected
            | `Host_invalid
            | `Host_revoked
            | `Host_denied -> [%test_eq: int] 1 !policy_calls
            | `Host_after_local ->
              [%test_eq: int] 1 !policy_calls;
              let reader =
                List.find_exn children ~f:(fun i ->
                  String.equal i.context.tool_name "read_file")
              in
              let routing = Option.value_exn reader.routing in
              [%test_eq: string]
                (Chatmd_shell_spec.Source_ref.digest
                   "{\"root\":\"data\",\"file\":\"missing.txt\"}")
                routing.original_payload.sha256
            | `Pre_reject -> [%test_eq: int] 0 !policy_calls
            | _ -> ());
           [%test_eq: string list] expected (List.sort !summaries ~compare:String.compare);
           print_s
             [%sexp
               (mode
                : [ `Success
                  | `Outside
                  | `Denied
                  | `Revoked
                  | `Rewrite
                  | `Pre_reject
                  | `Unselected
                  | `Narrowed
                  | `Loop
                  | `Timeout
                  | `Output_limit
                  | `Input_budget
                  | `Output_budget
                  | `Cancel
                  | `Recursive_calls
                  | `Recursive_depth
                  | `Recursive_domain
                  | `Host_rewrite
                  | `Host_reject
                  | `Host_unselected
                  | `Host_invalid
                  | `Host_revoked
                  | `Host_denied
                  | `Host_after_local
                  ])
             , (!native_calls : int)
             , (List.sort !summaries ~compare:String.compare : string list)
             , (List.length children : int)
             , (cancelled : int)]));
  [%expect
    {|
    (Success 2 (read read) 4 0)
    (Outside 1 ("path denied") 2 0)
    (Denied 0 (invocation.permission_denied) 2 0)
    (Revoked 0 (invocation.stale_binding) 2 0)
    (Rewrite 1 (read) 2 0)
    (Pre_reject 0 (invocation.pre_tool_rejected) 2 0)
    (Unselected 0 (invocation.unselected_tool) 1 0)
    (Narrowed 0 ("borrow rejected" capability.not_selected) 0 0)
    (Loop 0 (chatml.execution_limit) 1 0)
    (Timeout 0 (chatml.execution_timeout) 1 0)
    (Output_limit 1 (invocation.output_limit) 2 0)
    (Input_budget 0 (chatml.allocation_limit) 1 0)
    (Output_budget 0 (chatml.allocation_limit) 1 0)
    (Cancel 0 () 1 1)
    (Recursive_calls 0 (chatml.call_limit chatml.call_limit) 3 0)
    (Recursive_depth 0 (chatml.invocation_depth chatml.invocation_depth) 3 0)
    (Recursive_domain 0 (chatml.call_limit chatml.call_limit) 3 0)
    (Host_rewrite 1 (read) 2 0)
    (Host_reject 0 (invocation.pre_tool_rejected) 2 0)
    (Host_unselected 0 (invocation.unselected_tool) 2 0)
    (Host_invalid 0 (invocation.pre_tool_failed) 2 0)
    (Host_revoked 0 (invocation.pre_tool_failed) 2 0)
    (Host_denied 0 (invocation.permission_denied) 2 0)
    (Host_after_local 1 (read) 2 0)
    |}]
;;
