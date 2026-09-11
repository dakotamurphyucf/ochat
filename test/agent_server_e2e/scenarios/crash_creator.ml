open Core
open Agent_server_test_support
module F = Crash_recovery_fixture
module P = Agent_protocol
module I = P.Invocation
module Daemon = Agent_server.Daemon
module R = Agent_server.Session_registry
module A = Agent_session.Session_actor
module S = Agent_store.Session_store

let request =
  `Object
    [ "version", `Number "1"
    ; "root_file", `String "child.chatmd"
    ; ( "sources"
      , `Array
          [ `Object
              [ "path", `String "child.chatmd"
              ; ( "text"
                , `String
                    {|<developer>Persisted native child.</developer><tool type="inherited" name="read_file"/>|}
                )
              ]
          ] )
    ; "tools", `Array [ `String "read_file" ]
    ; "idempotency_key", `String "creator-crash-key"
    ]
;;

let rec resolved = function
  | Agent_session.Session_delta.Batch deltas -> List.find_map deltas ~f:resolved
  | Invocation_changed invocation
    when I.equal_origin invocation.context.origin Model
         && String.equal invocation.context.tool_name "agent_create" ->
    (match invocation.status with
     | Resolved (Complete value) -> Some value
     | _ -> None)
  | _ -> None
;;

let run_child env ~root ~boundary ~recover =
  let stop () =
    Eio.Flow.copy_string "creator-boundary\n" (Eio.Stdenv.stdout env);
    Eio.Fiber.await_cancel ()
  in
  let observed =
    if recover
    then env
    else
      Support.Crash_fault_io.wrap
        env
        ~boundary:(if String.equal boundary "linked" then After_rename else After_sync)
        ~matches:(fun path ->
          match boundary with
          | "linked" -> String.is_substring path ~substring:"/delegations/"
          | _ ->
            String.is_substring path ~substring:"/journal/"
            && String.is_suffix path ~suffix:".log")
        ~reached:(fun filename ->
          match boundary with
          | "linked" ->
            let contents = F.read env filename in
            (match
               Agent_store.Frame.decode ~max_payload_length:1048576 ~contents ~offset:0
             with
             | Ok (Complete { frame; _ })
               when String.is_substring
                      (Agent_store.Frame.payload frame)
                      ~substring:"(stage Linked)" -> stop ()
             | _ -> ())
          | _ ->
            let scan =
              F.read env filename
              |> Agent_store.Journal_segment.scan_contents
                   ~max_payload_length:(64 * 1024 * 1024)
              |> F.store_ok
            in
            (match List.last scan.entries with
             | Some entry when Agent_store.Frame.flags entry.frame = 0 ->
               let transaction =
                 Agent_store.Frame.payload entry.frame
                 |> Agent_store.Transaction.decode
                 |> F.store_ok
               in
               let delta =
                 Sexp.of_string transaction.delta |> Agent_session.Session_delta.t_of_sexp
               in
               (match resolved delta with
                | None -> ()
                | Some value ->
                  F.write
                    env
                    (Filename.concat root "resolved.json")
                    (Jsonaf.to_string value);
                  stop ())
             | _ -> ()))
  in
  let requests = ref 0 in
  let queued = ref false in
  let disconnected, disconnected_u = Eio.Promise.create () in
  let provider ~sw:_ ~inputs:_ =
    Int.incr requests;
    match !queued with
    | false -> Stdlib.Seq.empty
    | true ->
      queued := false;
      if not recover then Eio.Promise.await disconnected;
      let open Openai.Responses.Response_stream in
      [ Output_item_added
          { item =
              Function_call
                { name = "agent_create"
                ; arguments = ""
                ; call_id = sprintf "creator-%d" !requests
                ; _type = "function_call"
                ; id = Some "creator-item"
                ; status = None
                }
          ; output_index = 0
          ; type_ = "response.output_item.added"
          }
      ; Function_call_arguments_done
          { arguments = Jsonaf.to_string request
          ; item_id = "creator-item"
          ; output_index = 0
          ; type_ = "response.function_call_arguments.done"
          }
      ]
      |> Stdlib.List.to_seq
  in
  let configuration = config root root (Filename.concat root "parent.chatmd") in
  List.iter
    (if recover then [ 1; 2 ] else [ 0 ])
    ~f:(fun reopening ->
      Eio.Switch.run (fun sw ->
        let before = !requests in
        let daemon =
          Daemon.start
            ~sw
            ~env:observed
            ~config:configuration
            ~tool_dir:root
            ~home:root
            ~process_start_identity:None
            ~options:
              { Daemon.default_options with
                qualify_chatml_extensions = true
              ; model_post_stream = Some provider
              }
            ()
          |> F.protocol_ok
        in
        F.require (Int.equal before !requests) "creator recovery called a provider";
        Exn.protect
          ~finally:(fun () -> Daemon.shutdown daemon |> F.protocol_ok)
          ~f:(fun () ->
            let client = connection daemon (principal ()) in
            Exn.protect
              ~finally:(fun () -> Agent_client.Connection.close client)
              ~f:(fun () ->
                initialize client;
                let id =
                  if recover
                  then
                    F.read env (Filename.concat root "parent-id")
                    |> P.Id.Session.of_string
                    |> F.protocol_ok
                  else (
                    let session, _ = create_session ~start_immediately:true client in
                    F.write
                      env
                      (Filename.concat root "parent-id")
                      (P.Id.Session.to_string session.id);
                    session.id)
                in
                let entry = R.load (Daemon.registry daemon) id |> F.protocol_ok in
                let state () = A.state entry.actor |> F.protocol_ok in
                let outcomes current =
                  List.filter
                    current.Agent_session.Session_state.invocations
                    ~f:(fun invocation ->
                      I.equal_origin invocation.I.context.origin Model
                      && String.equal invocation.context.tool_name "agent_create")
                in
                let outputs current =
                  List.count
                    current.Agent_session.Session_state.conversation.canonical_history
                    ~f:(fun history ->
                      P.History.equal_kind history.P.History.kind Tool_output)
                in
                if recover
                then (
                  let current = state () in
                  F.require
                    (Int.equal (outputs current) reopening)
                    "creator response was missing or duplicated on recovery";
                  let first =
                    List.min_elt (outcomes current) ~compare:(fun left right ->
                      P.Timestamp.compare left.context.created_at right.context.created_at)
                    |> Option.value_exn
                  in
                  match boundary, first.status with
                  | "linked", Published (Cancelled reason) ->
                    F.require
                      (String.equal
                         reason
                         "daemon restarted before the invocation recorded an outcome")
                      "creator recovery lost its interruption reason"
                  | "resolved", Published (Complete value) ->
                    F.require
                      (Jsonaf.exactly_equal
                         value
                         (F.read env (Filename.concat root "resolved.json")
                          |> Jsonaf.of_string))
                      "creator recovery changed committed response"
                  | _ ->
                    raise_s
                      [%sexp
                        "creator invocation did not recover its original outcome"
                      , (boundary : string)
                      , (first.status : I.status)]);
                let handle =
                  Agent_client.Session_handle.attach
                    ~sw
                    ~clock:(Eio.Stdenv.clock env)
                    ~connection:client
                    ~session_id:id
                    ~mode:Read_write
                    ~subscribe:false
                    ()
                  |> F.protocol_ok
                in
                queued := true;
                Agent_client.Session_handle.send_message
                  handle
                  { kind = Plain_text; text = "Create once."; attachments = [] }
                |> F.protocol_ok
                |> ignore;
                if not recover
                then (
                  Agent_client.Connection.close client;
                  Eio.Promise.resolve disconnected_u ();
                  Eio.Fiber.await_cancel ());
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
                  let rec idle () =
                    match (state ()).active_operation with
                    | None -> ()
                    | Some _ ->
                      Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
                      idle ()
                  in
                  idle ());
                let current = state () in
                F.require
                  (Int.equal (outputs current) (reopening + 1))
                  "retry did not publish exactly one creator response";
                let records =
                  Agent_store.Delegation_store.with_records
                    (S.delegations (Daemon.store daemon))
                    ~max_records:8
                    ~max_bytes:1048576
                    ~f:(fun records -> Ok records)
                  |> F.store_ok
                in
                let record =
                  match records with
                  | [ record ] -> record
                  | _ -> F.fail "creator retry duplicated its mapping"
                in
                F.require
                  (List.length (S.list_sessions (Daemon.store daemon)) = 2)
                  "creator retry duplicated child storage";
                List.iter (outcomes current) ~f:(fun invocation ->
                  match invocation.status with
                  | Published (Complete value) ->
                    let actual =
                      Jsonaf.member_exn "session_id" value
                      |> Jsonaf.string_exn
                      |> P.Id.Session.of_string
                      |> F.protocol_ok
                    in
                    F.require
                      (P.Id.Session.equal actual record.admission.child_session_id)
                      "creator returned a different child"
                  | Published (Cancelled _) when String.equal boundary "linked" -> ()
                  | _ -> F.fail "creator response is not terminal");
                F.require
                  (Int.equal !requests (2 * reopening))
                  "creator recovery re-executed model work";
                Agent_client.Session_handle.close handle))));
  Eio.Flow.copy_string "creator-recovered\n" (Eio.Stdenv.stdout env)
;;

let test env environment =
  List.iter [ "linked"; "resolved" ] ~f:(fun boundary ->
    let root =
      Filename.concat
        (Support.Temporary_environment.roots environment).temporary
        ("native-creator-" ^ boundary)
    in
    Eio.Path.mkdir ~perm:0o700 (F.path env root);
    F.write
      env
      (Filename.concat root "parent.chatmd")
      {|<developer>Create persisted children.</developer><tool name="agent_create"/><tool name="read_file"><read id="data" path="${workspace}"/></tool>|};
    Eio.Switch.run (fun sw ->
      let child =
        F.child
          ~sw
          env
          environment
          ~case:"creator"
          ~arguments:[ "creator"; root; boundary ]
      in
      Exn.protect
        ~finally:(fun () -> F.terminate env child)
        ~f:(fun () ->
          F.await_marker env child "creator-boundary";
          F.kill env child);
      let recovery =
        F.child
          ~sw
          env
          environment
          ~case:"creator-recover"
          ~arguments:[ "creator-recover"; root; boundary ]
      in
      Exn.protect
        ~finally:(fun () -> F.terminate env recovery)
        ~f:(fun () ->
          let result =
            Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
              Support.Process_manager.await recovery)
          in
          match result.exit with
          | Exited 0
            when String.is_substring result.stdout.contents ~substring:"creator-recovered"
            -> ()
          | _ ->
            raise_s
              [%sexp
                "native creator recovery failed"
              , (boundary : string)
              , (result : Support.Process_manager.result)])))
;;
