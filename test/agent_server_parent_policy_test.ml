open Core
open Agent_server_test_support
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Owner = Agent_server.Runtime_owner
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let state (entry : Registry.entry) = A.state entry.actor |> protocol_ok

let create_child_result
      ?(lifetime = Agent_server.Session_factory.Owned)
      env
      root
      daemon
      (parent : Registry.entry)
  =
  let module G = Agent_session.Generated_definition in
  let module C = Chat_response.Tool_capability in
  let definition =
    Owner.with_background_runtime parent.runtime (fun runtime ->
      let native =
        Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
      in
      let capabilities =
        Lazy.force native.capabilities
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      let bundle =
        Chatmd_source_bundle.create
          ~root_file:"child.chatmd"
          ~sources:
            [ ( "child.chatmd"
              , {|<developer>Child.</developer><tool type="inherited" name="read_file"/>|}
              )
            ]
          ()
        |> Result.ok_or_failwith
      in
      G.prepare
        ~env
        ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
        ~revision_id:(P.Id.Prompt_revision.create ())
        ~created_at:(P.Timestamp.now ())
        ~current_capabilities:(fun () -> capabilities)
        ~references:(C.references capabilities)
        bundle
      |> Result.map_error ~f:(fun errors ->
        P.Error.invalid_request
          (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string
           |> String.concat ~sep:"\n")))
    |> protocol_ok
  in
  Agent_server.Session_factory.create_generated_session
    ~start_immediately:true
    ~lifetime
    (Daemon.factory daemon)
    ~parent_session_id:(state parent).identity.session_id
    ~idempotency_key:(P.Idempotency_key.of_string "policy-child" |> protocol_ok)
    ~display_name:None
    definition
;;

let create_child env root daemon parent =
  create_child_result env root daemon parent |> protocol_ok
;;

let%expect_test "generated descendants use persisted parent policy across restart" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let save name contents =
          Eio.Path.save
            ~create:(`Exclusive 0o600)
            Eio.Path.(Eio.Stdenv.fs env / root / name)
            contents
        in
        save "allowed.txt" "parent-approved sentinel";
        save "private.txt" "must never be disclosed";
        save
          "parent.chatmd"
          {|<developer>Parent.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="policy" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let rewrite = fun () -> Tool.rewrite_args(`Object([{key = "root"; value = `String("data")}, {key = "file"; value = `String("allowed.txt")}]))
let on_event = fun ctx state event -> match event with
  | `Pre_tool_call(call) ->
    let* ignored = (match state with
      | 0 -> rewrite()
      | 2 -> rewrite()
      | 3 -> Runtime.end_session("parent policy finished")
      | _ -> Tool.reject("private parent reason")) in
    Task.pure(state + 1)
  | _ -> Task.pure(state)
</script>|};
        let configuration = config root root (Filename.concat root "parent.chatmd") in
        let requests = ref 0 in
        let with_daemon f =
          Eio.Switch.run (fun sw ->
            let daemon =
              Daemon.start
                ~sw
                ~env
                ~config:configuration
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { Daemon.default_options with
                    qualify_chatml_extensions = true
                  ; independent_lifetime_policy = Some "moderation-fixture-v1"
                  ; model_post_stream =
                      Some
                        (fun ~sw:_ ~inputs ->
                          Int.incr requests;
                          match List.last inputs with
                          | Some (Openai.Responses.Item.Function_call_output _) ->
                            Stdlib.Seq.empty
                          | _ ->
                            let open Openai.Responses.Response_stream in
                            [ Output_item_added
                                { item =
                                    Function_call
                                      { name = "read_file"
                                      ; arguments = ""
                                      ; call_id = sprintf "read-%d" !requests
                                      ; _type = "function_call"
                                      ; id = Some "read-item"
                                      ; status = None
                                      }
                                ; output_index = 0
                                ; type_ = "response.output_item.added"
                                }
                            ; Function_call_arguments_done
                                { arguments = {|{"root":"data","file":"private.txt"}|}
                                ; item_id = "read-item"
                                ; output_index = 0
                                ; type_ = "response.function_call_arguments.done"
                                }
                            ]
                            |> Stdlib.List.to_seq)
                  }
                ()
              |> protocol_ok
            in
            Exn.protect
              ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      f sw daemon client))))
        in
        let attach sw client id =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client
            ~session_id:id
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        let rec idle entry =
          let current = state entry in
          match current.active_operation with
          | None -> current
          | Some _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            idle entry
        in
        let send handle =
          H.send_message
            handle
            { kind = Plain_text; text = "Read private.txt"; attachments = [] }
          |> protocol_ok
          |> ignore
        in
        let check_parent parent count =
          let current = state parent in
          let snapshot =
            Agent_session.Moderator_checkpoint.decode current.moderator
            |> protocol_ok
            |> Option.value_exn
          in
          (match snapshot.current_state with
           | Session.Snapshot.Int value -> [%test_eq: int] count value
           | _ -> failwith "policy counter missing");
          assert (List.is_empty current.invocations);
          let receipts =
            List.filter current.moderator_executions ~f:(fun receipt ->
              Option.is_some receipt.P.Moderator_execution.delegation)
          in
          [%test_eq: int] count (List.length receipts);
          List.iter receipts ~f:(fun receipt -> assert (Option.is_some receipt.decision));
          receipts
        in
        let check_read child =
          let current = idle child in
          [%test_eq: int] 1 (List.length current.invocations);
          let invocation = List.hd_exn current.invocations in
          (match invocation.status with
           | Published (Complete (`String text)) ->
             assert (String.is_substring text ~substring:"parent-approved sentinel");
             assert (not (String.is_substring text ~substring:"must never"))
           | status ->
             raise_s [%sexp "unexpected child result", (status : P.Invocation.status)]);
          invocation
        in
        let parent_id, child_id, grandchild_id =
          with_daemon (fun sw daemon client ->
            let parent, _ = create_session ~start_immediately:true client in
            let parent_entry =
              Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
            in
            let before = (state parent_entry).moderator in
            (match
               create_child_result ~lifetime:Independent env root daemon parent_entry
             with
             | Error { code = Permission_denied; message; _ } ->
               assert (
                 String.is_prefix
                   message
                   ~prefix:"delegation.independent_moderation_unavailable")
             | _ -> failwith "independent creation omitted the original parent policy");
            assert (
              Option.equal Jsonaf.exactly_equal before (state parent_entry).moderator);
            [%test_eq: int] 0 !requests;
            [%test_eq: int]
              1
              (List.length
                 (Agent_store.Session_store.list_sessions (Daemon.store daemon)));
            let records =
              Agent_store.Delegation_store.with_records
                (Agent_store.Session_store.delegations (Daemon.store daemon))
                ~max_records:8
                ~max_bytes:1048576
                ~f:(fun records -> Ok records)
              |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
              |> protocol_ok
            in
            assert (List.is_empty records);
            let child = create_child env root daemon parent_entry in
            let child_id = (state child).identity.session_id in
            let handle = attach sw client child_id in
            send handle;
            let invocation = check_read child in
            let receipt = check_parent parent_entry 1 |> List.hd_exn in
            let delegation = Option.value_exn receipt.delegation in
            assert (
              P.Id.Invocation.equal delegation.child_invocation_id invocation.context.id);
            assert (P.Id.Session.equal delegation.child_session_id child_id);
            let reference = Option.value_exn (state child).spec.delegation in
            [%test_eq: string] reference.admission_sha256 delegation.admission_sha256;
            send handle;
            let rejected = idle child in
            (* The rejected call is journaled but never executes the native reader. *)
            [%test_eq: int] 2 (List.length rejected.invocations);
            let denied =
              List.find_exn rejected.invocations ~f:(fun candidate ->
                not (P.Id.Invocation.equal candidate.context.id invocation.context.id))
            in
            (match denied.status with
             | Published (Fail error) ->
               [%test_eq: string] "invocation.pre_tool_rejected" error.code;
               assert (
                 not
                   (String.is_substring error.message ~substring:"private parent reason"))
             | status ->
               raise_s [%sexp "unexpected rejection", (status : P.Invocation.status)]);
            let receipts = check_parent parent_entry 2 in
            [%test_eq: int]
              1
              (List.count receipts ~f:(fun receipt ->
                 match receipt.decision with
                 | Some (Reject _) -> true
                 | _ -> false));
            let grandchild = create_child env root daemon child in
            H.detach handle |> protocol_ok;
            parent.id, child_id, (state grandchild).identity.session_id)
        in
        with_daemon (fun sw daemon client ->
          let parent = Registry.load (Daemon.registry daemon) parent_id |> protocol_ok in
          let child = Registry.load (Daemon.registry daemon) child_id |> protocol_ok in
          let grandchild =
            Registry.load (Daemon.registry daemon) grandchild_id |> protocol_ok
          in
          ignore (check_parent parent 2);
          let handle = attach sw client grandchild_id in
          send handle;
          let invocation = check_read grandchild in
          let receipts = check_parent parent 3 in
          assert (
            List.exists receipts ~f:(fun receipt ->
              let delegated = Option.value_exn receipt.delegation in
              P.Id.Session.equal delegated.child_session_id grandchild_id
              && P.Id.Invocation.equal delegated.child_invocation_id invocation.context.id));
          assert (List.is_empty (state child).moderator_executions);
          send handle;
          let rec stopped entry =
            let current = state entry in
            match current.lifecycle.desired, current.active_operation with
            | Stopped, None -> current
            | _ ->
              Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
              stopped entry
          in
          let ended = stopped parent in
          assert ended.halted;
          ignore (stopped child);
          ignore (stopped grandchild);
          ignore (check_parent parent 4);
          H.detach handle |> protocol_ok);
        print_endline
          "parent rewrites and rejects; checkpoint and decisions persist; grandchild \
           inherits through restart; parent end stops descendants"));
  [%expect
    {| parent rewrites and rejects; checkpoint and decisions persist; grandchild inherits through restart; parent end stops descendants |}]
;;
