open Core
open Agent_server_test_support
module P = Agent_protocol
module Daemon = Agent_server.Daemon
module D = Agent_store.Delegation_store
module Registry = Agent_server.Session_registry
module Factory = Agent_server.Session_factory
module Owner = Agent_server.Runtime_owner
module A = Agent_session.Session_actor
module H = Agent_client.Session_handle

let observe_rename
      ?(before_stat = fun _ -> ())
      (Eio.Resource.T (directory, handler) as native_directory)
      reached
  =
  let module Original = (val Eio.Resource.get handler Eio.Fs.Pi.Dir) in
  let module Directory = struct
    include Original

    let stat directory ~follow path =
      before_stat path;
      Original.stat directory ~follow path
    ;;

    let rename directory source _destination target =
      Original.rename directory source native_directory target;
      reached target
    ;;
  end
  in
  let bindings =
    Eio.Resource.bindings handler
    |> List.filter ~f:(function
      | H (Eio.Fs.Pi.Dir, _) -> false
      | _ -> true)
  in
  Eio.Resource.T
    (directory, Eio.Resource.handler (H (Eio.Fs.Pi.Dir, (module Directory)) :: bindings))
;;

let state (entry : Registry.entry) = A.state entry.actor |> protocol_ok
let id entry = (state entry).identity.session_id

let prepare
      ?(inherited_tools = [ "read_file" ])
      ?(moderator = "")
      env
      root
      (parent : Registry.entry)
  =
  let module G = Agent_session.Generated_definition in
  let module C = Chat_response.Tool_capability in
  Owner.with_background_runtime parent.runtime (fun runtime ->
    let native = Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime in
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
            , "<developer>Read the inherited workspace.</developer>"
              ^ (List.map inherited_tools ~f:(fun name ->
                   sprintf {|<tool type="inherited" name="%s"/>|} name)
                 |> String.concat)
              ^ moderator )
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
;;

let create daemon parent definition ~key ~lifetime =
  Factory.create_generated_session
    ~start_immediately:true
    ~lifetime
    (Daemon.factory daemon)
    ~parent_session_id:(id parent)
    ~idempotency_key:(P.Idempotency_key.of_string key |> protocol_ok)
    ~display_name:None
    definition
;;

let initial_start_moderator id =
  sprintf
    {|<script id=%S language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start -> Task.pure(state + 1)
| _ -> Task.pure(state)
</script>|}
    id
;;

let read_unpublished daemon ~sw child_id =
  let module Store = Agent_store.Session_store in
  let store = Daemon.store daemon in
  let handle =
    Store.open_session store ~sw ~actor_lock_nonce:"pending-moderated-child" child_id
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    |> protocol_ok
  in
  Exn.protect
    ~finally:(fun () ->
      Store.close_session store handle
      |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      |> protocol_ok)
    ~f:(fun () ->
      Factory.read_owned_session (Daemon.factory daemon) handle |> protocol_ok)
;;

let initial_receipts (current : Agent_session.Session_state.t) =
  List.filter current.moderator_executions ~f:(fun receipt ->
    P.Moderator_execution.equal_phase receipt.context.phase Session_start)
;;

let await_initial_receipt entry ~script_id =
  let module Receipt = P.Moderator_execution in
  let rec await () =
    let current = state entry in
    match initial_receipts current with
    | [] | [ { status = Running; _ } ] ->
      Eio.Fiber.yield ();
      await ()
    | [ ({ status = Completed _; _ } as receipt) ] ->
      assert (P.Id.Session.equal receipt.context.session_id current.identity.session_id);
      assert (Option.is_none receipt.context.operation_id);
      assert (Option.is_none receipt.delegation);
      let checkpoint =
        Agent_session.Moderator_checkpoint.decode current.moderator
        |> protocol_ok
        |> Option.value_exn
      in
      [%test_eq: string] script_id checkpoint.script_id;
      [%test_eq: string] checkpoint.script_id receipt.context.source.script_id;
      [%test_eq: string]
        checkpoint.script_source_hash
        receipt.context.source.source_sha256;
      (match checkpoint.current_state with
       | Int 1 -> ()
       | _ -> failwith "initial moderator checkpoint did not advance exactly once");
      receipt
    | receipts ->
      raise_s
        [%sexp
          "expected one completed recovered Session_start", (receipts : Receipt.t list)]
  in
  await ()
;;

let reconcile_unlinked_child
      env
      daemon
      ~(parent : Registry.entry)
      ~definition
      ~read_record
      (record : D.record)
  =
  let module Store = Agent_store.Session_store in
  let module Artifact = Agent_store.Prompt_artifact_store in
  let factory = Daemon.factory daemon in
  let registry = Daemon.registry daemon in
  let store = Daemon.store daemon in
  let child_id = record.admission.child_session_id in
  let parent_before = state parent in
  assert (P.Session.equal_desired_state parent_before.lifecycle.desired Running);
  assert (Owner.is_loaded parent.runtime);
  let child () = Registry.find registry child_id |> Option.value_exn in
  Factory.reconcile_generated_creations factory |> protocol_ok;
  let linked = read_record () in
  assert (D.equal_stage linked.D.stage Linked);
  assert (Option.is_none linked.revocation);
  assert (D.Reference.equal (D.reference record) (D.reference linked));
  let original_artifact = Agent_session.Generated_definition.artifact definition in
  let artifacts =
    Artifact.open_existing
      ~env
      ~root:(Agent_store.Data_root.prompt_artifacts_path (Store.data_root store))
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    |> protocol_ok
  in
  let installed_artifact =
    Artifact.load artifacts linked.admission.revision_id
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
    |> protocol_ok
  in
  [%test_eq: string] installed_artifact.manifest_sha256 original_artifact.manifest_sha256;
  let published = child () in
  let current = state published in
  let handle = Option.value_exn published.store_handle in
  assert (Store.owns_handle store handle);
  assert (P.Id.Session.equal (Store.Handle.session_id handle) child_id);
  assert (Owner.belongs_to_actor published.runtime published.actor);
  assert (P.Session.equal_desired_state current.lifecycle.desired Stopped);
  assert current.pending_initial_start;
  assert (not (Owner.is_loaded published.runtime));
  assert (Option.is_none current.moderator);
  assert (List.is_empty current.moderator_executions);
  assert (
    Option.equal D.Reference.equal current.spec.delegation (Some (D.reference linked)));
  Factory.reconcile_generated_creations factory |> protocol_ok;
  assert (D.equal_record linked (read_record ()));
  let repeated = child () in
  assert (phys_equal published.actor repeated.actor);
  assert (phys_equal published.runtime repeated.runtime);
  assert (Option.equal phys_equal published.store_handle repeated.store_handle);
  [%test_eq: int64] current.counters.revision (state repeated).counters.revision;
  assert (
    List.equal
      P.Moderator_execution.equal
      parent_before.moderator_executions
      (state parent).moderator_executions);
  [%test_eq: int]
    2
    (Store.list_sessions_checked store
     |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
     |> protocol_ok
     |> List.length);
  linked
;;

let recover_initial_start ~moderated ~lost_stage =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let timeout = if moderated then 45. else 15. in
        let prompt_file = Filename.concat root "parent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          ({|<developer>Parent.</developer><tool name="read_file"><read id="data" path="${workspace}"/></tool>|}
           ^ if moderated then initial_start_moderator "cold_parent" else "");
        let base = config root root prompt_file in
        let config =
          { base with
            workspaces =
              List.map base.workspaces ~f:(fun workspace ->
                { workspace with
                  source =
                    Temporary
                      { location = Session_dir
                      ; cleanup = On_session_stop
                      ; managed_root = None
                      }
                })
          }
        in
        let release, release_u = Eio.Promise.create () in
        let clock_started, clock_started_u = Eio.Promise.create () in
        let module Clock = struct
          type t = unit
          type time = float

          let now () = Eio.Time.now (Eio.Stdenv.clock env)

          let sleep_until () time =
            if Option.is_none (Eio.Promise.peek clock_started)
            then Eio.Promise.resolve clock_started_u ();
            Eio.Promise.await release;
            Eio.Time.sleep_until (Eio.Stdenv.clock env) time
          ;;
        end
        in
        let clock = Eio.Resource.T ((), Eio.Time.Pi.clock (module Clock)) in
        let providers = ref 0 in
        let armed = ref false in
        let reached target =
          if
            !armed
            && String.is_substring target ~substring:"/delegations/"
            && String.is_suffix target ~suffix:".frame"
          then (
            let contents = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / target) in
            let payload =
              match
                Agent_store.Frame.decode ~max_payload_length:262144 ~contents ~offset:0
              with
              | Ok (Complete { frame; _ }) -> Agent_store.Frame.payload frame
              | _ -> failwith "invalid delegation frame"
            in
            if D.equal_stage (delegation_stage payload) lost_stage
            then (
              armed := false;
              raise
                (Core_unix.Unix_error
                   (EIO, "injected lost delegation acknowledgement", target))))
        in
        let directory, fs_path = Eio.Stdenv.fs env in
        let observed_env =
          object
            method fs = observe_rename directory reached, fs_path
            method cwd = env#cwd
            method stdin = env#stdin
            method stdout = env#stdout
            method stderr = env#stderr
            method net = env#net
            method domain_mgr = env#domain_mgr
            method process_mgr = env#process_mgr
            method clock = clock
            method mono_clock = env#mono_clock
            method secure_random = env#secure_random
            method debug = env#debug
            method backend_id = env#backend_id
          end
        in
        Eio.Switch.run (fun sw ->
          let daemon =
            Daemon.start
              ~sw
              ~env:observed_env
              ~config
              ~tool_dir:root
              ~home:root
              ~process_start_identity:None
              ~options:
                { Daemon.default_options with
                  qualify_chatml_extensions = true
                ; independent_lifetime_policy = Some "pending-v1"
                ; inference_policy =
                    Agent_server_test_support.inference_policy
                      ~default_model:"fixture-model"
                      ~post_stream:(fun ~sw:_ ~inputs:_ ->
                        Int.incr providers;
                        failwith "unexpected provider call")
                }
              ()
            |> protocol_ok
          in
          Exn.protect
            ~finally:(fun () ->
              Eio.Promise.resolve release_u ();
              Daemon.shutdown daemon |> protocol_ok)
            ~f:(fun () ->
              Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) timeout (fun () ->
                let client = connection daemon (principal ()) in
                Exn.protect
                  ~finally:(fun () -> Agent_client.Connection.close client)
                  ~f:(fun () ->
                    initialize client;
                    Eio.Promise.await clock_started;
                    let parent, _ = create_session ~start_immediately:true client in
                    let entry =
                      Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
                    in
                    let workspace =
                      (state entry).spec.workspace_instance.canonical_root.native_path
                    in
                    let marker =
                      Eio.Path.(Eio.Stdenv.fs env / workspace / "retained.txt")
                    in
                    Eio.Path.save ~create:(`Exclusive 0o600) marker "pending-child";
                    let moderator =
                      if moderated then initial_start_moderator "recovered_child" else ""
                    in
                    let definition = prepare ~moderator observed_env root entry in
                    if moderated
                    then
                      ignore
                        (await_initial_receipt entry ~script_id:"cold_parent"
                         : P.Moderator_execution.t);
                    armed := true;
                    (match
                       create
                         daemon
                         entry
                         definition
                         ~key:
                           (if D.equal_stage lost_stage Linked
                            then "lost-linked"
                            else "lost-child-installed")
                         ~lifetime:Independent
                     with
                     | Error { code = Persistence_error; _ } -> ()
                     | Error error -> raise_s [%sexp (error : P.Error.t)]
                     | Ok _ -> failwith "creation did not lose its stage acknowledgement");
                    let read_record () =
                      let records =
                        D.with_records
                          (Agent_store.Session_store.delegations (Daemon.store daemon))
                          ~max_records:8
                          ~max_bytes:1048576
                          ~f:Result.return
                        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                        |> protocol_ok
                      in
                      match records with
                      | [ record ] -> record
                      | _ -> failwith "expected one durable child admission"
                    in
                    let record = read_record () in
                    assert (D.equal_stage record.stage lost_stage);
                    assert (Option.is_none record.revocation);
                    (match record.admission.lifetime with
                     | Independent _ -> ()
                     | Owned | Invocation_owned _ -> failwith "expected Independent grant");
                    let indexed =
                      Agent_store.Session_index.find
                        (Agent_store.Session_store.session_index (Daemon.store daemon))
                        record.admission.child_session_id
                      |> Option.value_exn
                    in
                    assert indexed.pending_initial_start;
                    assert (
                      Option.is_none
                        (Registry.find
                           (Daemon.registry daemon)
                           record.admission.child_session_id));
                    if moderated
                    then (
                      assert (not !armed);
                      let unpublished =
                        read_unpublished daemon ~sw record.admission.child_session_id
                      in
                      assert unpublished.pending_initial_start;
                      assert (
                        Option.equal
                          D.Reference.equal
                          unpublished.spec.delegation
                          (Some (D.reference record)));
                      assert (Option.is_none unpublished.moderator);
                      assert (List.is_empty unpublished.moderator_executions);
                      assert (Option.is_none unpublished.active_operation);
                      assert (
                        P.Session.equal_desired_state
                          unpublished.lifecycle.desired
                          Stopped));
                    let record =
                      if D.equal_stage lost_stage Child_installed
                      then
                        reconcile_unlinked_child
                          env
                          daemon
                          ~parent:entry
                          ~definition
                          ~read_record
                          record
                      else record
                    in
                    let handle =
                      H.attach
                        ~sw
                        ~clock:(Eio.Stdenv.clock env)
                        ~connection:client
                        ~session_id:parent.id
                        ~mode:Read_write
                        ~subscribe:false
                        ()
                      |> protocol_ok
                    in
                    H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
                    let stopped_parent = state entry in
                    [%test_eq: string] "pending-child" (Eio.Path.load marker);
                    (match
                       H.reset
                         handle
                         ~expected_revision:(state entry).counters.revision
                         ~keep_history:true
                         ~keep_tasks:true
                         ~keep_cache:false
                         ~keep_workspace:false
                         ~keep_grants:true
                         ~keep_labels:true
                     with
                     | Error { code = Conflict; _ } -> ()
                     | _ -> failwith "reset removed resources of a linked pending child");
                    H.close handle;
                    Agent_client.Connection.close client;
                    if moderated then Owner.unload_and_wait entry.runtime |> protocol_ok;
                    ignore
                      (Registry.unload_inactive
                         (Daemon.registry daemon)
                         ~index_entries:
                           (Agent_store.Session_store.list_sessions (Daemon.store daemon))
                       : int);
                    if moderated
                    then
                      assert (
                        Option.is_none (Registry.find (Daemon.registry daemon) parent.id));
                    Factory.resume_generated_initial_starts (Daemon.factory daemon);
                    let child =
                      Registry.find
                        (Daemon.registry daemon)
                        record.admission.child_session_id
                      |> Option.value_exn
                    in
                    let current = state child in
                    assert (not current.pending_initial_start);
                    assert (
                      P.Session.equal_desired_state current.lifecycle.desired Running);
                    assert (Option.is_none current.failure);
                    assert (Owner.is_loaded child.runtime);
                    let parent =
                      Registry.find (Daemon.registry daemon) parent.id |> Option.value_exn
                    in
                    assert (not (Owner.is_loaded parent.runtime));
                    if moderated
                    then (
                      let receipt =
                        await_initial_receipt child ~script_id:"recovered_child"
                      in
                      let recovered_parent = state parent in
                      assert (
                        P.Session.equal_desired_state
                          recovered_parent.lifecycle.desired
                          Stopped);
                      assert (
                        List.equal
                          P.Moderator_execution.equal
                          stopped_parent.moderator_executions
                          recovered_parent.moderator_executions);
                      assert (not (Owner.is_loaded parent.runtime));
                      let current = state child in
                      let reference = Option.value_exn current.spec.delegation in
                      let persisted =
                        D.resolve
                          (Agent_store.Session_store.delegations (Daemon.store daemon))
                          reference
                        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
                        |> protocol_ok
                      in
                      assert (D.equal_record record persisted);
                      Factory.resume_generated_initial_starts (Daemon.factory daemon);
                      assert (
                        List.equal
                          P.Moderator_execution.equal
                          [ receipt ]
                          (initial_receipts (state child)));
                      [%test_eq: string] "pending-child" (Eio.Path.load marker);
                      [%test_eq: int] 0 !providers)))))))
;;

let%expect_test "a linked initial start retains its workspace before resource admission" =
  recover_initial_start ~moderated:false ~lost_stage:Linked;
  print_endline
    "lost linked acknowledgement preserves pending child roots through parent stop, \
     eviction and initial activation";
  [%expect
    {| lost linked acknowledgement preserves pending child roots through parent stop, eviction and initial activation |}]
;;

let%expect_test
    "moderated Independent initial start recovers a lost linked acknowledgement"
  =
  recover_initial_start ~moderated:true ~lost_stage:Linked;
  print_endline
    "The linked child ran its initial moderator once after the stopped parent was \
     evicted.";
  recover_initial_start ~moderated:true ~lost_stage:Child_installed;
  print_endline
    "Reconciliation linked the existing child before its initial moderator ran once.";
  [%expect
    {|
    The linked child ran its initial moderator once after the stopped parent was evicted.
    Reconciliation linked the existing child before its initial moderator ran once.
    |}]
;;

let%expect_test "independent ancestry retains temporary roots across stop and restart" =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root = temporary_root env in
    Exn.protect
      ~finally:(fun () ->
        Eio.Path.rmtree ~missing_ok:true Eio.Path.(Eio.Stdenv.fs env / root))
      ~f:(fun () ->
        let prompt_file = Filename.concat root "parent.chatmd" in
        Eio.Path.save
          ~create:(`Exclusive 0o600)
          Eio.Path.(Eio.Stdenv.fs env / prompt_file)
          {|<developer>Parent.</developer><tool name="read_file"><read id="data" path="${workspace}"/></tool><tool name="meta_refine"/>|};
        let base = config root root prompt_file in
        let configuration =
          { base with
            (* Use event-count snapshots in this ancestry fixture. A wall-time
               snapshot can reserve the actor between its explicit lifecycle requests. *)
            server =
              { base.server with
                durability =
                  { base.server.durability with snapshot_every_ms = Int.max_value }
              }
          ; workspaces =
              List.map base.workspaces ~f:(fun workspace ->
                { workspace with
                  source =
                    Temporary
                      { location = Session_dir
                      ; cleanup = On_session_stop
                      ; managed_root = None
                      }
                })
          }
        in
        let requests = ref 0 in
        let independent_source = ref None in
        let projection_marker = ref None in
        let reject_projection = ref false in
        let projection_rejections = ref 0 in
        let cache_publications = ref 0 in
        let prepare env root parent =
          prepare ~inherited_tools:[ "read_file"; "meta_refine" ] env root parent
        in
        let directory, fs_path = Eio.Stdenv.fs env in
        let observed_directory =
          observe_rename
            directory
            ~before_stat:(fun path ->
              if
                !reject_projection
                && Option.exists !projection_marker ~f:(String.equal path)
              then (
                Int.incr projection_rejections;
                raise (Core_unix.Unix_error (EIO, "648 resource graph preflight", path))))
            (fun path ->
               if String.equal (Filename.basename path) "cache.bin"
               then Int.incr cache_publications)
        in
        let observed_env =
          object
            method fs = observed_directory, fs_path
            method cwd = env#cwd
            method stdin = env#stdin
            method stdout = env#stdout
            method stderr = env#stderr
            method net = env#net
            method domain_mgr = env#domain_mgr
            method process_mgr = env#process_mgr
            method clock = env#clock
            method mono_clock = env#mono_clock
            method secure_random = env#secure_random
            method debug = env#debug
            method backend_id = env#backend_id
          end
        in
        let post_stream ~sw:_ ~inputs =
          Int.incr requests;
          match !independent_source with
          | Some primary ->
            reject_projection := true;
            raise primary
          | None ->
            (match List.last inputs with
             | Some (Openai.Responses.Item.Function_call_output _) -> Stdlib.Seq.empty
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
                   { arguments = {|{"root":"data","file":"value.txt"}|}
                   ; item_id = "read-item"
                   ; output_index = 0
                   ; type_ = "response.function_call_arguments.done"
                   }
               ; Output_item_done
                   { item =
                       Function_call
                         { name = "read_file"
                         ; arguments = {|{"root":"data","file":"value.txt"}|}
                         ; call_id = sprintf "read-%d" !requests
                         ; _type = "function_call"
                         ; id = Some "read-item"
                         ; status = Some "completed"
                         }
                   ; output_index = 0
                   ; type_ = "response.output_item.done"
                   }
               ]
               |> Stdlib.List.to_seq)
        in
        (* Count real graph/native preparation at the existing host port. A
           stopped-ancestor read alone already passes the reconstructed route. *)
        let preparations = ref [] in
        let preparation_count entry =
          List.find_map !preparations ~f:(fun (actor, count) ->
            if phys_equal actor entry.Registry.actor then Some count else None)
          |> Option.value ~default:0
        in
        let with_daemon policy f =
          Eio.Switch.run (fun sw ->
            let inference_policy =
              Agent_server_test_support.inference_policy
                ~default_model:"fixture-model"
                ~post_stream
            in
            let inference_policy =
              { inference_policy with
                runtime_inference_ports =
                  (fun actor ->
                    (* The host callback may not reenter the Actor. Count its
                       SAME actual object; the entry associates it with its ID. *)
                    let count =
                      List.find_map !preparations ~f:(fun (actual, count) ->
                        if phys_equal actor actual then Some count else None)
                      |> Option.value ~default:0
                    in
                    preparations
                    := (actor, count + 1)
                       :: List.filter !preparations ~f:(fun (actual, _) ->
                         not (phys_equal actor actual));
                    inference_policy.runtime_inference_ports actor)
              }
            in
            let daemon =
              Daemon.start
                ~sw
                ~env:observed_env
                ~config:configuration
                ~tool_dir:root
                ~home:root
                ~process_start_identity:None
                ~options:
                  { Daemon.default_options with
                    qualify_chatml_extensions = true
                  ; independent_lifetime_policy = policy
                  ; inference_policy
                  }
                ()
              |> protocol_ok
            in
            Exn.protect
              ~finally:(fun () -> Daemon.shutdown daemon |> protocol_ok)
              ~f:(fun () ->
                Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
                  let client = connection daemon (principal ()) in
                  Exn.protect
                    ~finally:(fun () -> Agent_client.Connection.close client)
                    ~f:(fun () ->
                      initialize client;
                      f sw daemon client))))
        in
        let attach sw client entry =
          H.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection:client
            ~session_id:(id entry)
            ~mode:Read_write
            ~subscribe:false
            ()
          |> protocol_ok
        in
        let find daemon session_id =
          Registry.find (Daemon.registry daemon) session_id |> Option.value_exn
        in
        let rec idle entry =
          let current = state entry in
          match current.active_operation with
          | None -> current
          | Some _ ->
            Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
            idle entry
        in
        let read handle entry expected =
          let previous =
            List.map (state entry).invocations ~f:(fun invocation ->
              invocation.P.Invocation.context.id)
          in
          H.send_message
            handle
            { kind = Plain_text; text = "Read value.txt."; attachments = [] }
          |> protocol_ok
          |> ignore;
          let current = idle entry in
          let fresh =
            List.filter current.invocations ~f:(fun invocation ->
              not
                (List.mem
                   previous
                   invocation.P.Invocation.context.id
                   ~equal:P.Id.Invocation.equal))
          in
          match fresh with
          | [ { status = Published (Complete (`String text)); _ } ] ->
            assert (String.is_substring text ~substring:expected)
          | _ ->
            raise_s
              [%sexp "expected one new successful read", (fresh : P.Invocation.t list)]
        in
        let running entry =
          assert (P.Session.equal_desired_state (state entry).lifecycle.desired Running)
        in
        let stopped entry =
          assert (P.Session.equal_desired_state (state entry).lifecycle.desired Stopped)
        in
        with_daemon None (fun sw daemon client ->
          let parent, _ =
            create_session ~key:"default-denial" ~start_immediately:true client
          in
          let entry = find daemon parent.id in
          let definition = prepare env root entry in
          (match create daemon entry definition ~key:"denied" ~lifetime:Independent with
           | Error { code = Permission_denied; _ } -> ()
           | _ -> failwith "ungranted independent lifetime was admitted");
          [%test_eq: int]
            1
            (List.length (Agent_store.Session_store.list_sessions (Daemon.store daemon)));
          let handle = attach sw client entry in
          H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
          H.close handle);
        let parent_id, middle_id, child_id, leaf_id, workspace =
          with_daemon (Some "fixture-v1") (fun sw daemon client ->
            let parent, _ =
              create_session ~key:"independent-root" ~start_immediately:true client
            in
            let parent = find daemon parent.id in
            let parent_handle = attach sw client parent in
            let workspace =
              (state parent).spec.workspace_instance.canonical_root.native_path
            in
            Eio.Path.save
              ~create:(`Exclusive 0o600)
              Eio.Path.(Eio.Stdenv.fs env / workspace / "value.txt")
              "before-restart";
            let middle =
              create daemon parent (prepare env root parent) ~key:"middle" ~lifetime:Owned
              |> protocol_ok
            in
            projection_marker
            := Some
                 (Filename.concat
                    (Agent_store.Data_root.indexes_path
                       (Agent_store.Session_store.data_root (Daemon.store daemon)))
                    "sessions.recovery-required");
            (* This fresh Running sibling exercises an actual installed inherited
               implementation; the original stopped-ancestry scenario below is
               unchanged. Cleanup runs after the borrower has unwound. *)
            let probe =
              create
                daemon
                middle
                (prepare env root middle)
                ~key:"independent-graph-retry"
                ~lifetime:Independent
              |> protocol_ok
            in
            let probe_handle = attach sw client probe in
            let module B = Agent_session.Runtime_builder in
            let module CO = Agent_session.Close_observation in
            let module CD = Agent_session.Cleanup_diagnostic in
            let module L = Agent_session.Inference_ledger in
            let module O = Inference.Observation in
            let before_rows = L.rows (state parent).inference_ledger in
            let fresh_rows () =
              L.rows (state parent).inference_ledger
              |> List.filter ~f:(fun row ->
                not
                  (List.exists before_rows ~f:(fun previous ->
                     L.Handle.equal (L.Row.handle previous) (L.Row.handle row))))
            in
            let saved_runtime = ref None in
            let source_failure = Failure "648 actual Independent provider primary" in
            independent_source := Some source_failure;
            let requests_before = !requests in
            let recording = Stdlib.Printexc.backtrace_status () in
            Stdlib.Printexc.record_backtrace true;
            Exn.protect
              ~finally:(fun () -> Stdlib.Printexc.record_backtrace recording)
              ~f:(fun () ->
                Exn.protect
                  ~finally:(fun () ->
                    reject_projection := false;
                    independent_source := None;
                    Exn.protect
                      ~finally:(fun () -> H.close probe_handle)
                      ~f:(fun () ->
                        Exn.protect
                          ~finally:(fun () ->
                            H.stop probe_handle ~mode:Cancel |> protocol_ok |> ignore)
                          ~f:(fun () ->
                            Owner.unload_and_wait probe.runtime |> protocol_ok)))
                  ~f:(fun () ->
                    Owner.with_loaded_runtime probe.runtime (fun runtime ->
                      saved_runtime := Some runtime;
                      let native = Option.value_exn runtime.B.native_runtime in
                      let tool =
                        List.find_exn native.functions ~f:(fun tool ->
                          String.equal
                            tool.Ochat_function.info.function_.name
                            "meta_refine")
                      in
                      Exn.protect
                        ~finally:(fun () ->
                          (* The marker is store-wide. Never leave it armed across
                         this child's unrelated borrower finalization. *)
                          reject_projection := false;
                          independent_source := None)
                        ~f:(fun () ->
                          match
                            tool.run
                              {|{"prompt":"","task":"One bounded resource attempt."}|}
                          with
                          | _ ->
                            failwith "648 installed tool lost its actual source failure"
                          | exception exn ->
                            assert (phys_equal exn source_failure);
                            assert (
                              not (String.is_empty (Stdlib.Printexc.get_backtrace ()))));
                      Ok ())
                    |> protocol_ok;
                    [%test_eq: int] (requests_before + 1) !requests;
                    assert (!projection_rejections > 0);
                    let row = List.hd_exn (fresh_rows ()) in
                    [%test_eq: int] 1 (List.length (fresh_rows ()));
                    let handle = L.Row.handle row in
                    assert (P.Id.Session.equal (id parent) (L.Handle.session_id handle));
                    let assert_same_running () =
                      let row =
                        L.find
                          (state parent).inference_ledger
                          ~ordinal:(L.Handle.ordinal handle)
                        |> Option.value_exn
                      in
                      assert (L.Handle.equal handle (L.Row.handle row));
                      match O.Attempt_record.state (L.Row.record row) with
                      | Running -> ()
                      | Prepared | Terminal _ | Interrupted _ ->
                        failwith
                          "648 failed resource graph cleanup settled the retained row"
                    in
                    assert_same_running ();
                    (* This is now the canonical shared R graph. Drop the
                       ordinary M/R Product baselines without Stop/epoch changes;
                       the actual Independent probe loan keeps both native units
                       alive until its saved runtime performs the final release. *)
                    Owner.unload_and_wait middle.runtime |> protocol_ok;
                    Owner.unload_and_wait parent.runtime |> protocol_ok;
                    assert (not (Owner.is_loaded middle.runtime));
                    assert (not (Owner.is_loaded parent.runtime));
                    running parent;
                    running middle;
                    running probe;
                    assert_same_running ();
                    let runtime = Option.value_exn !saved_runtime in
                    let publications_before = !cache_publications in
                    reject_projection := true;
                    let first =
                      Exn.protect
                        ~finally:(fun () -> reject_projection := false)
                        ~f:(fun () -> B.close_reported runtime)
                    in
                    (match CO.exit first with
                     | Raised _ -> ()
                     | Returned () ->
                       failwith "648 actual graph close failure became success");
                    let primary = CO.primary first |> Option.value_exn in
                    let marker = Option.value_exn !projection_marker in
                    let expected_graph_error = function
                      | Agent_session.Callback_cleanup.Failure.Protocol error ->
                        P.Error.equal_code error.code Persistence_error
                        && String.is_substring error.message ~substring:marker
                        && String.is_substring
                             error.message
                             ~substring:"648 resource graph preflight"
                      | Raised _ -> false
                    in
                    assert (List.exists (CD.failures primary) ~f:expected_graph_error);
                    let first_groups = CO.diagnostics first in
                    let first_exit = CO.exit first in
                    assert_same_running ();
                    let publications_after_first = !cache_publications in
                    assert (publications_after_first > publications_before);
                    (* A second real persistence failure must replace current G
                       with authentic H while preserving the old report/aliases. *)
                    reject_projection := true;
                    let second =
                      Exn.protect
                        ~finally:(fun () -> reject_projection := false)
                        ~f:(fun () -> B.close_reported runtime)
                    in
                    (match CO.exit second with
                     | Raised _ -> ()
                     | Returned () -> failwith "639 failed outer retry became success");
                    let second_primary = CO.primary second |> Option.value_exn in
                    assert (not (CD.equal primary second_primary));
                    assert (
                      List.exists (CD.failures second_primary) ~f:expected_graph_error);
                    let second_groups = CO.diagnostics second in
                    let second_exit = CO.exit second in
                    assert (
                      List.for_all first_groups ~f:(fun original ->
                        List.mem second_groups original ~equal:CD.equal));
                    assert_same_running ();
                    [%test_eq: int] publications_after_first !cache_publications;
                    [%test_eq: int] (requests_before + 1) !requests;
                    let healed = B.close_reported runtime in
                    (match CO.exit healed with
                     | Returned () -> ()
                     | Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace);
                    assert (Option.is_none (CO.primary healed));
                    [%test_eq: int] publications_after_first !cache_publications;
                    [%test_eq: int] (requests_before + 1) !requests;
                    let ended =
                      L.find
                        (state parent).inference_ledger
                        ~ordinal:(L.Handle.ordinal handle)
                      |> Option.value_exn
                    in
                    assert (L.Handle.equal handle (L.Row.handle ended));
                    (match O.Attempt_record.state (L.Row.record ended) with
                     | Interrupted { reason = Host_interrupted; _ } -> ()
                     | Prepared | Running | Terminal _ | Interrupted _ ->
                       failwith "648 exact resource graph was not factually interrupted");
                    let repeated = B.close_reported runtime in
                    (match CO.exit repeated with
                     | Returned () -> ()
                     | Raised (exn, trace) -> Exn.raise_with_original_backtrace exn trace);
                    assert (Option.is_none (CO.primary repeated));
                    assert (
                      List.equal
                        CD.equal
                        (CO.diagnostics healed)
                        (CO.diagnostics repeated));
                    [%test_eq: int] publications_after_first !cache_publications;
                    [%test_eq: int] (requests_before + 1) !requests;
                    assert (List.equal CD.equal second_groups (CO.diagnostics second));
                    assert (CD.equal second_primary (CO.primary second |> Option.value_exn));
                    (match second_exit, CO.exit second with
                     | Raised (exn, trace), Raised (actual, actual_trace) ->
                       assert (phys_equal exn actual);
                       assert (phys_equal trace actual_trace)
                     | Returned (), Returned ()
                     | Raised _, Returned ()
                     | Returned (), Raised _ ->
                       failwith "639 retained current failure changed after later repair");
                    assert (List.equal CD.equal first_groups (CO.diagnostics first));
                    assert (CD.equal primary (CO.primary first |> Option.value_exn));
                    match first_exit, CO.exit first with
                    | Raised (exn, trace), Raised (actual, actual_trace) ->
                      assert (phys_equal exn actual);
                      assert (phys_equal trace actual_trace)
                    | Returned (), Returned ()
                    | Raised _, Returned ()
                    | Returned (), Raised _ ->
                      failwith "648 original resource-close report changed"));
            (* Restore ordinary construction sources after the exact shared
               graph was repaired. Main C/D reuse starts with fresh baselines;
               no logical Stop or Owned parent epoch changed in the fault probe. *)
            Owner.ensure_loaded parent.runtime |> protocol_ok;
            Owner.ensure_loaded middle.runtime |> protocol_ok;
            assert (Owner.is_loaded parent.runtime);
            assert (Owner.is_loaded middle.runtime);
            running parent;
            running middle;
            let child_definition = prepare env root middle in
            (* Birth needs the actual Running parent. D's first native admission
               is deliberately deferred until C has stopped and released its use. *)
            let next_consumer =
              Factory.create_generated_session
                ~start_immediately:false
                ~lifetime:Independent
                (Daemon.factory daemon)
                ~parent_session_id:(id middle)
                ~idempotency_key:
                  (P.Idempotency_key.of_string "reuse-next-consumer" |> protocol_ok)
                ~display_name:None
                child_definition
              |> protocol_ok
            in
            stopped next_consumer;
            assert (not (Owner.is_loaded next_consumer.runtime));
            [%test_eq: int] 0 (preparation_count next_consumer);
            (* Generated creation starts protocol-ready; readiness does not mean
               a native unit exists. The actual construction count above and the
               unloaded Owner distinguish this deferred first admission. *)
            let child =
              create daemon middle child_definition ~key:"child" ~lifetime:Independent
              |> protocol_ok
            in
            let replay =
              create daemon middle child_definition ~key:"child" ~lifetime:Independent
              |> protocol_ok
            in
            assert (P.Id.Session.equal (id child) (id replay));
            (match create daemon middle child_definition ~key:"child" ~lifetime:Owned with
             | Error { code = Conflict; _ } -> ()
             | _ -> failwith "lifetime change did not conflict with the retained key");
            let leaf =
              create daemon child (prepare env root child) ~key:"leaf" ~lifetime:Owned
              |> protocol_ok
            in
            let child_handle = attach sw client child in
            let leaf_handle = attach sw client leaf in
            H.stop parent_handle ~mode:Cancel |> protocol_ok |> ignore;
            stopped parent;
            stopped middle;
            running child;
            running leaf;
            assert (not (Owner.is_loaded parent.runtime));
            assert (not (Owner.is_loaded middle.runtime));
            read child_handle child "before-restart";
            read leaf_handle leaf "before-restart";
            let module Caps = Chat_response.Tool_capability in
            let inherited_read entry =
              Owner.with_loaded_runtime entry.Registry.runtime (fun runtime ->
                let native =
                  Option.value_exn runtime.Agent_session.Runtime_builder.native_runtime
                in
                let capabilities =
                  Lazy.force native.capabilities
                  |> Result.map_error ~f:(fun error -> error.Caps.message)
                  |> Result.ok_or_failwith
                in
                let binding =
                  Caps.find capabilities ~name:"read_file"
                  |> Result.map_error ~f:(fun error -> error.Caps.message)
                  |> Result.ok_or_failwith
                in
                Ok
                  ( Caps.reference binding
                  , Caps.native_implementation binding |> Option.value_exn ))
              |> protocol_ok
            in
            let first_reference, first_implementation = inherited_read child in
            let root_preparations = preparation_count parent
            and middle_preparations = preparation_count middle in
            let root_source = (state parent).spec
            and middle_source = (state middle).spec in
            let before_owned_denial = state middle in
            let before_owned_requests = !requests in
            (match Factory.prepare_session_start (Daemon.factory daemon) middle with
             | Error { code = Permission_denied; message; _ }
               when String.is_prefix message ~prefix:"delegation.parent_authority:" -> ()
             | Error error ->
               raise_s [%sexp "639 unexpected Owned denial", (error : P.Error.t)]
             | Ok () -> failwith "639 ordinary M used stopped R's authority");
            [%test_eq: int] before_owned_requests !requests;
            [%test_eq: int64]
              before_owned_denial.counters.revision
              (state middle).counters.revision;
            (* D must enter only after C closes, while a genuine other holder
               keeps the canonical unit alive. Reuse the already stopped
               Independent probe; its new admission has its own actual path. *)
            let holder_handle = attach sw client probe in
            Exn.protect
              ~finally:(fun () ->
                Eio.Cancel.protect (fun () ->
                  Exn.protect
                    ~finally:(fun () -> H.close holder_handle)
                    ~f:(fun () ->
                      Exn.protect
                        ~finally:(fun () ->
                          Owner.unload_and_wait probe.runtime |> protocol_ok)
                        ~f:(fun () ->
                          H.stop holder_handle ~mode:Cancel |> protocol_ok |> ignore))))
              ~f:(fun () ->
                H.start holder_handle ~queue_if_limited:false |> protocol_ok |> ignore;
                let holder_reference, holder_implementation = inherited_read probe in
                assert (phys_equal first_implementation holder_implementation);
                assert (P.Id.Capability.equal first_reference.id holder_reference.id);
                [%test_eq: string]
                  first_reference.fingerprint
                  holder_reference.fingerprint;
                [%test_eq: string]
                  first_reference.implementation_revision
                  holder_reference.implementation_revision;
                [%test_eq: int] 0 (preparation_count next_consumer);
                let next_handle = attach sw client next_consumer in
                Exn.protect
                  ~finally:(fun () ->
                    H.stop next_handle ~mode:Cancel |> protocol_ok |> ignore;
                    Owner.unload_and_wait next_consumer.runtime |> protocol_ok;
                    H.close next_handle)
                  ~f:(fun () ->
                    H.stop child_handle ~mode:Cancel |> protocol_ok |> ignore;
                    stopped child;
                    stopped leaf;
                    Owner.unload_and_wait child.runtime |> protocol_ok;
                    assert (not (Owner.is_loaded child.runtime));
                    [%test_eq: int] 0 (preparation_count next_consumer);
                    H.start next_handle ~queue_if_limited:false |> protocol_ok |> ignore;
                    read next_handle next_consumer "before-restart";
                    assert (preparation_count next_consumer > 0);
                    let next_reference, next_implementation =
                      inherited_read next_consumer
                    in
                    assert (phys_equal first_implementation next_implementation);
                    assert (P.Id.Capability.equal first_reference.id next_reference.id);
                    [%test_eq: string]
                      first_reference.fingerprint
                      next_reference.fingerprint;
                    [%test_eq: string]
                      first_reference.implementation_revision
                      next_reference.implementation_revision;
                    [%test_eq: int] root_preparations (preparation_count parent);
                    [%test_eq: int] middle_preparations (preparation_count middle);
                    assert (
                      Agent_session.Session_state.Spec.Resource_identity.equal
                        (Agent_session.Session_state.Spec.Resource_identity.capture
                           root_source)
                        (Agent_session.Session_state.Spec.Resource_identity.capture
                           (state parent).spec));
                    assert (
                      Agent_session.Session_state.Spec.Resource_identity.equal
                        (Agent_session.Session_state.Spec.Resource_identity.capture
                           middle_source)
                        (Agent_session.Session_state.Spec.Resource_identity.capture
                           (state middle).spec));
                    stopped parent;
                    stopped middle;
                    assert (not (Owner.is_loaded parent.runtime));
                    assert (not (Owner.is_loaded middle.runtime))));
            (* Restore the original child/leaf trajectory before its existing
               restart, grant, eviction and workspace assertions. *)
            H.start child_handle ~queue_if_limited:false |> protocol_ok |> ignore;
            H.start leaf_handle ~queue_if_limited:false |> protocol_ok |> ignore;
            running child;
            running leaf;
            read child_handle child "before-restart";
            read leaf_handle leaf "before-restart";
            let before = state parent in
            let reset =
              Agent_client.Connection.request
                client
                (Session_reset
                   { session_id = id parent
                   ; attachment_id = (H.attachment parent_handle).id
                   ; expected_revision = before.counters.revision
                   ; keep_history = true
                   ; keep_tasks = true
                   ; keep_cache = false
                   ; keep_workspace = false
                   ; keep_grants = true
                   ; keep_labels = true
                   ; idempotency_key =
                       P.Idempotency_key.of_string "retained-reset" |> protocol_ok
                   })
            in
            (match reset with
             | Error { code = Conflict; _ } -> ()
             | _ -> failwith "reset changed retained roots");
            H.close parent_handle;
            H.close child_handle;
            H.close leaf_handle;
            Agent_client.Connection.close client;
            ignore
              (Registry.unload_inactive
                 (Daemon.registry daemon)
                 ~index_entries:
                   (Agent_store.Session_store.list_sessions (Daemon.store daemon))
               : int);
            assert (Option.is_some (Registry.find (Daemon.registry daemon) (id parent)));
            assert (Option.is_some (Registry.find (Daemon.registry daemon) (id middle)));
            id parent, id middle, id child, id leaf, workspace)
        in
        Eio.Path.save
          ~create:(`Or_truncate 0o600)
          Eio.Path.(Eio.Stdenv.fs env / workspace / "value.txt")
          "after-restart";
        with_daemon (Some "fixture-v1") (fun sw daemon client ->
          let parent = find daemon parent_id
          and middle = find daemon middle_id in
          let child = find daemon child_id
          and leaf = find daemon leaf_id in
          stopped parent;
          stopped middle;
          running child;
          running leaf;
          assert (not (Owner.is_loaded parent.runtime));
          assert (not (Owner.is_loaded middle.runtime));
          let child_handle = attach sw client child
          and leaf_handle = attach sw client leaf in
          read child_handle child "after-restart";
          read leaf_handle leaf "after-restart";
          assert (List.is_empty (state parent).invocations);
          assert (List.is_empty (state middle).invocations);
          H.stop child_handle ~mode:Cancel |> protocol_ok |> ignore;
          stopped child;
          stopped leaf;
          H.close child_handle;
          H.close leaf_handle;
          Agent_client.Connection.close client;
          let registry = Daemon.registry daemon in
          let rec evict_stopped_ancestors remaining =
            if Option.is_some (Registry.find registry parent_id)
            then (
              if remaining <= 0 then failwith "inactive eviction exhausted its bound";
              let removed =
                Registry.unload_inactive
                  registry
                  ~index_entries:
                    (Agent_store.Session_store.list_sessions (Daemon.store daemon))
              in
              if Option.is_some (Registry.find registry parent_id)
              then
                if removed > 0
                then evict_stopped_ancestors (remaining - 1)
                else failwith "inactive eviction retained parent without progress")
          in
          (* One best-effort pass follows session IDs, so a later child close can
             release an ancestor already visited. Only actual removals justify
             another fresh-index pass; the loaded-entry count bounds all passes. *)
          evict_stopped_ancestors (List.length (Registry.entries registry));
          assert (Option.is_none (Registry.find (Daemon.registry daemon) parent_id));
          let client = connection daemon (principal ()) in
          Exn.protect
            ~finally:(fun () -> Agent_client.Connection.close client)
            ~f:(fun () ->
              initialize client;
              let child =
                Registry.load (Daemon.registry daemon) child_id |> protocol_ok
              in
              let child_handle = attach sw client child in
              H.start child_handle ~queue_if_limited:false |> protocol_ok |> ignore;
              read child_handle child "after-restart";
              let parent = find daemon parent_id in
              assert (not (Owner.is_loaded parent.runtime));
              H.stop child_handle ~mode:Cancel |> protocol_ok |> ignore;
              H.close child_handle));
        with_daemon (Some "fixture-v2") (fun sw daemon client ->
          let child = Registry.load (Daemon.registry daemon) child_id |> protocol_ok in
          let handle = attach sw client child in
          let before = !requests in
          (match H.start handle ~queue_if_limited:false with
           | Error { code = Permission_denied; message; _ } ->
             assert (String.is_substring message ~substring:"delegation.lifetime_denied")
           | _ -> failwith "changed host grant admitted old independent session");
          [%test_eq: int] before !requests;
          assert (not (Owner.is_loaded child.runtime));
          H.close handle);
        with_daemon (Some "fixture-v1") (fun sw daemon client ->
          let child = Registry.load (Daemon.registry daemon) child_id |> protocol_ok in
          let child_handle = attach sw client child in
          H.start child_handle ~queue_if_limited:false |> protocol_ok |> ignore;
          let leaf = Registry.load (Daemon.registry daemon) leaf_id |> protocol_ok in
          let leaf_handle = attach sw client leaf in
          H.start leaf_handle ~queue_if_limited:false |> protocol_ok |> ignore;
          read leaf_handle leaf "after-restart";
          let parent = find daemon parent_id in
          let parent_handle = attach sw client parent in
          (match
             H.delete
               parent_handle
               ~expected_revision:(state parent).counters.revision
               ~policy:Remove
               ~confirmation:(P.Id.Session.to_string parent_id)
           with
           | Error { code = Conflict; _ } -> ()
           | Error _ | Ok _ ->
             failwith "live independent descendants admitted parent removal");
          assert (Option.is_some (Registry.find (Daemon.registry daemon) parent_id));
          assert (Eio.Path.is_directory Eio.Path.(Eio.Stdenv.fs env / workspace));
          H.stop leaf_handle ~mode:Cancel |> protocol_ok |> ignore;
          H.stop child_handle ~mode:Cancel |> protocol_ok |> ignore;
          stopped child;
          stopped leaf;
          H.close child_handle;
          H.close leaf_handle;
          H.close parent_handle);
        print_endline
          "explicit grant and stable replay; lifetime conflict; stopped ancestors retain \
           roots and avoid execution";
        print_endline
          "independent child and owned leaf read across restart; parent reset/eviction \
           excluded; owned stop preserved";
        print_endline
          "lazy ancestor reload avoids execution; explicit owned stop preserved";
        print_endline
          "changed host policy denies restart; restored grant permits use; live \
           descendants reject removal and preserve workspace"));
  [%expect
    {|
    explicit grant and stable replay; lifetime conflict; stopped ancestors retain roots and avoid execution
    independent child and owned leaf read across restart; parent reset/eviction excluded; owned stop preserved
    lazy ancestor reload avoids execution; explicit owned stop preserved
    changed host policy denies restart; restored grant permits use; live descendants reject removal and preserve workspace
  |}]
;;
