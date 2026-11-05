open! Core
open Agent_server_test_support
module P = Agent_protocol
module A = Agent_session
module Factory = Agent_server.Session_factory
module Daemon = Agent_server.Daemon
module Owner = Agent_server.Runtime_owner
module Source = A.Runtime_builder.Source

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let state entry =
  A.Session_actor.state entry.Agent_server.Session_registry.actor |> protocol_ok
;;

let headless_request entry key =
  let before = state entry in
  A.Run_admission_request.Headless.create
    ~session_id:before.identity.session_id
    ~generation:before.identity.generation
    ~expected_revision:before.counters.revision
    ~mode:Single_turn
    ~input:Authored_start
    ~key:(P.Idempotency_key.of_string key |> protocol_ok)
  |> protocol_ok
;;

let with_preparation entry key f =
  let request = headless_request entry key in
  let preparation =
    match
      A.Session_actor.begin_headless_run_preparation
        entry.actor
        ~actor:(Operator_authorization.trusted_local (principal ()))
        ~authorize:(fun _ -> Ok ())
        ~request
      |> protocol_ok
    with
    | Prepare preparation -> preparation
    | Retained _ -> failwith "source fixture unexpectedly retained a Run"
  in
  Exn.protect
    ~finally:(fun () ->
      A.Session_actor.end_run_preparation entry.actor preparation |> protocol_ok)
    ~f:(fun () -> f preparation)
;;

let journal_entries env entry =
  let handle = Option.value_exn entry.Agent_server.Session_registry.store_handle in
  let journal =
    Agent_store.Journal.open_existing
      ~env
      ~directory:(Agent_store.Session_store.Handle.journal_directory handle)
      ~max_payload_length:16777216
      ~max_segment_bytes:67108864L
      ~max_segment_frames:4096
    |> store_ok
  in
  let scan = Agent_store.Journal.scan journal |> store_ok in
  scan.entries
;;

let frames env entry =
  journal_entries env entry
  |> List.map ~f:(fun entry -> Agent_store.Frame.payload entry.frame)
;;

let assert_same before entry =
  assert (
    Jsonaf.exactly_equal
      (state_document before |> store_ok |> Document_schema.Document.json)
      (state_document (state entry) |> store_ok |> Document_schema.Document.json))
;;

let source prepared =
  Owner.Prepared.check_basis prepared |> protocol_ok;
  A.Session_actor.Source_binding.source (Owner.Prepared.binding prepared)
;;

let%expect_test "Factory source staging verifies cached custody before binding" =
  let constructions = ref 0
  and provider_calls = ref 0 in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr provider_calls;
      failwith "source staging dispatched inference")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr constructions;
          base.runtime_inference_ports actor)
    }
  in
  let prompt_text =
    {|<developer>Retained linked source.</developer>
<script id="owner" language="chatml" kind="moderator" project="chatml.project.json">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
  in
  Creation_tests.with_daemon
    ~prompt_text
    ~prepare_sources:(fun env root ->
      Eio.Path.save
        ~create:(`Exclusive 0o600)
        Eio.Path.(Eio.Stdenv.fs env / root / "chatml.project.json")
        {|{"schema_version":1,"namespace":"factory-source"}|})
    ~policy
    (fun env sw root daemon ->
       let factory = Daemon.factory daemon in
       let entry =
         Factory.create_session
           factory
           ~command_audit:None
           ~principal:(principal ())
           (create_request ())
         |> protocol_ok
       in
       Exn.protect ~finally:entry.close ~f:(fun () ->
         assert (not (Owner.is_loaded entry.runtime));
         let retained =
           match
             (List.hd_exn (A.Prompt_catalog.entries (Daemon.prompts daemon))).availability
           with
           | Ready revision -> revision
           | Disabled | Unavailable _ -> failwith "captured source unavailable"
         in
         assert (Option.is_some (A.Prompt_revision.programs retained));
         List.iter [ "parent.chatmd"; "chatml.project.json" ] ~f:(fun name ->
           Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / root / name));
         let before = state entry in
         let saved = frames env entry in
         let initial_constructions = !constructions in
         with_preparation entry "verified-cached-source" (fun preparation ->
           Factory.with_prepared_run_source factory entry ~preparation (fun prepared ->
             let selected = source prepared in
             (match Source.selection selected with
              | Authored revision -> assert (phys_equal revision retained)
              | Captured _ | Authored_child _ | Generated _ ->
                failwith "ordinary source was reclassified");
             assert (Option.is_some (Source.programs selected));
             Ok ())
           |> protocol_ok);
         assert_same before entry;
         [%test_eq: string list] saved (frames env entry);
         [%test_eq: int] initial_constructions !constructions;
         [%test_eq: int] 0 !provider_calls;
         (* An actual second Factory cannot bind the first Factory's real entry,
           even though that entry is intentionally not registered anywhere. *)
         let foreign_root = Filename.concat root "foreign" in
         Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / foreign_root);
         let foreign_prompt = Filename.concat foreign_root "parent.chatmd" in
         Eio.Path.save
           ~create:(`Exclusive 0o600)
           Eio.Path.(Eio.Stdenv.fs env / foreign_prompt)
           "<developer>Foreign retained publication.</developer>";
         let foreign =
           Daemon.start
             ~sw
             ~env
             ~config:(config foreign_root foreign_root foreign_prompt)
             ~tool_dir:foreign_root
             ~home:foreign_root
             ~process_start_identity:None
             ~options:{ Daemon.default_options with inference_policy = policy }
             ()
           |> protocol_ok
         in
         Exn.protect
           ~finally:(fun () -> Daemon.shutdown foreign |> protocol_ok)
           ~f:(fun () ->
             let calls = ref 0 in
             with_preparation entry "foreign-source-factory" (fun preparation ->
               match
                 Factory.with_prepared_run_source
                   (Daemon.factory foreign)
                   entry
                   ~preparation
                   (fun _ ->
                      Int.incr calls;
                      Ok ())
               with
               | Error { code = Permission_denied; message; _ } ->
                 [%test_eq: string] "source staging has a foreign session store" message
               | Error _ | Ok _ ->
                 failwith "foreign Factory staged another owner's source");
             [%test_eq: int] 0 !calls;
             let original = Operator_authorization.trusted_local (principal ()) in
             let clean =
               Factory.with_effect_deferred_run_source
                 factory
                 entry
                 ~actor:original
                 ~authorize:(fun _ -> Ok ())
                 ~request:(headless_request entry "before-foreign-deferred-source")
                 (fun _ prepared ->
                    Owner.Prepared.check_basis prepared |> protocol_ok;
                    Ok ())
               |> protocol_ok
             in
             let module Attempt = Factory.Deferred_run_attempt in
             let observation =
               match clean with
               | Attempt.Outcome.Attempted attempt ->
                 Attempt.result attempt |> protocol_ok;
                 Attempt.no_effects attempt |> Option.value_exn
               | Retained _ -> failwith "clean foreign-control source retained a Run"
             in
             A.Session_actor.No_effects_observation.check_current observation
             |> protocol_ok;
             (match
                Factory.with_effect_deferred_run_source
                  (Daemon.factory foreign)
                  entry
                  ~actor:original
                  ~authorize:(fun _ -> Ok ())
                  ~request:(headless_request entry "foreign-deferred-source-factory")
                  (fun _ _ ->
                     Int.incr calls;
                     Ok ())
              with
              | Error error ->
                let expected =
                  P.Error.create
                    Permission_denied
                    ~message:"source lifetime belongs to another Factory"
                    ~retryable:false
                    ()
                in
                assert (
                  Jsonaf.exactly_equal (P.Error.to_json expected) (P.Error.to_json error))
              | Ok _ -> failwith "foreign Factory issued deferred source ownership");
             [%test_eq: int] 0 !calls;
             A.Session_actor.No_effects_observation.check_current observation
             |> protocol_ok;
             A.Session_actor.No_effects_observation.check_basis observation |> protocol_ok;
             assert_same before entry;
             [%test_eq: string list] saved (frames env entry);
             [%test_eq: int] initial_constructions !constructions;
             [%test_eq: int] 0 !provider_calls;
             assert (not (Owner.is_loaded entry.runtime)));
         (* Corrupt the consumed captured tree, not the deleted original source.
           The cached revision must not bypass the actual Factory verifier. *)
         let captured_file =
           Eio.Path.(
             A.Prompt_revision.materialized_tree retained
             / A.Prompt_revision.root_relative_path retained)
         in
         let payload =
           A.Prompt_revision.artifact retained
           |> fun (artifact : Agent_store.Prompt_artifact_store.Artifact.t) ->
           Option.value_exn artifact.program_bundle
           |> Agent_store.Prompt_program_bundle.payloads
           |> List.hd_exn
         in
         let payload_file =
           Eio.Path.(
             Eio.Stdenv.fs env
             / Filename.dirname
                 (Eio.Path.native_exn (A.Prompt_revision.materialized_tree retained))
             / "programs"
             / Agent_store.Prompt_program_bundle.Payload.sha256 payload)
         in
         [%test_eq: string]
           (Agent_store.Prompt_program_bundle.Payload.contents payload)
           (Eio.Path.load payload_file);
         List.iter
           [ "changed-cached-tree", captured_file
           ; "changed-private-program", payload_file
           ]
           ~f:(fun (key, file) ->
             let bytes = Eio.Path.load file in
             Eio.Path.unlink file;
             Eio.Path.save ~create:(`Exclusive 0o600) file (bytes ^ "\nchanged");
             Exn.protect
               ~finally:(fun () ->
                 Eio.Path.unlink file;
                 Eio.Path.save ~create:(`Exclusive 0o400) file bytes)
               ~f:(fun () ->
                 let calls = ref 0 in
                 with_preparation entry key (fun preparation ->
                   match
                     Factory.with_prepared_run_source factory entry ~preparation (fun _ ->
                       Int.incr calls;
                       Ok ())
                   with
                   | Error { code = Prompt_unavailable; _ } -> ()
                   | Error _ | Ok _ -> failwith "Factory accepted changed retained bytes");
                 [%test_eq: int] 0 !calls));
         with_preparation entry "restored-private-program" (fun preparation ->
           Factory.with_prepared_run_source factory entry ~preparation (fun prepared ->
             (match Source.selection (source prepared) with
              | Authored revision -> assert (phys_equal revision retained)
              | Captured _ | Authored_child _ | Generated _ ->
                failwith "restored source changed ownership");
             Ok ())
           |> protocol_ok);
         assert_same before entry;
         [%test_eq: string list] saved (frames env entry);
         [%test_eq: int] initial_constructions !constructions;
         [%test_eq: int] 0 !provider_calls;
         assert (not (Owner.is_loaded entry.runtime));
         print_endline
           "exact cached programs; originals absent; foreign/tampered denied; no \
            construction"));
  [%expect
    {| exact cached programs; originals absent; foreign/tampered denied; no construction |}]
;;

let%expect_test
    "Factory generated source borrows loaded bindings without loading a cold policy owner"
  =
  let constructions = ref 0
  and provider_calls = ref 0 in
  let base =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      Int.incr provider_calls;
      failwith "source staging dispatched inference")
  in
  let policy =
    { base with
      runtime_inference_ports =
        (fun actor ->
          Int.incr constructions;
          base.runtime_inference_ports actor)
    }
  in
  let prompt_text =
    {|<developer>Live inherited policy.</developer>
<script id="parent" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
  in
  Creation_tests.with_daemon
    ~prompt_text
    ~qualify_chatml_extensions:true
    ~policy
    (fun env _ root daemon ->
       let factory = Daemon.factory daemon in
       let parent =
         Factory.create_session
           factory
           ~command_audit:None
           ~principal:(principal ())
           (create_request ~start_immediately:true ())
         |> protocol_ok
       in
       (match
          Agent_server.Session_registry.add
            (Daemon.registry daemon)
            ~session_id:(state parent).identity.session_id
            parent
        with
        | Ok () -> ()
        | Error error ->
          parent.close ();
          raise_s [%sexp (error : P.Error.t)]);
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
         let rec await_idle () =
           let current = state parent in
           match
             current.active_operation, current.lifecycle.observed, current.moderator
           with
           | None, Idle, Some _ -> ()
           | _ ->
             Eio.Fiber.yield ();
             await_idle ()
         in
         await_idle ());
       let definition =
         Owner.with_loaded_runtime parent.runtime (fun (runtime : A.Runtime_builder.t) ->
           let native = Option.value_exn runtime.native_runtime in
           assert (Lazy.is_val native.capabilities);
           let capabilities =
             Lazy.force native.capabilities
             |> Result.map_error ~f:(fun error ->
               P.Error.invalid_request error.Chat_response.Tool_capability.message)
             |> protocol_ok
           in
           let bundle =
             Chatmd_source_bundle.create
               ~root_file:"child.chatmd"
               ~sources:
                 [ ( "child.chatmd"
                   , {|<developer>Captured generated source.</developer>
<script id="child" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>|}
                   )
                 ]
               ()
             |> Result.ok_or_failwith
           in
           A.Generated_definition.prepare
             ~env
             ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
             ~revision_id:(P.Id.Prompt_revision.create ())
             ~created_at:(P.Timestamp.now ())
             ~current_capabilities:(fun () -> capabilities)
             ~references:(Chat_response.Tool_capability.references capabilities)
             bundle
           |> Result.map_error ~f:(fun diagnostics ->
             P.Error.invalid_request
               (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string
                |> String.concat ~sep:"\n")))
         |> protocol_ok
       in
       let child =
         Factory.create_generated_session
           ~start_immediately:false
           factory
           ~parent_session_id:(state parent).identity.session_id
           ~idempotency_key:
             (P.Idempotency_key.of_string "source-generated-child" |> protocol_ok)
           ~display_name:None
           definition
         |> protocol_ok
       in
       let expected_revision = (state child).spec.prompt_revision_id in
       assert (not (Owner.is_loaded child.runtime));
       Eio.Path.unlink Eio.Path.(Eio.Stdenv.fs env / root / "parent.chatmd");
       let before = state child in
       let saved = frames env child in
       let baseline = !constructions in
       let escaped = ref None in
       with_preparation child "loaded-generated-source" (fun preparation ->
         Factory.with_prepared_run_source factory child ~preparation (fun prepared ->
           escaped := Some prepared;
           let selected = source prepared in
           (match Source.selection selected with
            | Generated actual ->
              assert (
                P.Id.Prompt_revision.equal
                  expected_revision
                  (A.Generated_definition.artifact actual).revision_id)
            | Authored _ | Captured _ | Authored_child _ ->
              failwith "generated source was coerced to ordinary revision");
           (match Source.moderator selected with
            | Extension _ -> ()
            | Legacy _ | No_moderator -> failwith "generated moderator selection was lost");
           Ok ())
         |> protocol_ok);
       assert (Result.is_error (Owner.Prepared.check_basis (Option.value_exn !escaped)));
       assert_same before child;
       [%test_eq: string list] saved (frames env child);
       [%test_eq: int] baseline !constructions;
       [%test_eq: int] 0 !provider_calls;
       (* Keep the real Factory-authenticated parent Owner. Its source lease
          finalizer must join the actual parent mutex before an observation can
          escape, even though the child has no new durable facts or effects. *)
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
         Eio.Switch.run (fun sw ->
           let entered, entered_u = Eio.Promise.create () in
           let returning, returning_u = Eio.Promise.create () in
           let release, release_u = Eio.Promise.create () in
           let blocker = ref None in
           let closed_prepared = ref None in
           let callbacks = ref 0 in
           let primary =
             P.Error.create
               Invalid_request
               ~message:"actual generated source callback rejection"
               ~retryable:false
               ~data:(`Object [ "retained", `Number "538" ])
               ()
           in
           let release_parent () =
             if Option.is_none (Eio.Promise.peek release)
             then Eio.Promise.resolve release_u ()
           in
           let attempted =
             Eio.Fiber.fork_promise ~sw (fun () ->
               Factory.with_effect_deferred_run_source
                 factory
                 child
                 ~actor:(Operator_authorization.trusted_local (principal ()))
                 ~authorize:(fun _ -> Ok ())
                 ~request:(headless_request child "generated-parent-source-join")
                 (fun _ prepared ->
                    Int.incr callbacks;
                    closed_prepared := Some prepared;
                    (match Source.selection (source prepared) with
                     | Generated actual ->
                       assert (
                         P.Id.Prompt_revision.equal
                           expected_revision
                           (A.Generated_definition.artifact actual).revision_id)
                     | Authored _ | Captured _ | Authored_child _ ->
                       failwith "deferred generated source changed family");
                    let held =
                      Eio.Fiber.fork_promise ~sw (fun () ->
                        Owner.For_testing.with_loaded_runtime parent.runtime (fun () ->
                          Eio.Promise.resolve entered_u ();
                          Eio.Promise.await release;
                          Ok ()))
                    in
                    blocker := Some held;
                    Eio.Promise.await entered;
                    (* No yield follows this signal before the Error; the next
                      wait is the real parent source-lease finalizer's mutex. *)
                    Eio.Promise.resolve returning_u ();
                    Error primary))
           in
           let capture f =
             match f () with
             | value -> Ok value
             | exception exn -> Error (exn, Stdlib.Printexc.get_raw_backtrace ())
           in
           let result =
             capture (fun () ->
               Eio.Fiber.first
                 (fun () -> Eio.Promise.await returning)
                 (fun () ->
                    ignore (Eio.Promise.await_exn attempted);
                    failwith "generated source returned before its parent mutex barrier");
               [%test_eq: int] 1 !callbacks;
               assert (Option.is_none (Eio.Promise.peek attempted));
               assert_same before child;
               [%test_eq: string list] saved (frames env child);
               [%test_eq: int] baseline !constructions;
               [%test_eq: int] 0 !provider_calls;
               (* Do not reenter the parent's Owner while its mutex is held. *)
               release_parent ();
               Eio.Promise.await_exn (Option.value_exn !blocker) |> protocol_ok;
               let module Attempt = Factory.Deferred_run_attempt in
               let completed = Eio.Promise.await_exn attempted |> protocol_ok in
               let attempt =
                 match completed with
                 | Attempt.Outcome.Attempted attempt -> attempt
                 | Retained _ -> failwith "generated source reused a retained Run"
               in
               (match Attempt.result attempt with
                | Error error ->
                  assert (
                    Jsonaf.exactly_equal (P.Error.to_json primary) (P.Error.to_json error))
                | Ok _ -> failwith "generated source lost its callback Error");
               let observation = Attempt.no_effects attempt |> Option.value_exn in
               A.Session_actor.No_effects_observation.check_current observation
               |> protocol_ok;
               A.Session_actor.No_effects_observation.check_basis observation
               |> protocol_ok;
               assert (
                 Result.is_error
                   (Owner.Prepared.check_basis (Option.value_exn !closed_prepared)));
               assert_same before child;
               [%test_eq: string list] saved (frames env child);
               [%test_eq: int] baseline !constructions;
               [%test_eq: int] 0 !provider_calls;
               assert (Owner.is_loaded parent.runtime);
               assert (not (Owner.is_loaded child.runtime)))
           in
           let cleaned =
             capture (fun () ->
               Eio.Cancel.protect (fun () ->
                 release_parent ();
                 ignore (Eio.Promise.await_exn attempted);
                 Option.iter !blocker ~f:(fun held ->
                   Eio.Promise.await_exn held |> protocol_ok)))
           in
           match result, cleaned with
           | Error (exn, backtrace), _ | Ok (), Error (exn, backtrace) ->
             Exn.raise_with_original_backtrace exn backtrace
           | Ok (), Ok () -> ()));
       (* Retained Running+moderator makes old generated_parent fingerprint try
         Runtime_owner.moderation_source, which used to load BEFORE the lease. *)
       let quiescent = state parent in
       assert (Option.is_none quiescent.active_operation);
       (match quiescent.lifecycle.observed with
        | Idle -> ()
        | _ -> failwith "source fixture parent is not idle before eviction");
       Owner.unload parent.runtime |> protocol_ok;
       assert (not (Owner.is_loaded parent.runtime));
       let parent_state = state parent in
       assert (P.Session.equal_desired_state parent_state.lifecycle.desired Running);
       assert (Option.is_some parent_state.moderator);
       let baseline = !constructions in
       let callbacks = ref 0 in
       with_preparation child "cold-generated-source" (fun preparation ->
         match
           Factory.with_prepared_run_source factory child ~preparation (fun _ ->
             Int.incr callbacks;
             Ok ())
         with
         | Error { code = Invalid_state; message; retryable = true; _ } ->
           [%test_eq: string] "session runtime is not already loaded" message
         | Error error ->
           raise_s [%sexp "unexpected cold-parent rejection", (error : P.Error.t)]
         | Ok () -> failwith "cold parent was admitted by source staging");
       [%test_eq: int] 0 !callbacks;
       [%test_eq: int] baseline !constructions;
       [%test_eq: int] 0 !provider_calls;
       assert (not (Owner.is_loaded parent.runtime));
       assert (not (Owner.is_loaded child.runtime));
       assert_same before child;
       [%test_eq: string list] saved (frames env child);
       print_endline
         "real generated owner; loaded selection; cold moderated parent never constructed");
  [%expect
    {| real generated owner; loaded selection; cold moderated parent never constructed |}]
;;
