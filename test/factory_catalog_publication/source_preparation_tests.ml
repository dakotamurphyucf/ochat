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

let with_preparation entry key f =
  let before = state entry in
  let request =
    A.Run_admission_request.Headless.create
      ~session_id:before.identity.session_id
      ~generation:before.identity.generation
      ~expected_revision:before.counters.revision
      ~mode:Single_turn
      ~input:Authored_start
      ~key:(P.Idempotency_key.of_string key |> protocol_ok)
    |> protocol_ok
  in
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

let frames env entry =
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
  List.map scan.entries ~f:(fun entry -> Agent_store.Frame.payload entry.frame)
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
             [%test_eq: int] 0 !calls);
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
