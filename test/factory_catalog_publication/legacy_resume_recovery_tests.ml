open! Core
open Agent_server_test_support
open Retry_support
module Checkpoint = A.Moderator_checkpoint

type mode =
  | Resume_then_halt
  | Unavailable_tool
[@@deriving sexp]

let prompt mode =
  sprintf
    {|<developer>Actual installed legacy recovery lifecycle.</developer>
<tool name="read_file"><read id="data" path="${workspace}"/></tool>
<script id="owner" language="chatml" kind="moderator">
let initial_state = 0
let on_event ctx state event = match event with
| `Session_start -> Task.pure(state + 1)
| `Session_resume -> %s
| _ -> Task.pure(state)
</script>|}
    (match mode with
     | Resume_then_halt ->
       {|if state == 1 then Task.pure(state + 1)
  else Task.bind(Runtime.end_session("installed resume stops before provider"), fun ignored -> Task.pure(state + 1))|}
     | Unavailable_tool ->
       {|Task.bind(Tool.call("read_file", `Object([
    {key = "root"; value = `String("data")},
    {key = "file"; value = `String("resume-only.txt")}
  ])), fun result -> match result with
  | `Error(message) -> Task.fail(message)
  | `Ok(value) -> Task.pure(state + 1))|})
;;

let receipts current phase =
  List.filter current.A.Session_state.moderator_executions ~f:(fun receipt ->
    P.Moderator_execution.equal_phase receipt.P.Moderator_execution.context.phase phase)
;;

let checkpoint current =
  Checkpoint.decode current.A.Session_state.moderator |> protocol_ok |> Option.value_exn
;;

let runtime entry =
  Owner.with_loaded_runtime entry.Registry.runtime (fun runtime -> Ok runtime)
  |> protocol_ok
;;

let activation entry =
  Option.value_exn (runtime entry).A.Runtime_builder.moderator_activation
;;

let assert_original before current =
  let original = List.hd_exn (receipts before Session_start) in
  assert (
    List.exists current.A.Session_state.moderator_executions ~f:(fun candidate ->
      P.Moderator_execution.equal original candidate));
  [%test_eq: int] 1 (List.length (receipts current Session_start));
  List.iter (receipts current Session_resume) ~f:(fun resume ->
    assert (P.Invocation.equal_observer original.context.source resume.context.source))
;;

let assert_same before after =
  assert (
    Jsonaf.exactly_equal
      (state_document before |> Existing.store_ok |> Document_schema.Document.json)
      (state_document after |> Existing.store_ok |> Document_schema.Document.json))
;;

(* Operator_only is used only as an existing trusted lower-host composition,
   permitting deterministic manual calls to the genuine Factory recovery and
   Owner lease APIs. The separate552 host-lifetime case starts actual Execute
   after physical shutdown; it remains the scheduler integration regression. *)
let with_recovered mode f =
  let providers = ref 0 in
  let policy =
    inference_policy ~default_model:"fixture-model" ~post_stream:(fun ~sw:_ ~inputs:_ ->
      incr providers;
      failwith "installed resume must stop or reject before any provider")
  in
  Creation_tests.with_daemon
    ~startup_mode:Operator_only
    ~prompt_text:(prompt mode)
    ~sources:[ "resume-only.txt", "unavailable original must not read this" ]
    ~policy
    (fun env sw root daemon ->
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 30. (fun () ->
         let old_actor, before =
           Goal_job_runtime_tests.with_loaded_manual_entry daemon (fun entry _ ->
             let before = state entry in
             [%test_eq: int] 1 (List.length (receipts before Session_start));
             [%test_eq: int] 0 (List.length (receipts before Session_resume));
             entry.actor, before)
         in
         let old_store = Daemon.store daemon in
         Daemon.shutdown daemon |> protocol_ok;
         assert (Agent_store.Session_store.is_closed old_store);
         let reopened =
           Daemon.start
             ~sw
             ~env:(env :> Eio_unix.Stdenv.base)
             ~config:(config root root (Filename.concat root "parent.chatmd"))
             ~tool_dir:root
             ~home:root
             ~process_start_identity:None
             ~options:
               { Daemon.default_options with
                 inference_policy = policy
               ; startup_mode = Operator_only
               }
             ()
           |> protocol_ok
         in
         protect_cleanup
           ~finally:(fun () -> Daemon.shutdown reopened |> protocol_ok)
           (fun () ->
              let indexed =
                Agent_store.Session_store.list_sessions_checked (Daemon.store reopened)
                |> Existing.store_ok
                |> List.find_exn ~f:(fun indexed ->
                  P.Id.Session.equal
                    indexed.Agent_store.Session_index.Entry.session.id
                    before.identity.session_id)
              in
              let recovered =
                Factory.recover_session (Daemon.factory reopened) indexed |> protocol_ok
              in
              (match
                 Registry.add
                   (Daemon.registry reopened)
                   ~session_id:before.identity.session_id
                   recovered
               with
               | Ok () -> ()
               | Error error ->
                 recovered.close ();
                 raise_s [%sexp (error : P.Error.t)]);
              protect_cleanup ~finally:recovered.close (fun () ->
                assert (not (phys_equal old_actor recovered.actor));
                assert (Owner.is_loaded recovered.runtime);
                let pending = activation recovered in
                assert (pending.pending ());
                assert (not (pending.startup_pending ()));
                let current = state recovered in
                assert_original before current;
                [%test_eq: int] 0 (List.length (receipts current Session_resume));
                [%test_eq: int] 0 !providers;
                f env recovered before providers))))
;;

let%expect_test
    "real legacy recovery resumes once; unavailable original stays unavailable"
  =
  with_recovered Resume_then_halt (fun _env recovered before providers ->
    (* This actual legacy Installed_resume is still pending after the Factory
       source stage has cleared. Its callback claims the ordinary installed
       owner, rather than retaining a constructor or joining its own stage. *)
    assert (Owner.source_lane_idle recovered.runtime);
    assert (Owner.drain_idle_moderator recovered.runtime |> protocol_ok);
    assert (Owner.source_lane_idle recovered.runtime);
    let completed = state recovered in
    assert_original before completed;
    (match receipts completed Session_resume with
     | [ { status = Completed _; _ } ] -> ()
     | actual -> raise_s [%sexp (actual : P.Moderator_execution.t list)]);
    assert (not ((activation recovered).pending ()));
    assert (not (Owner.drain_idle_moderator recovered.runtime |> protocol_ok));
    assert_same completed (state recovered);
    [%test_eq: int] 0 !providers;
    (* A second physical runtime from the SAME live Owner is initialized Ready.
       Do not drain its pending resume: the actual foreground handler must join
       it under this new Operation before reaching any provider. *)
    Owner.unload_and_wait recovered.runtime |> protocol_ok;
    Owner.ensure_loaded recovered.runtime |> protocol_ok;
    assert (Owner.source_lane_idle recovered.runtime);
    let resumed = activation recovered in
    assert (resumed.pending ());
    let writer, _ =
      Actor.attach recovered.actor ~mode:Read_write ~subscribe:false |> protocol_ok
    in
    protect_cleanup
      ~finally:(fun () -> Actor.detach recovered.actor writer.id |> protocol_ok)
      (fun () ->
         let id = A.History_id_source.allocate recovered.history_ids |> protocol_ok in
         let message =
           Owner.parse_user_content
             recovered.runtime
             ~id
             { kind = Plain_text
             ; text = "actual foreground after Ready reload"
             ; attachments = []
             }
           |> protocol_ok
           |> A.History_codec.to_protocol
         in
         let submission =
           Actor.submit_message_authenticated
             recovered.actor
             ~actor:original
             ~attachment_id:writer.id
             message
           |> protocol_ok
         in
         assert (Option.is_some submission.operation_id);
         let rec await () =
           let current =
             Actor.with_moderator_checkpoint recovered.actor (fun () ->
               Actor.state recovered.actor)
             |> protocol_ok
           in
           List.iter (receipts current Session_resume) ~f:(fun receipt ->
             match receipt.status with
             | Failed _ | Interrupted _ ->
               raise_s [%sexp (receipt : P.Moderator_execution.t)]
             | Running | Completed _ -> ());
           if Option.is_none current.active_operation && (checkpoint current).halted
           then current
           else (
             Eio.Fiber.yield ();
             await ())
         in
         let final = await () in
         assert_original before final;
         [%test_eq: int] 2 (List.length (receipts final Session_resume));
         (* The stopped runtime is retiring; inspect this same activation's
            factual completion instead of acquiring a new execution lease. *)
         assert (not (resumed.pending ()));
         [%test_eq: int] 0 !providers));
  with_recovered Unavailable_tool (fun _env recovered before providers ->
    let prior = state recovered in
    let error =
      match Owner.drain_idle_moderator recovered.runtime with
      | Ok _ -> failwith "recovery fabricated original Tool authority"
      | Error error -> error
    in
    (* Tool.call deliberately sanitizes the native admission refusal; the
       failed Event and absence of an Invocation prove no fresh tool owner. *)
    [%test_eq: P.Error.code] Invalid_state error.code;
    [%test_eq: string] "invocation.admission_failed" error.message;
    assert (not error.retryable);
    let failed = state recovered in
    assert_original before failed;
    (match receipts failed Session_resume with
     | [ { status = Failed _; _ } ] -> ()
     | actual -> raise_s [%sexp (actual : P.Moderator_execution.t list)]);
    assert (List.equal P.Invocation.equal prior.invocations failed.invocations);
    assert (Option.equal Jsonaf.exactly_equal prior.moderator failed.moderator);
    [%test_eq: int] 0 !providers;
    let repeated =
      Owner.with_loaded_runtime recovered.runtime (fun runtime ->
        (Option.value_exn runtime.A.Runtime_builder.moderator_activation).run ())
    in
    (match repeated with
     | Error repeated ->
       assert (Jsonaf.exactly_equal (P.Error.to_json error) (P.Error.to_json repeated))
     | Ok _ -> failwith "failed installed resume was replayed");
    assert_same failed (state recovered));
  print_endline
    "post-install resume ACK is once per runtime; foreground halt precedes provider; no \
     restored original";
  [%expect
    {| post-install resume ACK is once per runtime; foreground halt precedes provider; no restored original |}]
;;
