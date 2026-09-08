open Core
module M = Chat_response.Moderator_manager
module MI = Chat_response.Moderator_invocation
module EC = Chat_response.Extension_compiler
module Cap = Chat_response.Tool_capability
module P = Agent_protocol
module I = P.Invocation
module L = Chatml.Chatml_lang
module R = Chatml_host_runtime

let ok = Result.ok_or_failwith

let diags result =
  result
  |> Result.map_error ~f:(fun ds ->
    String.concat ~sep:"; " (List.map ds ~f:Chatmd_shell_spec.Diagnostic.to_string))
  |> ok
;;

let protocol result = result |> Result.map_error ~f:(fun e -> e.P.Error.message) |> ok

let expect prefix = function
  | Error message when String.is_substring message ~substring:prefix -> ()
  | Error message -> failwith ("expected " ^ prefix ^ ", got " ^ message)
  | Ok _ -> failwith ("expected " ^ prefix)
;;

let generator =
  let counter = ref 0 in
  P.Id.Generator.create ~bytes:(fun n ->
    incr counter;
    String.make n (Char.of_int_exn (!counter mod 255)))
;;

let setup
      env
      ?(schema = "true")
      ?(initial = "0")
      ?(capabilities = Chat_response.Moderation.Capabilities.default)
      body
  =
  let dir = Eio.Stdenv.cwd env in
  let source =
    "let initial_state = "
    ^ initial
    ^ "\nlet on_event = fun ctx state event -> match event with\n| `Tool_invoked(p) -> "
    ^ body
    ^ "\n| _ -> Task.pure(state)"
  in
  let loader =
    Source_loader.captured_filesystem ~root:dir ~sources:[ "schema.json", schema ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir
      ~source_loader:loader
      ("<script id=\"handler\" language=\"chatml\" kind=\"moderator\" \
        api=\"extensibility-v1\">"
       ^ source
       ^ "</script><tool name=\"counter\" type=\"moderator\" moderator=\"handler\" \
          input_schema=\"schema.json\" output_schema=\"schema.json\"/>")
  in
  let caps =
    Cap.create
      ~owner:"fixture"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
      []
    |> Result.map_error ~f:(fun e -> e.Cap.message)
    |> ok
  in
  let definition =
    EC.prepare_definition_in_domain ~env ~capabilities:caps elements |> diags
  in
  let _, artifact = M.Registry.of_definition M.Registry.empty definition |> ok in
  let artifact = Option.value_exn artifact in
  let allocator =
    History_entry.Allocator.create ~namespace:"invocation-fixture" ~next_sequence:0 |> ok
  in
  let manager = M.create_entries ~artifact ~capabilities ~allocator () |> ok in
  let prepared = List.hd_exn (EC.prepared_tools definition) in
  let make ?(input = `Null) () =
    I.create
      I.
        { id = P.Id.Invocation.create_with generator
        ; session_id = P.Id.Session.create_with generator
        ; generation = 0
        ; origin = Model
        ; provider_call_id = Some "provider-call"
        ; call_entry_id = None
        ; parent_invocation = None
        ; parent_job = None
        ; tool_name = "counter"
        ; implementation_revision = EC.fingerprint prepared
        ; capability_fingerprint = Cap.fingerprint (EC.capabilities prepared)
        ; input
        ; created_at = P.Timestamp.now ()
        ; deadline = None
        }
    |> protocol
    |> I.dispatch
    |> protocol
  in
  manager, prepared, make
;;

let call
      ?(validate_work = fun _ -> Error "invocation.invalid_work: not owned")
      ?(prepare_resolution = fun ~resolved:_ ~outcome:_ ~snapshot:_ -> Ok ignore)
      manager
      invocation
  =
  M.handle_invocation_entries
    manager
    ~invocation
    ~history:[]
    ~available_tools:[]
    ~session_meta:`Null
    ~now_ms:0
    ~validate_work
    ~prepare_resolution
;;

let state manager = (M.identity_snapshot manager |> ok).current_state

let%test_unit "stateful dedicated tool event commits one result and overlay" =
  Eio_main.run (fun env ->
    let manager, _, make =
      setup
        env
        "Task.bind(Turn.prepend_system(\"handled\"), fun ignored -> \
         Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(p.input)), fun \
         ignored -> Task.pure(state + 1)))"
    in
    let commits = ref 0 in
    let proposed = ref None in
    let prepare_resolution
          ~resolved
          ~outcome
          ~(snapshot : Session.Moderator_state.Identity_snapshot.t)
      =
      proposed := Some snapshot;
      assert (Poly.equal snapshot.current_state (Session.Snapshot.Int 1));
      assert (List.length outcome.Chat_response.Moderation.Outcome.overlay_ops = 1);
      (match resolved.I.status with
       | Resolved (Complete (`String "payload")) -> ()
       | _ -> assert false);
      Ok (fun () -> incr commits)
    in
    let result, _ =
      call ~prepare_resolution manager (make ~input:(`String "payload") ()) |> ok
    in
    assert (!commits = 1);
    assert (Poly.equal (state manager) (Session.Snapshot.Int 1));
    assert (List.length (M.identity_snapshot manager |> ok).prepended_items = 1);
    assert (Poly.equal !proposed (Some (M.identity_snapshot manager |> ok)));
    expect "not_dispatched" (call manager result))
;;

let%test_unit "unhandled duplicate wrong-id schema and host failures roll back" =
  Eio_main.run (fun env ->
    let cases =
      [ "invocation.unhandled", "Task.pure(state + 1)"
      ; ( "invocation.duplicate_resolution"
        , "Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)), fun \
           ignored -> Task.bind(Invocation.resolve(p.context.invocation_id, \
           `Complete(`Null)), fun ignored -> Task.pure(state + 1)))" )
      ; ( "invocation.wrong_id"
        , "Task.bind(Invocation.resolve(\"other-id\", `Complete(`Null)), fun ignored -> \
           Task.pure(state + 1))" )
      ; ( "after resolve"
        , "Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)), fun \
           ignored -> fail(\"after resolve\"))" )
      ]
    in
    List.iter cases ~f:(fun (code, body) ->
      let manager, _, make =
        setup
          env
          ("Task.bind(Turn.prepend_system(\"uncommitted\"), fun ignored -> " ^ body ^ ")")
      in
      expect code (call manager (make ()));
      assert (Poly.equal (state manager) (Session.Snapshot.Int 0));
      assert (List.is_empty (M.identity_snapshot manager |> ok).prepended_items));
    let manager, _, make =
      setup
        env
        ~schema:"{\"type\":\"string\"}"
        "Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)), fun \
         ignored -> Task.pure(state + 1))"
    in
    expect "invalid_output" (call manager (make ~input:(`String "ok") ()));
    let manager, _, make =
      setup
        env
        "Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)), fun \
         ignored -> Task.pure(state + 1))"
    in
    expect
      "host rejection"
      (call
         manager
         (make ())
         ~prepare_resolution:(fun ~resolved:_ ~outcome:_ ~snapshot:_ ->
           Error "host rejection"));
    assert (Poly.equal (state manager) (Session.Snapshot.Int 0));
    ignore (call manager (make ()) |> ok);
    assert (Poly.equal (state manager) (Session.Snapshot.Int 1)))
;;

let%test_unit "pending work requires owner validation and errors bypass success schema" =
  Eio_main.run (fun env ->
    let job = P.Id.Job.create_with generator in
    let manager, _, make =
      setup
        env
        ("Task.bind(Invocation.resolve(p.context.invocation_id, `Pending(`Job(\""
         ^ P.Id.Job.to_string job
         ^ "\"), `Null)), fun ignored -> Task.pure(state + 1))")
    in
    expect "invalid_work" (call manager (make ()));
    let checked = ref 0 in
    ignore
      (call manager (make ()) ~validate_work:(fun work ->
         assert (I.compare_work work (Job job) = 0);
         incr checked;
         Ok ())
       |> ok);
    assert (!checked = 1);
    let manager, _, make =
      setup
        env
        ~schema:"{\"type\":\"string\"}"
        "Task.bind(Invocation.resolve(p.context.invocation_id, `Fail({code = \
         \"rejected\"; message = \"reason\"; retryable = false; details = `Null})), fun \
         ignored -> Task.pure(state + 1))"
    in
    let resolved, _ = call manager (make ~input:(`String "ok") ()) |> ok in
    match resolved.I.status with
    | Resolved (Fail _) -> ()
    | _ -> assert false)
;;

let%test_unit "emit and schedule JSON become internal data events" =
  Eio_main.run (fun env ->
    let scheduled = ref None in
    let capabilities =
      { Chat_response.Moderation.Capabilities.default with
        on_schedule_after_ms =
          (fun ~delay_ms:_ ~payload ->
            scheduled := Some payload;
            Ok "timer")
      }
    in
    let manager, _, make =
      setup
        env
        ~capabilities
        "Task.bind(Runtime.emit(`String(\"Tool_invoked\")), fun ignored -> \
         Task.bind(Schedule.after_ms(1, `String(\"Job_completed\")), fun ignored -> \
         Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)), fun \
         ignored -> Task.pure(state))))"
    in
    ignore (call manager (make ()) |> ok);
    let snapshot = M.identity_snapshot manager |> ok in
    assert (
      Poly.equal
        snapshot.queued_internal_events
        [ Session.Snapshot.Variant
            ("Internal_event", [ Variant ("String", [ String "Tool_invoked" ]) ])
        ]);
    match !scheduled with
    | Some
        (L.VVariant
           ("Internal_event", [ VVariant ("String", [ VString "Job_completed" ]) ])) -> ()
    | _ -> assert false)
;;

let%test_unit "concurrent calls serialize moderator state" =
  Eio_main.run (fun env ->
    let manager, _, make =
      setup
        env
        "Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(p.input)), fun \
         ignored -> Task.pure(state + 1))"
    in
    let invocations = List.init 8 ~f:(fun _ -> make ()) in
    Eio.Fiber.all
      (List.map invocations ~f:(fun invocation () ->
         ignore (call manager invocation |> ok)));
    assert (Poly.equal (state manager) (Session.Snapshot.Int 8)))
;;

let%test_unit "persistence proposal includes the complete queue halt and overlay" =
  Eio_main.run (fun env ->
    let manager, _, make =
      setup
        env
        ~initial:"[0]"
        {|let ignored = state[0] <- state[0] + 1 in
          Task.bind(Turn.prepend_system("saved overlay"), fun ignored ->
          Task.bind(Runtime.emit(`String("new event")), fun ignored ->
          Task.bind(Runtime.end_session("finished"), fun ignored ->
          Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)),
            fun ignored -> Task.pure(state)))))|}
    in
    M.enqueue_internal_event
      manager
      (L.VVariant ("Internal_event", [ VVariant ("String", [ VString "existing event" ]) ]))
    |> ok;
    let before = M.identity_snapshot manager |> ok in
    let proposal = ref None in
    let wakeups = ref 0 in
    let subscription =
      M.subscribe_committed_changes manager ~on_wakeup:(fun () -> incr wakeups)
    in
    let prepare_resolution
          ~resolved:_
          ~outcome:_
          ~(snapshot : Session.Moderator_state.Identity_snapshot.t)
      =
      proposal := Some snapshot;
      assert (Poly.equal snapshot.current_state (Session.Snapshot.Array [ Int 1 ]));
      assert snapshot.halted;
      assert (List.length snapshot.prepended_items = 1);
      assert (
        Poly.equal
          snapshot.queued_internal_events
          (before.queued_internal_events
           @ [ Session.Snapshot.Variant
                 ("Internal_event", [ Variant ("String", [ String "new event" ]) ])
             ]));
      Error "persistence unavailable"
    in
    expect "persistence unavailable" (call ~prepare_resolution manager (make ()));
    assert (Poly.equal before (M.identity_snapshot manager |> ok));
    assert (!wakeups = 0);
    assert (List.is_empty (M.drain_committed_changes subscription));
    let rejected = Option.value_exn !proposal in
    (* The snapshot is detached from mutable handler state. A later attempt may
       allocate different overlay IDs, but cannot mutate the rejected proposal. *)
    assert (Poly.equal rejected.current_state (Session.Snapshot.Array [ Int 1 ]));
    let committed = ref None in
    ignore
      (call manager (make ()) ~prepare_resolution:(fun ~resolved:_ ~outcome:_ ~snapshot ->
         committed := Some snapshot;
         Ok ignore)
       |> ok);
    assert (Poly.equal !committed (Some (M.identity_snapshot manager |> ok)));
    assert (!wakeups = 1);
    assert (List.length (M.drain_committed_changes subscription) = 1);
    M.unsubscribe subscription;
    assert (Poly.equal rejected.current_state (Session.Snapshot.Array [ Int 1 ])))
;;

let%test_unit "exception or cancellation in persistence preparation installs nothing" =
  Eio_main.run (fun env ->
    let manager, _, make =
      setup
        env
        ~initial:"[0]"
        {|let ignored = state[0] <- state[0] + 1 in
          Task.bind(Runtime.emit(`Null), fun ignored ->
          Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)),
            fun ignored -> Task.pure(state)))|}
    in
    let before = M.identity_snapshot manager |> ok in
    (match
       call
         manager
         (make ())
         ~prepare_resolution:(fun ~resolved:_ ~outcome:_ ~snapshot:_ -> raise Exit)
     with
     | _ -> assert false
     | exception Exit -> ());
    assert (Poly.equal before (M.identity_snapshot manager |> ok));
    let entered = ref false in
    let never, _ = Eio.Promise.create () in
    (match
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 0.05 (fun () ->
         call
           manager
           (make ())
           ~prepare_resolution:(fun ~resolved:_ ~outcome:_ ~snapshot:_ ->
             entered := true;
             Eio.Promise.await never))
     with
     | _ -> assert false
     | exception Eio.Time.Timeout -> ());
    assert !entered;
    assert (Poly.equal before (M.identity_snapshot manager |> ok));
    ignore (call manager (make ()) |> ok);
    assert (Poly.equal (state manager) (Session.Snapshot.Array [ Int 1 ])))
;;

let%test_unit "host exception clears active execution and rolls back array state" =
  Eio_main.run (fun env ->
    let capabilities =
      { Chat_response.Moderation.Capabilities.default with
        on_tool_call = (fun ~name:_ ~args:_ -> raise Exit)
      }
    in
    let manager, _, make =
      setup
        env
        ~capabilities
        "Task.bind(Tool.call(\"cancel\", `Null), fun ignored -> \
         Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)), fun \
         ignored -> Task.pure(state + 1)))"
    in
    (match call manager (make ()) with
     | _ -> assert false
     | exception Exit -> ());
    assert (Poly.equal (state manager) (Session.Snapshot.Int 0));
    (* A second call reaches the host callback instead of 'already handling'. *)
    (match call manager (make ()) with
     | _ -> assert false
     | exception Exit -> ());
    let manager, _, make =
      setup env ~initial:"[0]" "let ignored = state[0] <- 7 in Task.pure(state)"
    in
    expect "invocation.unhandled" (call manager (make ()));
    assert (Poly.equal (state manager) (Session.Snapshot.Array [ Int 0 ])))
;;

let%test_unit "Eio cancellation rolls back state and leaves the manager usable" =
  Eio_main.run (fun env ->
    let entered = ref false in
    let never, _ = Eio.Promise.create () in
    let capabilities =
      { Chat_response.Moderation.Capabilities.default with
        on_tool_call =
          (fun ~name:_ ~args:_ ->
            if not !entered
            then (
              entered := true;
              Eio.Promise.await never)
            else Ok (Tool_ok `Null))
      }
    in
    let manager, _, make =
      setup
        env
        ~initial:"[0]"
        ~capabilities
        {|let ignored = state[0] <- state[0] + 1 in
          Task.bind(Tool.call("wait", `Null), fun ignored ->
          Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)),
            fun ignored -> Task.pure(state)))|}
    in
    (match
       Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 1. (fun () ->
         call manager (make ()))
     with
     | _ -> failwith "expected cancellation during the tool callback"
     | exception Eio.Time.Timeout -> ());
    assert !entered;
    assert (Poly.equal (state manager) (Session.Snapshot.Array [ Int 0 ]));
    ignore (call manager (make ()) |> ok);
    assert (Poly.equal (state manager) (Session.Snapshot.Array [ Int 1 ])))
;;

let%test_unit "stale bindings and invalid input reject before handler execution" =
  Eio_main.run (fun env ->
    let manager, _, make =
      setup
        env
        ~schema:"{\"type\":\"string\"}"
        {|Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(p.input)),
            fun ignored -> Task.pure(state + 1))|}
    in
    expect "invocation.invalid_input" (call manager (make ()));
    let original = make ~input:(`String "ok") () in
    let stale =
      I.create { original.context with implementation_revision = "stale" }
      |> protocol
      |> I.dispatch
      |> protocol
    in
    expect "invocation.stale_binding" (call manager stale);
    assert (Poly.equal (state manager) (Session.Snapshot.Int 0)))
;;

let%test_unit "manager callbacks reject reentry and leave the manager usable" =
  Eio_main.run (fun env ->
    let manager_ref = ref None in
    let reenter = ref true in
    let capabilities =
      { Chat_response.Moderation.Capabilities.default with
        on_tool_call =
          (fun ~name:_ ~args:_ ->
            if !reenter
            then (
              reenter := false;
              match M.identity_snapshot (Option.value_exn !manager_ref) with
              | Error error -> Error error
              | Ok _ -> failwith "expected moderator reentry rejection")
            else Ok (Tool_ok `Null))
      }
    in
    let manager, _, make =
      setup
        env
        ~capabilities
        {|Task.bind(Tool.call("reenter", `Null), fun ignored ->
        Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)),
          fun ignored -> Task.pure(state + 1)))|}
    in
    manager_ref := Some manager;
    expect "moderator_reentrancy" (call manager (make ()));
    assert (Poly.equal (state manager) (Session.Snapshot.Int 0));
    call manager (make ()) |> ok |> ignore;
    assert (Poly.equal (state manager) (Session.Snapshot.Int 1)))
;;

let%test_unit "dispatch applies the effective array limit before projection" =
  Eio_main.run (fun env ->
    let _, prepared, make =
      setup
        env
        {|Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(p.input)),
            fun ignored -> Task.pure(state))|}
    in
    let invocation = make ~input:(`Array [ `Null; `Null ]) () in
    expect
      "array item limit"
      (MI.create
         ~prepared
         ~invocation
         ~limits:
           { Chatmd_shell_spec.Chatmd_script_spec.default_limits with
             max_array_items = 1
           }
         ~validate_work:(fun _ -> Error "not owned")))
;;
