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
      ?(events = "| _ -> Task.pure(state)")
      ?(script_limits = "")
      body
  =
  let dir = Eio.Stdenv.cwd env in
  let source =
    "let initial_state = "
    ^ initial
    ^ "\nlet on_event = fun ctx state event -> match event with\n| `Tool_invoked(p) -> "
    ^ body
    ^ "\n"
    ^ events
  in
  let loader =
    Source_loader.captured_filesystem ~root:dir ~sources:[ "schema.json", schema ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir
      ~source_loader:loader
      ("<script id=\"handler\" language=\"chatml\" kind=\"moderator\" \
        api=\"extensibility-v1\" "
       ^ script_limits
       ^ ">"
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

let%expect_test
    "ordinary events hand off complete snapshots before installing local effects"
  =
  let module S = Session.Moderator_state.Identity_snapshot in
  let same_snapshot left right = Sexp.equal (S.sexp_of_t left) (S.sexp_of_t right) in
  List.iter [ `Commit; `Reject; `Raise; `Cancel ] ~f:(fun mode ->
    Eio_main.run (fun env ->
      let fallback_calls = ref 0 in
      let capabilities =
        { Chat_response.Moderation.Capabilities.default with
          on_tool_call =
            (fun ~name:_ ~args:_ ->
              incr fallback_calls;
              Ok (Tool_ok `Null))
        }
      in
      let manager, _, _ =
        setup
          env
          ~capabilities
          ~initial:"[0]"
          "Task.pure(state)"
          ~events:
            {| | `Session_start ->
              let ignored = state[0] <- state[0] + 1 in
              Task.bind(Tool.call("native", `Null), fun ignored ->
              Task.bind(Turn.prepend_system("saved overlay"), fun ignored ->
              Task.bind(Runtime.emit(`String("new event")), fun ignored ->
              Task.bind(Runtime.end_session("finished"), fun ignored -> Task.pure(state)))))
            | `Turn_start -> Task.bind(Tool.call("fallback", `Null), fun ignored -> Task.pure(state))
            | _ -> Task.pure(state) |}
      in
      let queued =
        MI.internal_event (L.VVariant ("String", [ VString "existing" ])) |> ok
      in
      M.enqueue_internal_event manager queued |> ok;
      let before = M.identity_snapshot manager |> ok in
      let saved = ref before in
      let proposal = ref None in
      let native_calls = ref 0 in
      let installs = ref 0 in
      let wakeups = ref 0 in
      let subscription =
        M.subscribe_committed_changes manager ~on_wakeup:(fun () -> incr wakeups)
      in
      let entered, enter = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let prepare_event ~outcome ~(snapshot : S.t) =
        proposal := Some snapshot;
        assert (!installs = 0 && !wakeups = 0 && !native_calls = 1);
        (match snapshot.current_state with
         | Session.Snapshot.Array [ Int 1 ] -> ()
         | _ -> failwith "proposal lost array mutation");
        assert snapshot.halted;
        assert (List.length snapshot.prepended_items = 1);
        assert (
          List.compare
            Session.Snapshot.compare
            snapshot.queued_internal_events
            (before.queued_internal_events
             @ [ Session.Snapshot.Variant
                   ("Internal_event", [ Variant ("String", [ String "new event" ]) ])
               ])
          = 0);
        assert (
          Option.is_some
            (Chat_response.Runtime_semantics.should_end_session
               outcome.Chat_response.Moderation.Outcome.runtime_requests));
        match mode with
        | `Commit ->
          (* Exercise the persisted snapshot codec before restoring another manager. *)
          saved := Binable.of_string (module S) (Binable.to_string (module S) snapshot);
          Ok (fun () -> incr installs)
        | `Reject -> Error "event save rejected"
        | `Raise -> raise Exit
        | `Cancel ->
          Eio.Promise.resolve enter ();
          Eio.Promise.await never
      in
      let execute () =
        M.handle_event_entries_transactional
          manager
          ~session_id:"event-fixture"
          ~now_ms:0
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Session_start
          ~authorize:(fun () -> Ok ())
          ~on_tool_call:(fun ~name:_ ~args:_ ->
            incr native_calls;
            Ok (Tool_ok `Null))
          ~prepare_event
      in
      (match mode with
       | `Commit -> execute () |> ok |> ignore
       | `Reject -> expect "event save rejected" (execute ())
       | `Raise ->
         (match execute () with
          | exception Exit -> ()
          | _ -> assert false)
       | `Cancel ->
         (match
            Eio.Fiber.first execute (fun () ->
              Eio.Promise.await entered;
              raise Exit)
          with
          | exception Exit -> ()
          | _ -> assert false));
      assert (!native_calls = 1 && !fallback_calls = 0);
      let after = M.identity_snapshot manager |> ok in
      assert (same_snapshot after !saved);
      let changes = M.drain_committed_changes subscription in
      let committed =
        match mode with
        | `Commit -> true
        | _ -> false
      in
      assert (Bool.equal committed (not (same_snapshot before after)));
      assert (!installs = Bool.to_int committed);
      assert (!wakeups = !installs && List.length changes = !installs);
      M.unsubscribe subscription;
      let definition = M.extension_definition manager |> Option.value_exn in
      let _, artifact = M.Registry.of_definition M.Registry.empty definition |> ok in
      let allocator =
        History_entry.Allocator.create ~namespace:"restored" ~next_sequence:0 |> ok
      in
      let restored =
        M.create_entries
          ~artifact:(Option.value_exn artifact)
          ~capabilities
          ~allocator
          ~snapshot:!saved
          ()
        |> ok
      in
      assert (same_snapshot !saved (M.identity_snapshot restored |> ok));
      (* Failure neither retries the native call nor leaves its scoped callback installed. *)
      (match mode with
       | `Commit -> ()
       | `Reject | `Raise | `Cancel ->
         M.handle_event_entries
           manager
           ~session_id:"event-fixture"
           ~now_ms:0
           ~history:[]
           ~available_tools:[]
           ~session_meta:`Null
           ~event:Turn_start
         |> ok
         |> ignore;
         assert (!fallback_calls = 1 && !native_calls = 1);
         assert (same_snapshot before (M.identity_snapshot manager |> ok)));
      assert (Option.is_some !proposal);
      print_s
        [%sexp
          (mode : [ `Commit | `Reject | `Raise | `Cancel ])
        , { native_calls = (!native_calls : int)
          ; installs = (!installs : int)
          ; committed : bool
          ; restored_halted = (M.is_halted restored |> ok : bool)
          }]));
  [%expect
    {|
    (Commit
     ((native_calls 1) (installs 1) (committed true) (restored_halted true)))
    (Reject
     ((native_calls 1) (installs 0) (committed false) (restored_halted false)))
    (Raise
     ((native_calls 1) (installs 0) (committed false) (restored_halted false)))
    (Cancel
     ((native_calls 1) (installs 0) (committed false) (restored_halted false)))
    |}]
;;

let%expect_test
    "ordinary event admission cannot forge invocation events or resolve a tool"
  =
  Eio_main.run (fun env ->
    let manager, _, _ =
      setup
        env
        ~initial:"[0]"
        "fail(\"forged invocation reached handler\")"
        ~events:
          {| | `Session_start ->
            let ignored = state[0] <- state[0] + 1 in
            Task.bind(Runtime.emit(`Null), fun ignored ->
            Task.bind(Invocation.resolve("not-dispatched", `Complete(`Null)), fun ignored -> Task.pure(state)))
          | `Internal_event(payload) ->
            let ignored = state[0] <- state[0] + 1 in
            Task.bind(Runtime.emit(payload), fun ignored -> Task.pure(state))
          | _ -> Task.pure(state) |}
    in
    let before = M.identity_snapshot manager |> ok in
    let authorizations = ref 0 in
    let preparations = ref 0 in
    let saved = ref before in
    let deliver ~event ~allowed =
      M.handle_event_entries_transactional
        manager
        ~session_id:"event-fixture"
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~event
        ~authorize:(fun () ->
          incr authorizations;
          match allowed with
          | true -> Ok ()
          | false -> Error "event owner revoked")
        ~on_tool_call:(fun ~name:_ ~args:_ -> failwith "unexpected native call")
        ~prepare_event:(fun ~outcome:_ ~snapshot ->
          incr preparations;
          saved := snapshot;
          Ok ignore)
    in
    expect
      "invalid_internal_event"
      (deliver ~event:(Internal_event (L.VVariant ("Tool_invoked", []))) ~allowed:true);
    assert (!authorizations = 0 && !preparations = 0);
    expect "event owner revoked" (deliver ~event:Session_start ~allowed:false);
    expect "invalid in phase 'session_start'" (deliver ~event:Session_start ~allowed:true);
    assert (!authorizations = 2 && !preparations = 0);
    let module S = Session.Moderator_state.Identity_snapshot in
    assert (
      Sexp.equal (S.sexp_of_t before) (S.sexp_of_t (M.identity_snapshot manager |> ok)));
    (* A JSON string naming a privileged event stays data inside Internal_event. *)
    let event =
      MI.internal_event (L.VVariant ("String", [ VString "Tool_invoked" ])) |> ok
    in
    deliver ~event:(Internal_event event) ~allowed:true |> ok |> ignore;
    let after = M.identity_snapshot manager |> ok in
    assert (Sexp.equal (S.sexp_of_t !saved) (S.sexp_of_t after));
    print_s
      [%sexp
        { authorizations = (!authorizations : int)
        ; preparations = (!preparations : int)
        ; state = (after.current_state : Session.Snapshot.t)
        ; queue = (after.queued_internal_events : Session.Snapshot.t list)
        }]);
  [%expect
    {|
    ((authorizations 3) (preparations 1) (state (Array ((Int 1))))
     (queue
      ((Variant Internal_event ((Variant String ((String Tool_invoked))))))))
    |}]
;;

let%expect_test
    "queued event checkpoint consumes only the head and isolates mutable aliases"
  =
  let module S = Session.Moderator_state.Identity_snapshot in
  let same left right = Sexp.equal (S.sexp_of_t left) (S.sexp_of_t right) in
  List.iter [ `Commit; `Reject; `Cancel ] ~f:(fun mode ->
    Eio_main.run (fun env ->
      let manager, _, _ =
        setup
          env
          ~initial:"[`String(\"original\")]"
          "Task.pure(state)"
          ~events:
            {| | `Session_start ->
              Task.bind(Runtime.emit(`Array(state)), fun ignored ->
              Task.bind(Runtime.emit(`Array(state)), fun ignored -> Task.pure(state)))
            | `Internal_event(`Array(payload)) ->
              let ignored = state[0] <- `String("state changed") in
              let ignored = payload[0] <- `String("payload changed") in
              Task.bind(Tool.call("native", `Array(payload)), fun ignored ->
              Task.bind(Runtime.emit(`String("emitted")), fun ignored -> Task.pure(state)))
            | _ -> Task.pure(state) |}
      in
      M.handle_event_entries_transactional
        manager
        ~session_id:"queue-fixture"
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~event:Session_start
        ~authorize:(fun () -> Ok ())
        ~on_tool_call:(fun ~name:_ ~args:_ -> failwith "startup must not call tools")
        ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok ignore)
      |> ok
      |> ignore;
      let before = M.identity_snapshot manager |> ok in
      let saved = ref before in
      let installs = ref 0 in
      let native_calls = ref 0 in
      let entered, enter = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let on_tool_call ~name:_ ~args =
        assert (Jsonaf.exactly_equal args (`Array [ `String "payload changed" ]));
        incr native_calls;
        Ok (Chat_response.Moderation.Capabilities.Tool_ok `Null)
      in
      let authorize ~event =
        assert (
          Session.Snapshot.compare event (List.hd_exn before.queued_internal_events) = 0);
        Ok ()
      in
      let prepare_event ~outcome:_ ~(snapshot : S.t) =
        (match snapshot.current_state with
         | Array [ Variant ("String", [ String "state changed" ]) ] -> ()
         | _ -> failwith "queued handler state missing");
        let expected_queue =
          List.tl_exn before.queued_internal_events
          @ [ Session.Snapshot.Variant
                ("Internal_event", [ Variant ("String", [ String "emitted" ]) ])
            ]
        in
        assert (
          List.compare
            Session.Snapshot.compare
            snapshot.queued_internal_events
            expected_queue
          = 0);
        match mode with
        | `Commit ->
          saved := Binable.of_string (module S) (Binable.to_string (module S) snapshot);
          Ok (fun () -> incr installs)
        | `Reject -> Error "queue save rejected"
        | `Cancel ->
          Eio.Promise.resolve enter ();
          Eio.Promise.await never
      in
      let consume () =
        M.handle_next_event_entries_transactional
          manager
          ~session_id:"queue-fixture"
          ~now_ms:0
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~authorize
          ~on_tool_call
          ~prepare_event
      in
      (match mode with
       | `Commit -> consume () |> ok |> Option.value_exn |> ignore
       | `Reject -> expect "queue save rejected" (consume ())
       | `Cancel ->
         (match
            Eio.Fiber.first consume (fun () ->
              Eio.Promise.await entered;
              raise Exit)
          with
          | exception Exit -> ()
          | _ -> assert false));
      assert (!native_calls = 1);
      assert (same !saved (M.identity_snapshot manager |> ok));
      let committed =
        match mode with
        | `Commit -> true
        | _ -> false
      in
      assert (!installs = Bool.to_int committed);
      let definition = M.extension_definition manager |> Option.value_exn in
      let _, artifact = M.Registry.of_definition M.Registry.empty definition |> ok in
      let allocator =
        History_entry.Allocator.create ~namespace:"queue-restored" ~next_sequence:0 |> ok
      in
      let restored =
        M.create_entries
          ~artifact:(Option.value_exn artifact)
          ~capabilities:Chat_response.Moderation.Capabilities.default
          ~allocator
          ~snapshot:!saved
          ()
        |> ok
      in
      let commits_after_restore = ref 0 in
      let next () =
        M.handle_next_event_entries_transactional
          restored
          ~session_id:"queue-fixture"
          ~now_ms:0
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~authorize:(fun ~event:_ ->
            match mode with
            | `Commit -> Ok ()
            | `Reject | `Cancel -> Error "failed claim must not replay")
          ~on_tool_call
          ~prepare_event:(fun ~outcome:_ ~snapshot ->
            saved := snapshot;
            Ok (fun () -> incr commits_after_restore))
      in
      (match mode with
       | `Commit ->
         (* One original duplicate and two emitted messages remain; the first
            native call must not be replayed after checkpoint restoration. *)
         for _ = 1 to 3 do
           next () |> ok |> Option.value_exn |> ignore
         done;
         assert (Option.is_none (next () |> ok));
         assert (!native_calls = 2 && !commits_after_restore = 3);
         assert (same !saved (M.identity_snapshot restored |> ok));
         assert (List.is_empty !saved.queued_internal_events)
       | `Reject | `Cancel ->
         expect "failed claim must not replay" (next ());
         assert (!native_calls = 1 && !commits_after_restore = 0);
         assert (same before (M.identity_snapshot restored |> ok)));
      print_s
        [%sexp
          (mode : [ `Commit | `Reject | `Cancel ])
        , { first_installs = (!installs : int)
          ; commits_after_restore = (!commits_after_restore : int)
          ; native_calls = (!native_calls : int)
          ; remaining =
              (List.length (M.identity_snapshot restored |> ok).queued_internal_events
               : int)
          }]));
  [%expect
    {|
    (Commit
     ((first_installs 1) (commits_after_restore 3) (native_calls 2)
      (remaining 0)))
    (Reject
     ((first_installs 0) (commits_after_restore 0) (native_calls 1)
      (remaining 2)))
    (Cancel
     ((first_installs 0) (commits_after_restore 0) (native_calls 1)
      (remaining 2)))
    |}]
;;

let%expect_test "observation handlers can retain coalesced runtime follow-up intent" =
  List.iter [ false; true ] ~f:(fun retain_follow_up ->
    Eio_main.run (fun env ->
      let manager, prepared, make =
        setup
          env
          "Task.pure(state)"
          ~events:
            {| | `Tool_observed(p) ->
          Task.bind(Runtime.request_turn(), fun ignored ->
          Task.bind(Runtime.request_turn(), fun ignored ->
          Task.bind(Runtime.request_compaction(), fun ignored ->
          Task.bind(Runtime.end_session("done"), fun ignored -> Task.pure(state + 1)))))
          | _ -> Task.pure(state) |}
      in
      let context = (make ()).context in
      let script = EC.script prepared in
      let invocation =
        I.create
          ~observer:{ script_id = script.id; source_sha256 = script.source_sha256 }
          { context with
            origin = Moderator
          ; provider_call_id = None
          ; parent_invocation = Some (P.Id.Invocation.create_with generator)
          }
        |> protocol
        |> I.dispatch
        |> protocol
        |> fun invocation ->
        I.resolve
          invocation
          ~session_id:context.session_id
          ~generation:context.generation
          (Complete `Null)
        |> protocol
        |> I.claim_observation
        |> protocol
      in
      let receipt = ref None in
      let outcome =
        M.handle_observation_entries
          manager
          ~retain_follow_up
          ~invocation
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~now_ms:0
          ~prepare_observation:(fun ~observed ~outcome:_ ~snapshot ->
            I.validate_transition ~previous:(Some invocation) observed |> protocol;
            (match snapshot.Session.Moderator_state.Identity_snapshot.current_state with
             | Session.Snapshot.Int 1 -> ()
             | _ -> assert false);
            assert snapshot.halted;
            receipt := Some observed;
            Ok ignore)
        |> ok
      in
      let observed = Option.value_exn !receipt in
      print_s
        [%sexp
          { retained = (retain_follow_up : bool)
          ; requests =
              (outcome.runtime_requests : Chat_response.Moderation.Runtime_request.t list)
          ; follow_up =
              ((Option.value_exn observed.observation).follow_up
               : I.follow_up_status option)
          }];
      assert (M.is_halted manager |> ok)));
  [%expect
    {|
    ((retained false)
     (requests (Request_turn Request_turn Request_compaction (End_session done)))
     (follow_up ()))
    ((retained true)
     (requests (Request_turn Request_turn Request_compaction (End_session done)))
     (follow_up
      ((Pending_follow_up
        ((request_turn true) (request_compaction true) (end_session (done)))))))
    |}]
;;

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

let%test_unit
    "versioned entry events roll back copied state and buffered effects on failure"
  =
  Eio_main.run (fun env ->
    let manager, _, make =
      setup
        env
        ~initial:"[0]"
        ~events:
          {| | `Turn_start ->
        let ignored = state[0] <- 99 in
        Task.bind(Turn.prepend_system("uncommitted"), fun ignored ->
        Task.bind(Runtime.emit(`Null), fun ignored -> Task.fail("entry phase failed")))
        | _ -> Task.pure(state) |}
        {|let ignored = state[0] <- state[0] + 1 in
        Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)),
          fun ignored -> Task.pure(state))|}
    in
    let before = M.identity_snapshot manager |> ok in
    let subscription = M.subscribe_committed_changes manager ~on_wakeup:ignore in
    expect
      "entry phase failed"
      (M.handle_event_entries
         manager
         ~session_id:"entry-phase"
         ~now_ms:0
         ~history:[]
         ~available_tools:[]
         ~session_meta:`Null
         ~event:Chat_response.Moderation.Event.Turn_start);
    assert (Poly.equal before (M.identity_snapshot manager |> ok));
    assert (List.is_empty (M.drain_committed_changes subscription));
    ignore (call manager (make ()) |> ok);
    assert (Poly.equal (state manager) (Session.Snapshot.Array [ Int 1 ]));
    M.unsubscribe subscription)
;;

let%test_unit "halted stream observations do not execute or consume queued work" =
  Eio_main.run (fun env ->
    let manager, _, make =
      setup
        env
        ~events:"| _ -> Task.fail(\"halted handler executed\")"
        {|Task.bind(Runtime.emit(`String("retained")), fun ignored ->
        Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)), fun ignored ->
        Task.bind(Runtime.end_session("done"), fun ignored -> Task.pure(state + 1))))|}
    in
    call manager (make ()) |> ok |> ignore;
    assert (M.is_halted manager |> ok);
    let before = M.identity_snapshot manager |> ok in
    assert (List.length before.queued_internal_events = 1);
    let subscription = M.subscribe_committed_changes manager ~on_wakeup:ignore in
    let event = Chat_response.Moderation.Event.Turn_start in
    let item () =
      M.handle_event
        ~skip_if_halted:true
        manager
        ~session_id:"stopped"
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~event
    in
    let entry () =
      M.handle_event_entries
        ~skip_if_halted:true
        manager
        ~session_id:"stopped"
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~event
    in
    List.iter [ item; entry ] ~f:(fun observe ->
      let outcome = observe () |> ok in
      assert (List.is_empty outcome.overlay_ops);
      assert (List.is_empty outcome.emitted_events);
      assert (Option.is_none outcome.tool_moderation);
      assert (
        Option.is_some
          (Chat_response.Runtime_semantics.should_end_session outcome.runtime_requests)));
    assert (
      List.is_empty
        (M.drain_internal_events
           manager
           ~session_id:"stopped"
           ~now_ms:0
           ~history:[]
           ~available_tools:[]
           ~session_meta:`Null
         |> ok));
    assert (
      List.is_empty
        (M.drain_internal_events_entries
           manager
           ~session_id:"stopped"
           ~now_ms:0
           ~history:[]
           ~available_tools:[]
           ~session_meta:`Null
         |> ok));
    assert (
      Result.is_error
        (M.handle_event
           manager
           ~session_id:"stopped"
           ~now_ms:0
           ~history:[]
           ~available_tools:[]
           ~session_meta:`Null
           ~event));
    assert (Poly.equal before (M.identity_snapshot manager |> ok));
    assert (List.is_empty (M.drain_committed_changes subscription));
    M.unsubscribe subscription)
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

let%test_unit "pure input preparation enforces projection boundaries" =
  Eio_main.run (fun env ->
    let _, prepared, _ =
      setup
        env
        ~script_limits:{|max_array_items="3" max_depth="8" max_value="1KiB"|}
        "Task.pure(state)"
    in
    let limits = EC.execution_limits prepared in
    let prepare = MI.prepare_input ~prepared ~limits in
    let array n = `Array (List.init n ~f:(fun _ -> `Null)) in
    prepare (array limits.max_array_items) |> ok |> ignore;
    expect "array item limit" (prepare (array (limits.max_array_items + 1)));
    let nested n =
      List.fold (List.init n ~f:Fn.id) ~init:`Null ~f:(fun value _ -> `Array [ value ])
    in
    (* JSON arrays project to a variant containing an array: two levels each. *)
    prepare (nested (limits.max_depth / 2)) |> ok |> ignore;
    expect "depth/node limit" (prepare (nested ((limits.max_depth / 2) + 1)));
    let max_bytes =
      Chatmd_shell_spec.Duration.bytes_to_int64 limits.max_value_bytes |> Int64.to_int_exn
    in
    (* The String variant and its contained value consume eight budget bytes. *)
    prepare (`String (String.make (max_bytes - 8) 'x')) |> ok |> ignore;
    expect "value byte limit" (prepare (`String (String.make (max_bytes - 7) 'x'))))
;;
