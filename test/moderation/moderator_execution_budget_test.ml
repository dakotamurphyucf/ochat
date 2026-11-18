open Core
module M = Chat_response.Moderator_manager
module X = Chatml_execution
module L = Chatml.Chatml_lang
module S = Session.Moderator_state.Identity_snapshot

let ok = Result.ok_or_failwith

let artifact env ?(script_limits = "") source =
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir:(Eio.Stdenv.cwd env)
      ("<script id=\"bounded\" language=\"chatml\" kind=\"moderator\" \
        api=\"extensibility-v1\" "
       ^ script_limits
       ^ ">"
       ^ source
       ^ "</script>")
  in
  let capabilities =
    Chat_response.Tool_capability.create
      ~owner:"budget-fixture"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "budget-fixture")
      []
    |> Result.map_error ~f:(fun error -> error.Chat_response.Tool_capability.message)
    |> ok
  in
  let definition =
    Chat_response.Extension_compiler.prepare_definition_in_domain
      ~env
      ~capabilities
      elements
    |> Result.map_error ~f:(fun errors ->
      String.concat ~sep:"; " (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
    |> ok
  in
  let _, artifact = M.Registry.of_definition M.Registry.empty definition |> ok in
  Option.value_exn artifact
;;

let create
      env
      ?(policy = X.Bounded { X.default_limits with fuel = 3000 })
      ?script_limits
      source
  =
  let allocator =
    History_entry.Allocator.create ~namespace:"budget-fixture" ~next_sequence:0 |> ok
  in
  M.create_entries
    ~env
    ~execution_policy:policy
    ~artifact:(artifact env ?script_limits source)
    ~capabilities:Chat_response.Moderation.Capabilities.default
    ~allocator
    ()
;;

let code = function
  | Ok _ -> "ok"
  | Error message -> String.lsplit2_exn message ~on:':' |> fst
;;

let snapshot manager = M.identity_snapshot manager |> ok
let same_snapshot a b = Sexp.equal (S.sexp_of_t a) (S.sexp_of_t b)

let%expect_test "legacy manager diagnostics cannot recurse forever or invalidate a commit"
  =
  let module Debug = Chatml.Chatml_debug_log in
  let script : Prompt.Chat_markdown.script =
    { id = "logging"
    ; language = "chatml"
    ; project_selection = None
    ; kind = "moderator"
    ; source_ref = None
    ; source =
        Inline
          "let initial_state = 0\nlet on_event ctx state event = Task.pure(state + 1)"
    }
  in
  let _, artifact = M.Registry.compile_script M.Registry.empty script |> ok in
  let manager =
    M.create ~artifact ~capabilities:Chat_response.Moderation.Capabilities.default ()
    |> ok
  in
  let cell = ref L.VUnit in
  cell := L.VRef cell;
  let deliver () =
    M.handle_event
      manager
      ~session_id:"logging"
      ~now_ms:0
      ~history:[]
      ~available_tools:[]
      ~session_meta:`Null
      ~event:(Internal_event (L.VRef cell))
  in
  Debug.clear_sink ();
  print_endline (code (deliver ()));
  let sink_failures = ref 0 in
  Debug.set_sink (fun line ->
    if String.is_prefix line ~prefix:"[moderator-manager] handle_event_ok"
    then (
      incr sink_failures;
      failwith "diagnostic sink unavailable"));
  Exn.protect ~finally:Debug.clear_sink ~f:(fun () ->
    let outcome = deliver () in
    let saved = M.snapshot manager |> ok in
    print_s
      [%sexp
        (code outcome : string)
      , (saved.current_state : Session.Snapshot.t)
      , (!sink_failures : int)]);
  [%expect
    {|
    ok
    (ok (Int 2) 1)
    |}]
;;

let%expect_test "host observations keep observer budgets separate from retained state" =
  Eio_main.run (fun env ->
    let event =
      Chat_response.Moderation.Event.Item_appended
        (Chat_response.Moderation.Item.create
           ~id:"large-observation"
           ~value:(`String (String.make 4096 'x')))
    in
    let source ~retain =
      {|let initial_state = ""
        let on_event ctx state event = match event with
        | `Item_appended(item) -> (match item.value with
          | `String(value) -> Task.pure(|}
      ^ (if retain then "value" else "\"small\"")
      ^ {|)
          | _ -> Task.pure(state))
        | _ -> Task.pure(state)|}
    in
    let manager
          ?(policy = X.Bounded { X.default_limits with max_value_bytes = 64 * 1024 })
          ~retain
          ()
      =
      create env ~policy ~script_limits:{|max_value="1KiB"|} (source ~retain) |> ok
    in
    let owned manager installs =
      M.handle_event_entries_transactional
        manager
        ~session_id:"budget-fixture"
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~event
        ~authorize:(fun () -> Ok ())
        ~on_tool_call:(fun ~name:_ ~args:_ -> failwith "unexpected tool call")
        ~prepare_event:(fun ~outcome:_ ~snapshot:_ ->
          Ok (M.memory_commit (fun () -> incr installs)))
    in
    List.iter [ `Raw; `Owned ] ~f:(fun route ->
      List.iter [ false; true ] ~f:(fun retain ->
        let manager = manager ~retain () in
        let before = snapshot manager in
        let installs = ref 0 in
        let result =
          match route with
          | `Raw ->
            M.handle_event_entries
              manager
              ~session_id:"budget-fixture"
              ~now_ms:0
              ~history:[]
              ~available_tools:[]
              ~session_meta:`Null
              ~event
          | `Owned -> owned manager installs
        in
        let unchanged = same_snapshot before (snapshot manager) in
        assert (Bool.equal unchanged retain);
        (match retain with
         | true -> assert (String.equal (code result) "invocation.invalid_state")
         | false ->
           ignore (ok result : Chat_response.Moderation.Outcome.t);
           (match (snapshot manager).current_state with
            | Session.Snapshot.String value -> assert (String.equal value "small")
            | _ -> failwith "observer did not retain its small state"));
        let owned_installs =
          match route with
          | `Raw -> None
          | `Owned ->
            assert (!installs = Bool.to_int (not retain));
            Some !installs
        in
        print_s
          [%sexp
            (route : [ `Raw | `Owned ])
          , (retain : bool)
          , (code result : string)
          , (unchanged : bool)
          , (owned_installs : int option)]));
    let manager = manager ~policy:X.Unrestricted ~retain:false () in
    let before = snapshot manager in
    let installs = ref 0 in
    let blocked =
      X.with_control
        ~env
        ~policy:(Bounded { X.default_limits with max_value_bytes = 2048 })
        (fun _ -> owned manager installs)
      |> Result.map_error ~f:(fun error -> error.X.code ^ ": " ^ error.message)
      |> Result.join
    in
    assert (same_snapshot before (snapshot manager));
    assert (!installs = 0);
    let after_ancestor = owned manager installs in
    ignore (ok after_ancestor : Chat_response.Moderation.Outcome.t);
    assert (!installs = 1);
    print_s [%sexp (code blocked : string), (code after_ancestor : string)]);
  [%expect
    {|
    (Raw false ok false ())
    (Raw true invocation.invalid_state true ())
    (Owned false ok false (1))
    (Owned true invocation.invalid_state true (0))
    (chatml.value_limit ok)
    |}]
;;

let%expect_test "event projection and state copying fail before tools or persistence" =
  Eio_main.run (fun env ->
    List.iter [ "state"; "context"; "event"; "allowed" ] ~f:(fun mode ->
      let payload =
        match mode with
        | "state" | "allowed" -> String.make 12_800 'x'
        | _ -> ""
      in
      let source =
        "let initial_state = { count = 0; payload = \""
        ^ payload
        ^ "\" }\n"
        ^ "let on_event ctx state event = Task.bind(Tool.call(\"native\", `Null), "
        ^ "fun ignored -> Task.pure({ state with count = state.count + 1 }))"
      in
      let limits =
        { X.default_limits with
          allocation_bytes =
            (match mode with
             | "allowed" -> X.default_limits.allocation_bytes
             | _ -> 8192)
        ; max_invocation_depth = 1
        }
      in
      let manager = create env ~policy:(Bounded limits) source |> ok in
      let before = snapshot manager in
      let native_calls = ref 0 in
      let installs = ref 0 in
      let outcome =
        M.handle_event_entries_transactional
          manager
          ~session_id:"budget-fixture"
          ~now_ms:0
          ~history:[]
          ~available_tools:[]
          ~session_meta:
            (match mode with
             | "context" -> `String (String.make 12_800 'x')
             | _ -> `Null)
          ~event:
            (match mode with
             | "event" ->
               Item_appended
                 (Chat_response.Moderation.Item.create
                    ~id:"large"
                    ~value:(`String (String.make 12_800 'x')))
             | _ -> Session_start)
          ~authorize:(fun () -> Ok ())
          ~on_tool_call:(fun ~name:_ ~args:_ ->
            incr native_calls;
            Ok (Tool_ok `Null))
          ~prepare_event:(fun ~outcome:_ ~snapshot:_ ->
            Ok (M.memory_commit (fun () -> incr installs)))
      in
      print_s
        [%sexp
          (mode : string)
        , (code outcome : string)
        , (!native_calls : int)
        , (!installs : int)
        , (same_snapshot before (snapshot manager) : bool)]));
  [%expect
    {|
    (state chatml.allocation_limit 0 0 true)
    (context chatml.allocation_limit 0 0 true)
    (event chatml.allocation_limit 0 0 true)
    (allowed ok 1 1 false)
    |}]
;;

let%expect_test "initialization is bounded before a persistent runtime becomes usable" =
  Eio_main.run (fun env ->
    let recursive =
      {|let rec loop n = loop(n + 1)
let initial_state = loop(0)
let on_event ctx state event = Task.pure(state)|}
    in
    let allocation =
      {|let initial_state = Array.make(10000, 0)
let on_event ctx state event = Task.pure(state)|}
    in
    print_s
      [%sexp
        (code (create env recursive) : string)
      , (code
           (create
              env
              ~policy:(Bounded { X.default_limits with allocation_bytes = 1024 })
              allocation)
         : string)];
    let compiled = artifact env recursive in
    print_s
      [%sexp
        (code
           (M.create
              ~artifact:compiled
              ~capabilities:Chat_response.Moderation.Capabilities.default
              ())
         : string)]);
  [%expect
    {|
    (chatml.execution_limit chatml.allocation_limit)
    chatml.execution_host_required
    |}]
;;

let source =
  {|let rec loop n = loop(n + 1)
let rec count n = if n == 0 then 0 else 1 + count(n - 1)
let initial_state = [0]
let on_event ctx state event = match event with
| `Session_start ->
  let ignored = state[0] <- state[0] + 1 in
  Task.bind(Runtime.emit(`String("uncommitted")), fun ignored ->
  Task.bind(Tool.call("native", `Null), fun ignored ->
    let ignored = loop(0) in Task.pure(state)))
| `Internal_event(payload) ->
  let ignored = state[0] <- state[0] + 1 in
  let ignored = count(80) in
  Task.bind(Runtime.emit(payload), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)|}
;;

let%expect_test "failed pure continuations roll back and later events get fresh scopes" =
  Eio_main.run (fun env ->
    let manager = create env source |> ok in
    let before = snapshot manager in
    let native_calls = ref 0 in
    let installs = ref 0 in
    let saved = ref before in
    let deliver event =
      M.handle_event_entries_transactional
        manager
        ~session_id:"budget-fixture"
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~event
        ~authorize:(fun () -> Ok ())
        ~on_tool_call:(fun ~name:_ ~args:_ ->
          incr native_calls;
          Ok (Tool_ok `Null))
        ~prepare_event:(fun ~outcome:_ ~snapshot ->
          saved := snapshot;
          Ok (M.memory_commit (fun () -> incr installs)))
    in
    let failure = code (deliver Session_start) in
    print_s
      [%sexp
        (failure : string)
      , (!native_calls : int)
      , (!installs : int)
      , (same_snapshot before (snapshot manager) : bool)];
    let event =
      Chat_response.Moderator_invocation.internal_event (L.VVariant ("Null", [])) |> ok
    in
    let first = code (deliver (Internal_event event)) in
    let second = code (deliver (Internal_event event)) in
    let after = snapshot manager in
    print_s
      [%sexp
        (first : string)
      , (second : string)
      , (!installs : int)
      , (same_snapshot !saved after : bool)
      , (after.current_state : Session.Snapshot.t)
      , (List.length after.queued_internal_events : int)]);
  [%expect
    {|
    (chatml.execution_limit 1 0 true)
    (ok ok 2 true (Array ((Int 2))) 2)
    |}]
;;

let%expect_test "pure event evaluation observes Eio cancellation and releases the runtime"
  =
  Eio_main.run (fun env ->
    let manager =
      create env ~policy:(Bounded { X.default_limits with fuel = 10_000_000 }) source
      |> ok
    in
    let before = snapshot manager in
    let entered, enter = Eio.Promise.create () in
    let commits = ref 0 in
    let deliver event =
      M.handle_event_entries_transactional
        manager
        ~session_id:"budget-fixture"
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~event
        ~authorize:(fun () -> Ok ())
        ~on_tool_call:(fun ~name:_ ~args:_ ->
          Eio.Promise.resolve enter ();
          Ok (Tool_ok `Null))
        ~prepare_event:(fun ~outcome:_ ~snapshot:_ ->
          Ok (M.memory_commit (fun () -> incr commits)))
    in
    let cancelled =
      try
        Eio.Fiber.both
          (fun () -> deliver Session_start |> ok |> ignore)
          (fun () ->
             Eio.Promise.await entered;
             Eio.Fiber.yield ();
             raise Exit);
        false
      with
      | Exit -> true
    in
    let rolled_back = same_snapshot before (snapshot manager) in
    let event =
      Chat_response.Moderator_invocation.internal_event (L.VVariant ("Null", [])) |> ok
    in
    let next = code (deliver (Internal_event event)) in
    print_s
      [%sexp (cancelled : bool), (rolled_back : bool), (next : string), (!commits : int)]);
  [%expect {| (true true ok 1) |}]
;;
