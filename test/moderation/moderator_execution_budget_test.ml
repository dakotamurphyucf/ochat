open Core
module M = Chat_response.Moderator_manager
module X = Chatml_execution
module L = Chatml.Chatml_lang
module S = Session.Moderator_state.Identity_snapshot

let ok = Result.ok_or_failwith

let artifact env source =
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir:(Eio.Stdenv.cwd env)
      ("<script id=\"bounded\" language=\"chatml\" kind=\"moderator\" \
        api=\"extensibility-v1\">"
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

let create env ?(policy = X.Bounded { X.default_limits with fuel = 3000 }) source =
  let allocator =
    History_entry.Allocator.create ~namespace:"budget-fixture" ~next_sequence:0 |> ok
  in
  M.create_entries
    ~env
    ~execution_policy:policy
    ~artifact:(artifact env source)
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
          Ok (fun () -> incr installs))
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
        ~prepare_event:(fun ~outcome:_ ~snapshot:_ -> Ok (fun () -> incr commits))
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
