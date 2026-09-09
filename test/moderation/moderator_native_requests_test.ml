open Core
module M = Chat_response.Moderator_manager
module S = Chat_response.Runtime_request_scope
module R = Chat_response.Moderation.Runtime_request
module Snapshot = Session.Moderator_state.Identity_snapshot

let ok = Result.ok_or_failwith

let create env body =
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs
      ~dir:(Eio.Stdenv.cwd env)
      ({|<script id="native-requests" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = |}
       ^ body
       ^ "</script>")
  in
  let capabilities =
    Chat_response.Tool_capability.create
      ~owner:"request-fixture"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "native request fixture")
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
      List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string |> String.concat ~sep:"; ")
    |> ok
  in
  let _, artifact = M.Registry.of_definition M.Registry.empty definition |> ok in
  let allocator =
    History_entry.Allocator.create ~namespace:"requests" ~next_sequence:0 |> ok
  in
  M.create_entries
    ~env
    ~artifact:(Option.value_exn artifact)
    ~capabilities:Chat_response.Moderation.Capabilities.default
    ~allocator
    ()
  |> ok
;;

let%expect_test
    "native requests join the moderator transaction, including catch and rejected commits"
  =
  Eio_main.run (fun env ->
    List.iter
      [ `Commit; `Catch; `Fail; `Rejected_save; `Bad_phase; `Domain; `Native_error ]
      ~f:(fun mode ->
        let after =
          match mode with
          | `Catch | `Fail -> {|Task.fail("after native effect")|}
          | _ -> {|Task.pure(state + 1)|}
        in
        let call =
          {|Task.bind(Tool.call("probe", `Null), fun ignored -> |} ^ after ^ ")"
        in
        let call =
          match mode with
          | `Catch | `Native_error ->
            "Task.catch(" ^ call ^ ", fun error -> Task.pure(state + 1))"
          | _ -> call
        in
        let body =
          {|Task.bind(Runtime.request_compaction(), fun ignored -> |} ^ call ^ ")"
        in
        let manager = create env body in
        let before = M.identity_snapshot manager |> ok in
        let calls = ref 0
        and prepares = ref 0
        and installs = ref 0 in
        let saved = ref None
        and requests = ref []
        and leaked = ref None in
        let event =
          match mode with
          | `Bad_phase -> Chat_response.Moderation.Event.Session_start
          | _ ->
            Internal_event
              (Chat_response.Moderator_invocation.internal_event
                 (Chatml.Chatml_lang.VVariant ("Null", []))
               |> ok)
        in
        let result =
          M.handle_event_entries_transactional
            manager
            ~session_id:"requests"
            ~now_ms:0
            ~history:[]
            ~available_tools:[]
            ~session_meta:`Null
            ~event
            ~authorize:(fun () -> Ok ())
            ~on_tool_call:(fun ~name:_ ~args:_ ->
              incr calls;
              let context = S.capture () in
              leaked := context;
              let emit () =
                S.emit [ R.Request_turn; End_session "native"; End_session "later" ]
              in
              let emitted =
                match mode with
                | `Domain ->
                  Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
                    S.with_context context emit)
                | _ -> emit ()
              in
              let open Result.Let_syntax in
              let%bind () = emitted in
              match mode with
              | `Native_error -> Error "native effect failed"
              | _ -> Ok (Chat_response.Moderation.Capabilities.Tool_ok `Null))
            ~prepare_event:(fun ~outcome ~snapshot ->
              incr prepares;
              requests := outcome.runtime_requests;
              saved := Some snapshot;
              match mode with
              | `Rejected_save -> Error "snapshot save rejected"
              | _ -> Ok (fun () -> incr installs))
        in
        assert (Result.is_error (S.with_context !leaked (fun () -> S.emit [])));
        let after = M.identity_snapshot manager |> ok in
        (match mode with
         | `Fail | `Rejected_save | `Bad_phase ->
           assert (Sexp.equal (Snapshot.sexp_of_t before) (Snapshot.sexp_of_t after))
         | _ ->
           assert (
             Sexp.equal
               (Snapshot.sexp_of_t (Option.value_exn !saved))
               (Snapshot.sexp_of_t after)));
        (match mode, result with
         | `Bad_phase, Error message ->
           assert (String.is_substring message ~substring:"invalid in phase")
         | `Bad_phase, Ok _ -> failwith "native request bypassed phase checks"
         | _ -> ());
        print_s
          [%sexp
            (mode
             : [ `Commit
               | `Catch
               | `Fail
               | `Rejected_save
               | `Bad_phase
               | `Domain
               | `Native_error
               ])
          , (Result.is_ok result : bool)
          , (!calls : int)
          , (!prepares : int)
          , (!installs : int)
          , (after.current_state : Session.Snapshot.t)
          , (after.halted : bool)
          , (!requests : R.t list)]));
  [%expect
    {|
    (Commit true 1 1 1 (Int 1) true
     (Request_compaction Request_turn (End_session native)))
    (Catch true 1 1 1 (Int 1) false (Request_compaction))
    (Fail false 1 0 0 (Int 0) false ())
    (Rejected_save false 1 1 0 (Int 0) false
     (Request_compaction Request_turn (End_session native)))
    (Bad_phase false 1 0 0 (Int 0) false ())
    (Domain true 1 1 1 (Int 1) true
     (Request_compaction Request_turn (End_session native)))
    (Native_error true 1 1 1 (Int 1) false (Request_compaction))
    |}]
;;
