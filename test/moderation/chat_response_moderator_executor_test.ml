open! Core
module CM = Prompt.Chat_markdown
module Cache = Chat_response.Cache
module Ctx = Chat_response.Ctx
module Manager = Chat_response.Moderator_manager
module Model_executor = Chat_response.Model_executor
module Moderation = Chat_response.Moderation
module Lang = Chatml.Chatml_lang

(* These tests inject the domain run_agent callback. A selected context is still
   explicit; any accidental inference dispatch must fail, never use a network. *)
let fixture_ctx ?agent_prompt_v1_recipe ~env ~dir ~tool_dir ~cache () =
  let require result =
    Result.map_error result ~f:(fun _ -> "fixture admission") |> Result.ok_or_failwith
  in
  let target =
    Inference.Request.Target.create
      ~adapter:"moderator-fixture"
      ~profile:"fixture"
      ~profile_revision:None
      ~account:None
      ~endpoint:"local"
      ~model:"fixture"
      ~settings:[]
      ~limits:Transcript.Admission.default
    |> require
  in
  let inference_context =
    Inference_runtime.Adapter.create
      ~preflight_history:(fun ~target:_ _ -> Ok ())
      ~id:"moderator-fixture"
      ~limits:Inference_runtime.Limits.default
      ~bind:(fun _ -> Ok ())
      ~prepare:(fun ~preparation_id:_ _ ->
        failwith "injected run_agent fixture dispatched inference")
      ()
    |> require
    |> Inference_runtime.Context.create ~target
    |> require
  in
  let inference_identity : Chat_response.Neutral_turn.Identity.t =
    { new_preparation_id = (fun () -> failwith "fixture prepared")
    ; with_attempt = (fun _ ~relation:_ ~f:_ -> failwith "fixture started attempt")
    }
  in
  Ctx.create
    ~inference_context
    ~inference_identity
    ~on_inference_attempt:(fun _ -> failwith "fixture admitted attempt")
    ~on_inference_completion:(fun _ -> ())
    ?agent_prompt_v1_recipe
    ~env
    ~dir
    ~tool_dir
    ~cache
    ()
;;

let%test_unit "nested Driver moderator uses its installed canonical recipe" =
  Eio_main.run (fun env ->
    let dir = Eio.Stdenv.cwd env in
    let calls = ref 0 in
    let physical_recipe : Moderation.Capabilities.model_recipe =
      { call = (fun ~payload:_ -> failwith "unexpected synchronous model call")
      ; spawn =
          (fun ~payload ->
            (match payload with
             | `Null -> ()
             | `Object _ | `Array _ | `String _ | `Number _ | `True | `False ->
               failwith "unexpected model request payload");
            Int.incr calls;
            Ok "physical-model-child")
      }
    in
    let ctx =
      fixture_ctx
        ~agent_prompt_v1_recipe:physical_recipe
        ~env
        ~dir
        ~tool_dir:dir
        ~cache:(Cache.create ~max_size:1 ())
        ()
    in
    let prompt =
      {|
<developer>Use the selected model recipe.</developer>
<script language="chatml" kind="moderator">
  type state = int
  type event = [ `Session_start ]
  let initial_state = 0
  let on_event : context -> state -> event -> state task =
    fun ctx st ev ->
      match ev with
      | `Session_start ->
        Task.bind(Model.spawn("agent_prompt_v1", `Null), fun child ->
        Task.bind(Runtime.end_session("done"), fun ignored -> Task.pure(st + 1)))
</script>
|}
    in
    Chat_response.Driver.run_agent ~ctx prompt [] |> ignore;
    assert (Int.equal !calls 1))
;;

let json_object_field (json : Jsonaf.t) (key : string) : Jsonaf.t option =
  match json with
  | `Object fields ->
    List.find_map fields ~f:(fun (k, v) -> if String.equal k key then Some v else None)
  | _ -> None
;;

let json_string_field_exn (json : Jsonaf.t) (key : string) : string =
  match json_object_field json key with
  | Some (`String s) -> s
  | Some _ -> failwith (Printf.sprintf "Expected %S to be a string" key)
  | None -> failwith (Printf.sprintf "Missing field %S" key)
;;

let json_bool_field_exn (json : Jsonaf.t) (key : string) : bool =
  match json_object_field json key with
  | Some `True -> true
  | Some `False -> false
  | Some _ -> failwith (Printf.sprintf "Expected %S to be a bool" key)
  | None -> failwith (Printf.sprintf "Missing field %S" key)
;;

let%expect_test "agent_prompt_v1 returns structured json (no OpenAI)" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  let tmp = Eio.Path.(cwd / "_tmp_model_executor_test") in
  Eio.Path.mkdirs ~perm:0o700 tmp;
  let cache_file = Eio.Path.(tmp / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:10 () in
  let ctx = fixture_ctx ~env ~dir:tmp ~tool_dir:tmp ~cache () in
  let exec_context : Model_executor.exec_context =
    { ctx
    ; run_agent =
        (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ _prompt_xml items ->
          let input =
            match items with
            | [ CM.Basic basic ] -> Option.value basic.text ~default:""
            | _ -> ""
          in
          "echo:" ^ input)
    ; fetch_prompt =
        (fun ~ctx:_ ~prompt:_ ~is_local:_ -> Ok ("<user>fixture</user>", None))
    }
  in
  let executor = Model_executor.create ~sw ~exec_context () in
  let recipe =
    Model_executor.recipe_agent_prompt_v1 executor ~session_id:"caller-session" ()
  in
  let payload =
    `Object
      [ "prompt", `String "<prompt/>"
      ; "input", `String "hi"
      ; "session_id", `String "nested-session"
      ]
  in
  (match recipe.call ~payload with
   | Error msg -> failwith msg
   | Ok (Moderation.Capabilities.Model_ok json) ->
     let recipe = json_string_field_exn json "recipe" in
     let session_id = json_string_field_exn json "session_id" in
     let final_text = json_string_field_exn json "final_text" in
     let terminated_normally = json_bool_field_exn json "terminated_normally" in
     print_s
       [%sexp
         { recipe : string
         ; session_id : string
         ; final_text : string
         ; terminated_normally : bool
         }]
   | Ok other -> print_s [%sexp (other : Moderation.Capabilities.model_call_result)]);
  [%expect
    {|
    ((recipe agent_prompt_v1) (session_id nested-session) (final_text echo:hi)
     (terminated_normally true))
    |}];
  let sentinel = Failure "strict recipe sentinel" in
  let failing : Model_executor.exec_context =
    { exec_context with
      run_agent =
        (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ ->
          raise sentinel)
    }
  in
  let request =
    Chat_response.Model_spawn_request.decode payload |> Result.ok_or_failwith
  in
  let previous_backtrace = Stdlib.Printexc.backtrace_status () in
  Stdlib.Printexc.record_backtrace true;
  Exn.protect
    ~finally:(fun () -> Stdlib.Printexc.record_backtrace previous_backtrace)
    ~f:(fun () ->
      let strict_raised_with_backtrace =
        match
          Model_executor.call_agent_prompt_v1_strict
            failing
            ~request
            ~capture_recipe_target:(fun _ -> Ok ())
            ()
        with
        | Ok _ | Error _ -> false
        | exception exn ->
          phys_equal exn sentinel
          && not
               (String.is_empty
                  (Stdlib.Printexc.raw_backtrace_to_string
                     (Stdlib.Printexc.get_raw_backtrace ())))
      in
      assert strict_raised_with_backtrace;
      let conversation = Model_executor.create ~sw ~exec_context:failing () in
      let wrapped =
        Model_executor.recipe_agent_prompt_v1 conversation ~session_id:"caller-session" ()
      in
      assert (
        match wrapped.call ~payload with
        | Ok (Moderation.Capabilities.Model_error message) ->
          String.is_substring message ~substring:"strict recipe sentinel"
        | Ok (Moderation.Capabilities.Model_ok _ | Model_refused _) | Error _ -> false);
      assert (Result.is_error (wrapped.call ~payload:`Null)))
;;

let%test_unit
    "selected Model recipe keeps its derived context; ordinary Captured never fetches"
  =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let cwd = Eio.Stdenv.cwd env in
      let ctx =
        fixture_ctx ~env ~dir:cwd ~tool_dir:cwd ~cache:(Cache.create ~max_size:1 ()) ()
      in
      let fetches = ref 0 in
      let ordinary : Model_executor.exec_context =
        { ctx
        ; run_agent =
            (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ ->
              failwith "ordinary run was reached")
        ; fetch_prompt =
            (fun ~ctx:_ ~prompt:_ ~is_local:_ ->
              Int.incr fetches;
              Error "ordinary Fetch was reached")
        }
      in
      let payload input =
        `Object
          [ "prompt", `String "agents/child.chatmd"
          ; "input", `String input
          ; "is_local", `True
          ; "source_mode", `String "captured"
          ]
      in
      let request =
        Chat_response.Model_spawn_request.decode (payload "outer")
        |> Result.ok_or_failwith
      in
      assert (
        match
          Model_executor.call_agent_prompt_v1_strict
            ordinary
            ~request
            ~capture_recipe_target:(fun _ -> Ok ())
            ()
        with
        | Error message ->
          String.is_substring message ~substring:"selected source custody"
        | Ok _ -> false);
      let ordinary_executor = Model_executor.create ~sw ~exec_context:ordinary () in
      let ordinary_recipe =
        Model_executor.recipe_agent_prompt_v1 ordinary_executor ~session_id:"ordinary" ()
      in
      assert (
        match ordinary_recipe.call ~payload:(payload "outer") with
        | Ok (Moderation.Capabilities.Model_error message) ->
          String.is_substring message ~substring:"selected source custody"
        | Ok (Model_ok _ | Model_refused _) | Error _ -> false);
      assert (Int.equal !fetches 0);
      let selected_calls = ref [] in
      let source =
        Model_executor.Selected_source.create ~with_selected:(fun request ~f ->
          let config =
            if String.equal (Chat_response.Model_spawn_request.input request) "outer"
            then { Chat_response.Config.default with model = Some "selected-outer" }
            else Chat_response.Config.default
          in
          f ~config ~run:(fun ~ctx ~input ~history_compaction ~session_id ->
            assert (
              String.equal
                (Inference.Request.Target.model
                   (Inference_runtime.Context.target ctx.inference_context))
                "selected-outer");
            let input =
              match input with
              | [ CM.Basic item ] -> Option.value item.text ~default:""
              | _ -> failwith "selected recipe changed synthesized input"
            in
            selected_calls := (input, history_compaction, session_id) :: !selected_calls;
            if String.equal input "outer"
            then (
              match ctx.agent_prompt_v1_recipe with
              | None -> failwith "derived context lost selected recipe"
              | Some nested ->
                (match nested.call ~payload:(payload "inner") with
                 | Ok (Moderation.Capabilities.Model_ok json) ->
                   Ok ("outer:" ^ json_string_field_exn json "final_text")
                 | Ok (Model_error message | Model_refused message) | Error message ->
                   Error message))
            else Ok input))
      in
      let selected = Model_executor.create_selected ~sw ~ctx ~source () in
      let recipe =
        Model_executor.recipe_agent_prompt_v1 selected ~session_id:"selected" ()
      in
      (match recipe.call ~payload:(payload "outer") with
       | Ok (Moderation.Capabilities.Model_ok json) ->
         assert (String.equal (json_string_field_exn json "final_text") "outer:inner");
         assert (
           String.equal (json_string_field_exn json "session_id") "agents/child.chatmd")
       | Ok (Model_error message | Model_refused message) | Error message ->
         failwith message);
      assert (
        List.equal
          (Tuple3.equal ~eq1:String.equal ~eq2:Bool.equal ~eq3:String.equal)
          (List.rev !selected_calls)
          [ "outer", false, "agents/child.chatmd"; "inner", false, "agents/child.chatmd" ])))
;;

let%test_unit "joined selected Model audit ignores only the private stop reason" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      let cwd = Eio.Stdenv.cwd env in
      let ctx =
        fixture_ctx ~env ~dir:cwd ~tool_dir:cwd ~cache:(Cache.create ~max_size:1 ()) ()
      in
      let stop_reason = Failure "private selected Model stop" in
      let unrelated = Failure "unrelated selected worker" in
      let run ~include_failure =
        let retained = ref None in
        let blocked_started, mark_blocked_started = Eio.Promise.create () in
        let never, _ = Eio.Promise.create () in
        (match
           Eio.Switch.run (fun sw ->
             let source =
               Model_executor.Selected_source.create ~with_selected:(fun request ~f ->
                 f
                   ~config:Chat_response.Config.default
                   ~run:(fun ~ctx:_ ~input:_ ~history_compaction:_ ~session_id:_ ->
                     if
                       String.equal
                         (Chat_response.Model_spawn_request.prompt request)
                         "blocked"
                     then (
                       Eio.Promise.resolve mark_blocked_started ();
                       Eio.Promise.await never)
                     else raise unrelated))
             in
             let executor = Model_executor.create_selected ~sw ~ctx ~source () in
             retained := Some executor;
             let recipe =
               Model_executor.recipe_agent_prompt_v1 executor ~session_id:"audit" ()
             in
             let spawn prompt =
               match
                 recipe.spawn
                   ~payload:(`Object [ "prompt", `String prompt; "input", `String "hi" ])
               with
               | Ok _ -> ()
               | Error message -> failwith message
             in
             spawn "blocked";
             Eio.Promise.await blocked_started;
             if include_failure
             then (
               spawn "fail";
               match Model_executor.settle_session executor ~session_id:"audit" with
               | exception exn -> assert (phys_equal exn unrelated)
               | Ok _ | Error _ -> failwith "failing worker was not observed");
             Eio.Switch.fail sw stop_reason)
         with
         | () -> failwith "selected switch did not stop"
         | exception exn ->
           assert (
             phys_equal exn stop_reason
             ||
             match exn with
             | Eio.Cancel.Cancelled reason -> phys_equal reason stop_reason
             | _ -> false));
        Option.value_exn !retained
      in
      let stopped = run ~include_failure:false in
      (match
         Model_executor.audit_joined_session
           stopped
           ~session_id:"audit"
           ~allowed_cancelled_reason:stop_reason
       with
       | Ok () -> ()
       | Error message -> failwith message);
      let failed = run ~include_failure:true in
      match
        Model_executor.audit_joined_session
          failed
          ~session_id:"audit"
          ~allowed_cancelled_reason:stop_reason
      with
      | exception exn -> assert (phys_equal exn unrelated)
      | Ok () | Error _ -> failwith "unrelated failure was hidden by private stop"))
;;

let%test_unit "selected moderator startup stops before foreground and retains queued work"
  =
  Eio_main.run (fun env ->
    let artifact id source =
      let script =
        CM.
          { id
          ; language = "chatml"
          ; project_selection = None
          ; kind = "moderator"
          ; source_ref = None
          ; source = Inline source
          }
      in
      let _, artifact =
        Manager.Registry.compile_script Manager.Registry.empty script
        |> Result.ok_or_failwith
      in
      artifact
    in
    let allocator namespace =
      History_entry.Allocator.create ~namespace ~next_sequence:0 |> Result.ok_or_failwith
    in
    let start ~artifact ~namespace ~capabilities ~runtime_policy =
      Chat_response.Driver.create_selected_moderator_entries
        ~env
        ~session_id:namespace
        ~artifact:(Some artifact)
        ~allocator:(allocator namespace)
        ~history:[]
        ~available_tools:[]
        ~capabilities
        ~runtime_policy
        ()
      |> Result.ok_or_failwith
    in
    let end_artifact =
      artifact
        "selected-start-end"
        {|
          type event = [ `Session_start ]
          let initial_state = 0
          let on_event : context -> int -> event -> int task = fun ctx st ev ->
            match ev with
            | `Session_start ->
              Task.bind(Runtime.end_session("before-provider"), fun ignored -> Task.pure(st))
        |}
    in
    (match
       start
         ~artifact:end_artifact
         ~namespace:"selected-start-end"
         ~capabilities:Moderation.Capabilities.default
         ~runtime_policy:Chat_response.Runtime_semantics.default_policy
     with
     | Chat_response.Driver.Selected_moderator_start.End_session_requested reason ->
       assert (String.equal reason "before-provider")
     | No_moderator | Ready _ | Halted | Paused ->
       failwith "Session_start did not stop the selected child");
    let queue_artifact =
      artifact
        "selected-start-queue"
        {|
          let initial_state = 0
          let on_event ctx st ev = match ev with
            | `Session_start ->
              Task.bind(Runtime.emit(`String("first")), fun ignored ->
              Task.bind(Runtime.emit(`String("second")), fun ignored -> Task.pure(st)))
            | `String(payload) ->
              Task.bind(Tool.call("startup-probe", `String(payload)), fun ignored ->
              Task.pure(st + 1))
            | _ -> Task.pure(st)
        |}
    in
    let handled = ref 0 in
    let capabilities =
      { Moderation.Capabilities.default with
        on_tool_call =
          (fun ~name ~args:_ ->
            assert (String.equal name "startup-probe");
            Int.incr handled;
            Ok (Tool_ok `Null))
      }
    in
    let policy = Chat_response.Runtime_semantics.default_policy in
    let paused_policy =
      { policy with
        budget =
          { policy.budget with
            pause_conditions =
              [ Chat_response.Runtime_semantics.Pause_internal_event_drains ]
          }
      }
    in
    (match
       start
         ~artifact:queue_artifact
         ~namespace:"selected-start-paused"
         ~capabilities
         ~runtime_policy:paused_policy
     with
     | Chat_response.Driver.Selected_moderator_start.Paused -> ()
     | No_moderator | Ready _ | Halted | End_session_requested _ ->
       failwith "selected startup ignored pause policy");
    assert (Int.equal !handled 0);
    let capped_policy =
      { policy with budget = { policy.budget with max_internal_event_drain = 1 } }
    in
    match
      start
        ~artifact:queue_artifact
        ~namespace:"selected-start-capped"
        ~capabilities
        ~runtime_policy:capped_policy
    with
    | Chat_response.Driver.Selected_moderator_start.Ready moderator ->
      assert (Int.equal !handled 1);
      let snapshot = Manager.snapshot moderator.manager |> Result.ok_or_failwith in
      (match snapshot.current_state with
       | Session.Snapshot.Int 1 -> ()
       | _ -> failwith "selected startup did not handle exactly one queued event");
      assert (Int.equal (List.length snapshot.queued_internal_events) 1)
    | No_moderator | Halted | Paused | End_session_requested _ ->
      failwith "selected startup did not retain capped remainder")
;;

let%expect_test "agent_prompt_v1 spawn enforces max_spawned_jobs (no OpenAI)" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  let tmp = Eio.Path.(cwd / "_tmp_model_executor_test_spawn") in
  Eio.Path.mkdirs ~perm:0o700 tmp;
  let cache_file = Eio.Path.(tmp / "cache.bin") in
  let cache = Cache.load ~file:cache_file ~max_size:10 () in
  let ctx = fixture_ctx ~env ~dir:tmp ~tool_dir:tmp ~cache () in
  let exec_context : Model_executor.exec_context =
    { ctx
    ; run_agent =
        (fun ?history_compaction:_
          ?prompt_dir:_
          ?session_id:_
          ~ctx:_
          _prompt_xml
          _items -> "ok")
    ; fetch_prompt =
        (fun ~ctx:_ ~prompt:_ ~is_local:_ -> Ok ("<user>fixture</user>", None))
    }
  in
  let executor = Model_executor.create ~sw ~exec_context ~max_spawned_jobs:1 () in
  let recipe =
    Model_executor.recipe_agent_prompt_v1 executor ~session_id:"caller-session" ()
  in
  let payload =
    `Object
      [ "prompt", `String "<prompt/>"
      ; "input", `String "hi"
      ; "session_id", `String "nested-session"
      ]
  in
  let first = recipe.spawn ~payload in
  let second = recipe.spawn ~payload in
  print_s [%sexp ((first, second) : (string, string) result * (string, string) result)];
  [%expect
    {|
    ((Ok model-job-1) (Error "Model.spawn: exceeded maximum spawned job limit.")) |}]
;;

type job_state =
  [ `Pending
  | `Succeeded of Jsonaf.t
  | `Failed of string
  ]
[@@deriving sexp]

let ok_or_fail = function
  | Ok value -> value
  | Error msg -> failwith msg
;;

let moderator_artifact () =
  let script =
    CM.
      { id = "main"
      ; language = "chatml"
      ; project_selection = None
      ; kind = "moderator"
      ; source_ref = None
      ; source =
          Inline
            {|
              type state = { count : int }
              type event = [ `Session_start ]

              let initial_state = { count = 0 }

              let on_event : context -> state -> event -> state task =
                fun ctx st ev ->
                  match ev with
                  | `Session_start -> Task.pure({ count = st.count + 1 })
            |}
      }
  in
  let _, compiled =
    ok_or_fail (Manager.Registry.compile_script Manager.Registry.empty script)
  in
  compiled
;;

let model_payload prompt : Jsonaf.t =
  `Object [ "prompt", `String prompt; "input", `String "hi" ]
;;

let%test_unit "strict settlement observes a failing worker before a blocked sibling" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      let blocked_joined = ref false in
      let marker = Failure "selected model cancellation" in
      let escaped = Eio.Cancel.Cancelled marker in
      let observed = ref false in
      let () =
        try
          Eio.Switch.run (fun sw ->
            let cwd = Eio.Stdenv.cwd env in
            let cache = Cache.create ~max_size:1 () in
            let ctx = fixture_ctx ~env ~dir:cwd ~tool_dir:cwd ~cache () in
            let never, _ = Eio.Promise.create () in
            let started, mark_started = Eio.Promise.create () in
            let exec_context : Model_executor.exec_context =
              { ctx
              ; run_agent =
                  (fun ?history_compaction:_
                    ?prompt_dir:_
                    ?session_id:_
                    ~ctx:_
                    prompt
                    _ ->
                    if String.equal prompt "<user>blocked</user>"
                    then
                      Exn.protect
                        ~f:(fun () ->
                          Eio.Promise.resolve mark_started ();
                          Eio.Promise.await never)
                        ~finally:(fun () -> blocked_joined := true)
                    else raise escaped)
              ; fetch_prompt =
                  (fun ~ctx:_ ~prompt ~is_local:_ ->
                    Ok ("<user>" ^ prompt ^ "</user>", None))
              }
            in
            let executor = Model_executor.create ~sw ~exec_context () in
            let recipe =
              Model_executor.recipe_agent_prompt_v1 executor ~session_id:"strict" ()
            in
            ignore (ok_or_fail (recipe.spawn ~payload:(model_payload "blocked")));
            Eio.Promise.await started;
            let manager =
              Manager.create
                ~artifact:(moderator_artifact ())
                ~capabilities:Moderation.Capabilities.default
                ()
              |> ok_or_fail
            in
            Model_executor.register_session executor ~session_id:"strict" ~manager;
            let policy = Chat_response.Runtime_semantics.default_policy in
            let paused_policy =
              { policy with
                budget =
                  { policy.budget with
                    pause_conditions =
                      [ Chat_response.Runtime_semantics.Pause_internal_event_drains ]
                  }
              }
            in
            let moderator : Chat_response.In_memory_stream.moderator =
              { manager
              ; session_id = "strict"
              ; session_meta = `Null
              ; runtime_policy = paused_policy
              ; event_handlers = None
              }
            in
            let paused =
              Chat_response.Model_completion_drain.run_entries
                ~executor
                ~moderator
                ~now_ms:(fun () -> 1)
                ~history:[]
                ~available_tools:[]
                ()
              |> ok_or_fail
            in
            (match paused.stop with
             | Paused -> ()
             | _ -> failwith "Paused completion waited on a blocked Model job");
            assert (Int.equal paused.spawned_jobs 1);
            ignore (ok_or_fail (recipe.spawn ~payload:(model_payload "fail")));
            (match Model_executor.settle_session executor ~session_id:"strict" with
             | exception exn ->
               assert (phys_equal exn escaped);
               observed := true
             | Ok _ | Error _ -> failwith "Expected escaped worker cancellation");
            Eio.Switch.fail sw Exit)
        with
        | Exit | Eio.Cancel.Cancelled Exit -> ()
      in
      assert !observed;
      assert !blocked_joined))
;;

let%test_unit "cancelled wakeup retains one delivered event" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
        let cwd = Eio.Stdenv.cwd env in
        let ctx =
          fixture_ctx ~env ~dir:cwd ~tool_dir:cwd ~cache:(Cache.create ~max_size:1 ()) ()
        in
        let exec_context : Model_executor.exec_context =
          { ctx
          ; run_agent =
              (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ -> "ok")
          ; fetch_prompt = (fun ~ctx:_ ~prompt:_ ~is_local:_ -> Ok ("text", None))
          }
        in
        let executor = Model_executor.create ~sw ~exec_context () in
        let manager =
          ok_or_fail
            (Manager.create
               ~artifact:(moderator_artifact ())
               ~capabilities:Moderation.Capabilities.default
               ())
        in
        let marker = Failure "wakeup cancelled" in
        let escaped = Eio.Cancel.Cancelled marker in
        Model_executor.register_session
          executor
          ~session_id:"wake"
          ~manager
          ~on_wakeup:(fun () -> raise escaped);
        let recipe =
          Model_executor.recipe_agent_prompt_v1 executor ~session_id:"wake" ()
        in
        let job_id = ok_or_fail (recipe.spawn ~payload:(model_payload "text")) in
        (match Model_executor.settle_session executor ~session_id:"wake" with
         | exception exn -> assert (phys_equal exn escaped)
         | Ok _ | Error _ -> failwith "Expected original wakeup cancellation");
        assert (Option.is_some (Model_executor.job_state executor ~job_id));
        let queued () =
          ok_or_fail (Manager.snapshot manager)
          |> fun snapshot ->
          List.length snapshot.Session.Moderator_snapshot.queued_internal_events
        in
        assert (Int.equal (queued ()) 1);
        Model_executor.register_session executor ~session_id:"wake" ~manager;
        assert (Int.equal (queued ()) 1))))
;;

let%test_unit "entry completion stops before a queued event after End_session" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let cwd = Eio.Stdenv.cwd env in
      let ctx =
        fixture_ctx ~env ~dir:cwd ~tool_dir:cwd ~cache:(Cache.create ~max_size:1 ()) ()
      in
      let exec_context : Model_executor.exec_context =
        { ctx
        ; run_agent =
            (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ -> "ok")
        ; fetch_prompt = (fun ~ctx:_ ~prompt:_ ~is_local:_ -> Ok ("text", None))
        }
      in
      let executor = Model_executor.create ~sw ~exec_context () in
      let script =
        CM.
          { id = "completion-stop"
          ; language = "chatml"
          ; project_selection = None
          ; kind = "moderator"
          ; source_ref = None
          ; source =
              Inline
                {|
                  type event = [ `Session_start | `Internal_done(string) ]
                  let initial_state = 0
                  let on_event : context -> int -> event -> int task = fun ctx st ev ->
                    match ev with
                    | `Session_start -> Task.pure(st)
                    | `Internal_done(_) ->
                      Task.bind(Runtime.end_session("done"), fun ignored -> Task.pure(st + 1))
                |}
          }
      in
      let _, artifact =
        Manager.Registry.compile_script Manager.Registry.empty script |> ok_or_fail
      in
      let allocator =
        History_entry.Allocator.create ~namespace:"completion-stop" ~next_sequence:0
        |> ok_or_fail
      in
      let manager =
        Manager.create_entries
          ~artifact
          ~capabilities:Moderation.Capabilities.default
          ~allocator
          ()
        |> ok_or_fail
      in
      let event = Lang.VVariant ("Internal_done", [ Lang.VString "queued" ]) in
      Manager.enqueue_internal_event manager event |> ok_or_fail;
      Manager.enqueue_internal_event manager event |> ok_or_fail;
      let moderator : Chat_response.In_memory_stream.moderator =
        { manager
        ; session_id = "completion-stop"
        ; session_meta = `Null
        ; runtime_policy = Chat_response.Runtime_semantics.default_policy
        ; event_handlers = None
        }
      in
      let result =
        Chat_response.Model_completion_drain.run_entries
          ~executor
          ~moderator
          ~now_ms:(fun () -> 1)
          ~history:[]
          ~available_tools:[]
          ()
        |> ok_or_fail
      in
      (match result.stop with
       | End_session_requested "done" -> ()
       | _ -> failwith "Expected End_session stop");
      assert (Int.equal result.processed_events 1);
      let snapshot = Manager.snapshot manager |> ok_or_fail in
      assert (Int.equal (List.length snapshot.queued_internal_events) 1)))
;;

let%test_unit
    "entry completion rejects a mutable request before another event or blocked job"
  =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      let blocked_joined = ref false in
      let rejected = ref false in
      let marker = Failure "selected completion mutable request" in
      (try
         Eio.Switch.run (fun sw ->
           let cwd = Eio.Stdenv.cwd env in
           let ctx =
             fixture_ctx
               ~env
               ~dir:cwd
               ~tool_dir:cwd
               ~cache:(Cache.create ~max_size:1 ())
               ()
           in
           let blocked, _ = Eio.Promise.create () in
           let started, mark_started = Eio.Promise.create () in
           let exec_context : Model_executor.exec_context =
             { ctx
             ; run_agent =
                 (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ ->
                   Exn.protect
                     ~f:(fun () ->
                       Eio.Promise.resolve mark_started ();
                       Eio.Promise.await blocked)
                     ~finally:(fun () -> blocked_joined := true))
             ; fetch_prompt =
                 (fun ~ctx:_ ~prompt:_ ~is_local:_ -> Ok ("<user>blocked</user>", None))
             }
           in
           let executor = Model_executor.create ~sw ~exec_context () in
           let recipe =
             Model_executor.recipe_agent_prompt_v1 executor ~session_id:"mutable" ()
           in
           ignore (ok_or_fail (recipe.spawn ~payload:(model_payload "blocked")));
           Eio.Promise.await started;
           let script =
             CM.
               { id = "completion-mutable"
               ; language = "chatml"
               ; project_selection = None
               ; kind = "moderator"
               ; source_ref = None
               ; source =
                   Inline
                     {|
                       type event = [ `Internal_done(string) ]
                       let initial_state = 0
                       let on_event : context -> int -> event -> int task = fun ctx st ev ->
                         match ev with
                         | `Internal_done(_) ->
                           Task.bind(Runtime.request_turn(), fun ignored ->
                             Task.pure(st + 1))
                     |}
               }
           in
           let _, artifact =
             Manager.Registry.compile_script Manager.Registry.empty script |> ok_or_fail
           in
           let allocator =
             History_entry.Allocator.create
               ~namespace:"completion-mutable"
               ~next_sequence:0
             |> ok_or_fail
           in
           let manager =
             Manager.create_entries
               ~artifact
               ~capabilities:Moderation.Capabilities.default
               ~allocator
               ()
             |> ok_or_fail
           in
           Model_executor.register_session executor ~session_id:"mutable" ~manager;
           let event = Lang.VVariant ("Internal_done", [ Lang.VString "queued" ]) in
           Manager.enqueue_internal_event manager event |> ok_or_fail;
           Manager.enqueue_internal_event manager event |> ok_or_fail;
           let moderator : Chat_response.In_memory_stream.moderator =
             { manager
             ; session_id = "mutable"
             ; session_meta = `Null
             ; runtime_policy = Chat_response.Runtime_semantics.default_policy
             ; event_handlers = None
             }
           in
           (match
              Chat_response.Model_completion_drain.run_entries
                ~executor
                ~moderator
                ~now_ms:(fun () -> 1)
                ~history:[]
                ~available_tools:[]
                ~on_runtime_request:(function
                  | Request_turn | Request_compaction -> raise marker
                  | End_session _ -> ())
                ()
            with
            | exception exn when phys_equal exn marker -> rejected := true
            | Ok _ | Error _ -> failwith "Expected mutable request rejection"
            | exception exn -> raise exn);
           let snapshot = Manager.snapshot manager |> ok_or_fail in
           (match snapshot.current_state with
            | Session.Snapshot.Int 1 -> ()
            | _ -> failwith "Second event handler ran after rejection");
           assert (Int.equal (List.length snapshot.queued_internal_events) 1);
           Eio.Switch.fail sw Exit)
       with
       | Exit | Eio.Cancel.Cancelled Exit -> ());
      assert !rejected;
      assert !blocked_joined))
;;

let%test_unit "entry completion joins jobs spawned by a completed-job handler" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
        let completed_calls = ref 0 in
        let sampled_times = ref [] in
        let release_first, first_released = Eio.Promise.create () in
        let release_blocked, blocked_released = Eio.Promise.create () in
        let blocked_started, mark_blocked_started = Eio.Promise.create () in
        let empty_observed, mark_empty_observed = Eio.Promise.create () in
        let saw_initial_observation = ref false in
        let cwd = Eio.Stdenv.cwd env in
        let ctx =
          fixture_ctx ~env ~dir:cwd ~tool_dir:cwd ~cache:(Cache.create ~max_size:1 ()) ()
        in
        let exec_context : Model_executor.exec_context =
          { ctx
          ; run_agent =
              (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ prompt _ ->
                (match prompt with
                 | "<user>first</user>" -> Eio.Promise.await release_first
                 | "<user>blocked</user>" ->
                   Eio.Promise.resolve mark_blocked_started ();
                   Eio.Promise.await release_blocked
                 | "<user>next</user>" -> ()
                 | _ -> failwith "Unexpected model prompt");
                Int.incr completed_calls;
                "ok")
          ; fetch_prompt =
              (fun ~ctx:_ ~prompt ~is_local:_ -> Ok ("<user>" ^ prompt ^ "</user>", None))
          }
        in
        let executor = Model_executor.create ~sw ~exec_context ~max_spawned_jobs:3 () in
        let recipe =
          Model_executor.recipe_agent_prompt_v1 executor ~session_id:"chain" ()
        in
        let capabilities =
          { Moderation.Capabilities.default with
            model_recipes = Map.singleton (module String) "agent_prompt_v1" recipe
          ; on_tool_call =
              (fun ~name:_ ~args:_ ->
                Eio.Promise.resolve blocked_released ();
                Ok (Tool_ok `Null))
          }
        in
        let script =
          CM.
            { id = "completion-chain"
            ; language = "chatml"
            ; project_selection = None
            ; kind = "moderator"
            ; source_ref = None
            ; source =
                Inline
                  {|
                    type event =
                      [ `Model_job_succeeded(string, string, json)
                      | `Model_job_failed(string, string, string) ]
                    let initial_state = 0
                    let on_event : context -> int -> event -> int task = fun ctx st ev ->
                      match ev with
                      | `Model_job_succeeded(_, _, _) ->
                        if st == 0 then
                          Task.bind(Tool.call("release", `Null), fun ignored_tool ->
                            Task.bind(
                              Model.spawn("agent_prompt_v1", Json.parse("{\"prompt\":\"next\",\"input\":\"hi\"}")),
                              fun ignored -> Task.pure(st + 1)))
                        else Task.pure(st + 1)
                      | `Model_job_failed(_, _, _) -> Task.pure(st + 1)
                  |}
            }
        in
        let _, artifact =
          Manager.Registry.compile_script Manager.Registry.empty script |> ok_or_fail
        in
        let allocator =
          History_entry.Allocator.create ~namespace:"completion-chain" ~next_sequence:0
          |> ok_or_fail
        in
        let manager =
          Manager.create_entries ~artifact ~capabilities ~allocator () |> ok_or_fail
        in
        Model_executor.register_session executor ~session_id:"chain" ~manager;
        let moderator : Chat_response.In_memory_stream.moderator =
          { manager
          ; session_id = "chain"
          ; session_meta = `Null
          ; runtime_policy = Chat_response.Runtime_semantics.default_policy
          ; event_handlers = None
          }
        in
        ignore (ok_or_fail (recipe.spawn ~payload:(model_payload "first")));
        ignore (ok_or_fail (recipe.spawn ~payload:(model_payload "blocked")));
        Eio.Promise.await blocked_started;
        let completed_promise =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Chat_response.Model_completion_drain.run_entries
              ~executor
              ~moderator
              ~now_ms:(fun () ->
                let now = !completed_calls in
                sampled_times := now :: !sampled_times;
                if not !saw_initial_observation
                then (
                  saw_initial_observation := true;
                  Eio.Promise.resolve mark_empty_observed ());
                now)
              ~history:[]
              ~available_tools:[]
              ()
            |> ok_or_fail)
        in
        Eio.Promise.await empty_observed;
        Eio.Promise.resolve first_released ();
        let completed = Eio.Promise.await_exn completed_promise in
        (match completed.stop with
         | Quiescent -> ()
         | _ -> failwith "Expected completion quiescence");
        assert (Int.equal completed.processed_events 3);
        assert (Int.equal completed.spawned_jobs 3);
        assert (List.mem !sampled_times 3 ~equal:Int.equal);
        let snapshot = Manager.snapshot manager |> ok_or_fail in
        (match snapshot.current_state with
         | Session.Snapshot.Int 3 -> ()
         | _ -> failwith "Expected three handled model completion events");
        assert (List.is_empty snapshot.queued_internal_events))))
;;

let%test_unit "concurrent completed-job delivery enqueues only once" =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
        let cwd = Eio.Stdenv.cwd env in
        let ctx =
          fixture_ctx ~env ~dir:cwd ~tool_dir:cwd ~cache:(Cache.create ~max_size:1 ()) ()
        in
        let exec_context : Model_executor.exec_context =
          { ctx
          ; run_agent =
              (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ -> "ok")
          ; fetch_prompt =
              (fun ~ctx:_ ~prompt:_ ~is_local:_ -> Ok ("<user>ok</user>", None))
          }
        in
        let executor = Model_executor.create ~sw ~exec_context () in
        let recipe =
          Model_executor.recipe_agent_prompt_v1 executor ~session_id:"race" ()
        in
        let job_id = ok_or_fail (recipe.spawn ~payload:(model_payload "ok")) in
        Model_executor.await_job executor ~job_id |> ok_or_fail;
        let held, entered = Eio.Promise.create () in
        let release, released = Eio.Promise.create () in
        let capabilities =
          { Moderation.Capabilities.default with
            on_tool_call =
              (fun ~name:_ ~args:_ ->
                Eio.Promise.resolve entered ();
                Eio.Promise.await release;
                Ok (Tool_ok `Null))
          }
        in
        let script =
          CM.
            { id = "delivery-race"
            ; language = "chatml"
            ; project_selection = None
            ; kind = "moderator"
            ; source_ref = None
            ; source =
                Inline
                  {|
                    type event = [ `Session_start ]
                    let initial_state = 0
                    let on_event : context -> int -> event -> int task = fun ctx st ev ->
                      match ev with
                      | `Session_start ->
                        Task.bind(Tool.call("hold", `Null), fun ignored -> Task.pure(st + 1))
                  |}
            }
        in
        let _, artifact =
          Manager.Registry.compile_script Manager.Registry.empty script |> ok_or_fail
        in
        let manager = Manager.create ~artifact ~capabilities () |> ok_or_fail in
        let handler =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Manager.handle_event
              manager
              ~session_id:"race"
              ~now_ms:0
              ~history:[]
              ~available_tools:[]
              ~session_meta:`Null
              ~event:Moderation.Event.Session_start
            |> ok_or_fail
            |> ignore)
        in
        Eio.Promise.await held;
        let registration =
          Eio.Fiber.fork_promise ~sw (fun () ->
            Model_executor.register_session executor ~session_id:"race" ~manager)
        in
        (match Model_executor.settle_session executor ~session_id:"race" with
         | Error reason ->
           assert (String.equal reason "Model job delivery is already in progress.")
         | Ok _ -> failwith "Concurrent delivery should be busy");
        Eio.Promise.resolve released ();
        Eio.Promise.await_exn registration;
        Eio.Promise.await_exn handler;
        let settled = Model_executor.settle_session executor ~session_id:"race" in
        ignore (ok_or_fail settled : Model_executor.Session_settlement.t);
        let queued =
          Manager.snapshot manager
          |> ok_or_fail
          |> fun snapshot ->
          List.length snapshot.Session.Moderator_snapshot.queued_internal_events
        in
        assert (Int.equal queued 1))))
;;

let%test_unit
    "background completion waits for an active moderator without false recursion"
  =
  Eio_main.run (fun env ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
      Eio.Switch.run (fun sw ->
        let cwd = Eio.Stdenv.cwd env in
        let tmp = Eio.Path.(cwd / "_tmp_model_spawn_active_owner") in
        Eio.Path.mkdirs ~perm:0o700 tmp;
        let cache = Cache.load ~file:Eio.Path.(tmp / "cache.bin") ~max_size:10 () in
        let ctx = fixture_ctx ~env ~dir:tmp ~tool_dir:tmp ~cache () in
        let exec_context : Model_executor.exec_context =
          { ctx
          ; run_agent =
              (fun ?history_compaction:_ ?prompt_dir:_ ?session_id:_ ~ctx:_ _ _ -> "ok")
          ; fetch_prompt =
              (fun ~ctx:_ ~prompt:_ ~is_local:_ -> Ok ("<user>fixture</user>", None))
          }
        in
        let executor = Model_executor.create ~sw ~exec_context () in
        let recipe =
          Model_executor.recipe_agent_prompt_v1 executor ~session_id:"active-owner" ()
        in
        let job = ref None in
        let capabilities =
          { Moderation.Capabilities.default with
            on_tool_call =
              (fun ~name:_ ~args:_ ->
                let id =
                  recipe.spawn
                    ~payload:
                      (`Object [ "prompt", `String "<prompt/>"; "input", `String "hi" ])
                  |> ok_or_fail
                in
                job := Some id;
                let rec await_business_result () =
                  match Model_executor.job_state executor ~job_id:id with
                  | Some (`Succeeded _) -> ()
                  | Some `Pending ->
                    Eio.Fiber.yield ();
                    await_business_result ()
                  | _ -> failwith "expected successful background model result"
                in
                await_business_result ();
                Ok (Tool_ok `Null))
          }
        in
        let script =
          CM.
            { id = "active-owner"
            ; language = "chatml"
            ; project_selection = None
            ; kind = "moderator"
            ; source_ref = None
            ; source =
                Inline
                  {|
          type event = [ `Session_start ]
          let initial_state = 0
          let on_event : context -> int -> event -> int task = fun ctx st ev ->
            Task.bind(Tool.call("spawn", `Null), fun ignored -> Task.pure(st + 1))
        |}
            }
        in
        let _, artifact =
          Manager.Registry.compile_script Manager.Registry.empty script |> ok_or_fail
        in
        let manager = Manager.create ~artifact ~capabilities () |> ok_or_fail in
        Model_executor.register_session executor ~session_id:"active-owner" ~manager;
        Manager.handle_event
          manager
          ~session_id:"active-owner"
          ~now_ms:0
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~event:Moderation.Event.Session_start
        |> ok_or_fail
        |> ignore;
        Model_executor.await_job executor ~job_id:(Option.value_exn !job) |> ok_or_fail;
        let snapshot = Manager.snapshot manager |> ok_or_fail in
        assert (Poly.equal snapshot.current_state (Session.Snapshot.Int 1));
        assert (List.length snapshot.queued_internal_events = 1))))
;;

let%expect_test "spawn completion encodes stable internal event variants" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let module Cache = Chat_response.Cache in
  let module Ctx = Chat_response.Ctx in
  let cwd = Eio.Stdenv.cwd env in
  let tmp = Eio.Path.(cwd / "_tmp_model_spawn_reinject") in
  Eio.Path.mkdirs ~perm:0o700 tmp;
  let cache = Cache.load ~file:Eio.Path.(tmp / "cache.bin") ~max_size:10 () in
  let ctx = fixture_ctx ~env ~dir:tmp ~tool_dir:tmp ~cache () in
  let exec_context : Model_executor.exec_context =
    { ctx
    ; run_agent =
        (fun ?history_compaction:_
          ?prompt_dir:_
          ?session_id:_
          ~ctx:_
          _prompt_xml
          _items -> "ok")
    ; fetch_prompt =
        (fun ~ctx:_ ~prompt:_ ~is_local:_ -> Ok ("<user>fixture</user>", None))
    }
  in
  let executor = Model_executor.create ~sw ~exec_context () in
  let recipe = Model_executor.recipe_agent_prompt_v1 executor ~session_id:"sess-1" () in
  let payload =
    `Object
      [ "prompt", `String "<prompt/>"
      ; "input", `String "hi"
      ; "session_id", `String "nested-session"
      ]
  in
  let job_id = recipe.spawn ~payload |> Result.ok_or_failwith in
  Model_executor.await_job executor ~job_id |> Result.ok_or_failwith;
  (* We can't observe moderator delivery here without constructing a full moderator
     manager; task 10 will cover that. We at least assert the job completed. *)
  print_s [%sexp (Model_executor.job_state executor ~job_id : job_state option)];
  [%expect
    {|
    ((Succeeded
      (Object
       ((recipe (String agent_prompt_v1)) (prompt (String <prompt/>))
        (is_local False) (session_id (String nested-session))
        (final_text (String ok)) (terminated_normally True)))))
    |}]
;;

let%expect_test "spawn completion wakes registered session and unregister is inert" =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  let cwd = Eio.Stdenv.cwd env in
  let tmp = Eio.Path.(cwd / "_tmp_model_spawn_wakeup") in
  Eio.Path.mkdirs ~perm:0o700 tmp;
  let cache = Cache.load ~file:Eio.Path.(tmp / "cache.bin") ~max_size:10 () in
  let ctx = fixture_ctx ~env ~dir:tmp ~tool_dir:tmp ~cache () in
  let exec_context : Model_executor.exec_context =
    { ctx
    ; run_agent =
        (fun ?history_compaction:_
          ?prompt_dir:_
          ?session_id:_
          ~ctx:_
          _prompt_xml
          _items -> "ok")
    ; fetch_prompt =
        (fun ~ctx:_ ~prompt:_ ~is_local:_ -> Ok ("<user>fixture</user>", None))
    }
  in
  let executor = Model_executor.create ~sw ~exec_context () in
  let manager =
    ok_or_fail
      (Manager.create
         ~artifact:(moderator_artifact ())
         ~capabilities:Moderation.Capabilities.default
         ())
  in
  let wakeups = ref 0 in
  Model_executor.register_session
    executor
    ~session_id:"sess-1"
    ~manager
    ~on_wakeup:(fun () -> wakeups := !wakeups + 1);
  let recipe = Model_executor.recipe_agent_prompt_v1 executor ~session_id:"sess-1" () in
  let payload =
    `Object
      [ "prompt", `String "<prompt/>"
      ; "input", `String "hi"
      ; "session_id", `String "nested-session"
      ]
  in
  let job_id_1 = recipe.spawn ~payload |> Result.ok_or_failwith in
  Model_executor.await_job executor ~job_id:job_id_1 |> Result.ok_or_failwith;
  let queued_after_first =
    ok_or_fail (Manager.snapshot manager)
    |> fun snapshot ->
    List.length snapshot.Session.Moderator_snapshot.queued_internal_events
  in
  let wakeups_after_first = !wakeups in
  print_s
    [%sexp
      ([ "wakeups", Int.to_string wakeups_after_first
       ; "queued_after_first", Int.to_string queued_after_first
       ]
       : (string * string) list)];
  Model_executor.unregister_session_wakeup executor ~session_id:"sess-1";
  let job_id_2 = recipe.spawn ~payload |> Result.ok_or_failwith in
  Model_executor.await_job executor ~job_id:job_id_2 |> Result.ok_or_failwith;
  let queued_after_second =
    ok_or_fail (Manager.snapshot manager)
    |> fun snapshot ->
    List.length snapshot.Session.Moderator_snapshot.queued_internal_events
  in
  let wakeups_after_second = !wakeups in
  print_s
    [%sexp
      ([ "wakeups", Int.to_string wakeups_after_second
       ; "queued_after_second", Int.to_string queued_after_second
       ]
       : (string * string) list)];
  [%expect
    {|
    ((wakeups 1) (queued_after_first 1))
    ((wakeups 1) (queued_after_second 2))
    |}]
;;
