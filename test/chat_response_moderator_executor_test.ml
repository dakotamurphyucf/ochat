open! Core
module CM = Prompt.Chat_markdown
module Cache = Chat_response.Cache
module Ctx = Chat_response.Ctx
module Manager = Chat_response.Moderator_manager
module Model_executor = Chat_response.Model_executor
module Moderation = Chat_response.Moderation
module Lang = Chatml.Chatml_lang

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
  let ctx = Ctx.create ~env ~dir:tmp ~tool_dir:tmp ~cache in
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
    ; fetch_prompt = (fun ~ctx:_ ~prompt ~is_local:_ -> Ok (prompt, None))
    }
  in
  let executor = Model_executor.create ~sw ~exec_context () in
  let recipe =
    Model_executor.recipe_agent_prompt_v1 executor ~session_id:"caller-session"
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
    |}]
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
  let ctx = Ctx.create ~env ~dir:tmp ~tool_dir:tmp ~cache in
  let exec_context : Model_executor.exec_context =
    { ctx
    ; run_agent =
        (fun ?history_compaction:_
          ?prompt_dir:_
          ?session_id:_
          ~ctx:_
          _prompt_xml
          _items -> "ok")
    ; fetch_prompt = (fun ~ctx:_ ~prompt ~is_local:_ -> Ok (prompt, None))
    }
  in
  let executor = Model_executor.create ~sw ~exec_context ~max_spawned_jobs:1 () in
  let recipe =
    Model_executor.recipe_agent_prompt_v1 executor ~session_id:"caller-session"
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
      ; kind = "moderator"
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
  let ctx = Ctx.create ~env ~dir:tmp ~tool_dir:tmp ~cache in
  let exec_context : Model_executor.exec_context =
    { ctx
    ; run_agent =
        (fun ?history_compaction:_
          ?prompt_dir:_
          ?session_id:_
          ~ctx:_
          _prompt_xml
          _items -> "ok")
    ; fetch_prompt = (fun ~ctx:_ ~prompt ~is_local:_ -> Ok (prompt, None))
    }
  in
  let executor = Model_executor.create ~sw ~exec_context () in
  let recipe = Model_executor.recipe_agent_prompt_v1 executor ~session_id:"sess-1" in
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
  let ctx = Ctx.create ~env ~dir:tmp ~tool_dir:tmp ~cache in
  let exec_context : Model_executor.exec_context =
    { ctx
    ; run_agent =
        (fun ?history_compaction:_
          ?prompt_dir:_
          ?session_id:_
          ~ctx:_
          _prompt_xml
          _items -> "ok")
    ; fetch_prompt = (fun ~ctx:_ ~prompt ~is_local:_ -> Ok (prompt, None))
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
  let recipe = Model_executor.recipe_agent_prompt_v1 executor ~session_id:"sess-1" in
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
    ok_or_fail (Manager.snapshot manager) |> fun snapshot ->
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
    ok_or_fail (Manager.snapshot manager) |> fun snapshot ->
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
