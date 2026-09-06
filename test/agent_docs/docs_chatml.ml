open! Core
module Lang = Chatml.Chatml_lang
module Runtime = Chatml_host_runtime

let fixture = "docs-src/examples/chatml/moderator.chatml"
let fail_exn message = failwith (fixture ^ ": " ^ message)

let result_exn stage = function
  | Ok value -> value
  | Error message -> fail_exn (stage ^ ": " ^ message)
;;

let record fields = Lang.VRecord (Map.of_alist_exn (module String) fields)
let json_null = Lang.VVariant ("Null", [])

let after_exn source marker =
  match String.substr_index source ~pattern:marker with
  | Some index -> String.drop_prefix source (index + String.length marker)
  | None -> fail_exn ("language guide is missing " ^ marker)
;;

let check_source_exn env root source =
  let guide =
    Eio.Path.load
      Eio.Path.(Eio.Stdenv.fs env / root / "docs-src/guide/chatml-language-spec.md")
  in
  let section = after_exn guide "### 21.5 Moderator script contract\n" in
  let code = after_exn section "```ocaml\n" in
  let block =
    match String.substr_index code ~pattern:"\n```" with
    | Some index -> String.prefix code index
    | None -> fail_exn "language guide moderator code fence is not closed"
  in
  if not (String.equal (String.rstrip source) (String.rstrip block))
  then fail_exn "language guide moderator example differs from the fixture"
;;

let context phase =
  record
    [ "session_id", Lang.VString "docs-chatml"
    ; "now_ms", Lang.VInt 0
    ; "phase", Lang.VString phase
    ; "items", Lang.VArray [||]
    ; "available_tools", Lang.VArray [||]
    ; "session_meta", json_null
    ]
;;

let session_exn source =
  let compiled = Runtime.compile_script ~source () |> result_exn "compile" in
  Runtime.instantiate_session
    (Runtime.default_runtime_config ())
    compiled
    ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
  |> result_exn "instantiate"
;;

let handle_exn session ~phase name arguments =
  Runtime.handle_event
    ~limits:{ fuel = 10_000; max_tasks = 1_000 }
    session
    ~context:(context phase)
    ~event:(Lang.VVariant (name, arguments))
  |> result_exn name
;;

let check_count_exn session expected =
  match Runtime.current_state session with
  | Lang.VRecord fields ->
    (match Map.find fields "appended_count" with
     | Some (Lang.VInt actual) when Int.equal actual expected -> ()
     | _ -> fail_exn (sprintf "expected appended_count=%d" expected))
  | _ -> fail_exn "expected record state"
;;

let check_no_effects_exn session =
  if not (List.is_empty (Runtime.committed_local_effects session))
  then fail_exn "non-tool events unexpectedly committed effects"
;;

let append_exn session id =
  let item = record [ "id", Lang.VString id; "value", json_null ] in
  handle_exn session ~phase:"message_appended" "Item_appended" [ item ]
;;

let reject_tool_exn session =
  let call =
    record [ "id", Lang.VString "call-1"; "name", Lang.VString "echo"; "args", json_null ]
  in
  handle_exn session ~phase:"pre_tool_call" "Pre_tool_call" [ call ];
  match
    Runtime.committed_local_effects session
    |> Runtime.decode_local_effects
    |> result_exn "decode committed effects"
  with
  | [ Runtime.Tool_moderation_effect
        (Runtime.Reject "Tools are disabled by this moderator.")
    ] -> ()
  | _ -> fail_exn "expected exactly one committed tool rejection"
;;

let run env root =
  let source = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / fixture) in
  check_source_exn env root source;
  let session = session_exn source in
  check_count_exn session 0;
  handle_exn session ~phase:"session_start" "Session_start" [];
  check_count_exn session 0;
  append_exn session "item-1";
  check_count_exn session 1;
  append_exn session "item-2";
  check_count_exn session 2;
  check_no_effects_exn session;
  reject_tool_exn session;
  check_count_exn session 2;
  Eio.Flow.copy_string
    "ChatML moderator example: source parity, compile, state updates, and tool rejection \
     PASS\n"
    (Eio.Stdenv.stdout env)
;;
