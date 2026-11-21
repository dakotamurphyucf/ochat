open! Core
module P = Agent_protocol
module Temp = Support.Temporary_environment
module Config = Support.Config_fixture
module Process = Support.Process_manager
module Stdio = Support.Stdio_process
module Client = Support.Stdio_client
module Fixture = Support.Tui_fixture
module Provider = Support.Tui_stream_provider
module Notes = Recovery_note_provider_scenario

let ok = Fixture.ok
let require = Fixture.require
let await = Fixture.await
let key name = P.Idempotency_key.of_string name |> ok
let page = P.Page.Request.create ~limit:10 () |> ok
let note = "exact retained headless note β"

let prompt =
  {|
<config model="gpt-4.1"/>
<developer>Qualify exact local recovery permissions.</developer>
<tool name="recovery_note_write"/>
<tool name="recovery_note_read"/>
<script id="root" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> Task.pure(state)
</script>
|}
;;

let write =
  `Object
    [ "kind", `String "create"
    ; "expected", `Null
    ; "path", `String "headless/exact"
    ; "expected_catalog_revision", `String "0"
    ; "content", `String note
    ]
;;

let read ~stop =
  `Object
    [ ( "selection"
      , `Object [ "kind", `String "local"; "id", `String "1"; "revision", `String "1" ] )
    ; "range", `Object [ "start", `Number "0"; "stop", `Number (Int.to_string stop) ]
    ]
;;

let check_read request ~id =
  match Notes.output request ~id with
  | Complete value ->
    require
      (String.equal (Notes.field "text" value |> Jsonaf.string_exn) note)
      "protected provider response did not contain the exact retained note"
  | outcome -> raise_s [%sexp "protected read failed", (outcome : P.Invocation.outcome)]
;;

let request client command = (Client.request client command |> ok).result
let internal client command = request client command |> Support.Public_view.non_history

let session client session_id =
  match request client (Session_get { session_id; history = None }) with
  | Session_get snapshot -> (P.Public.Snapshot.fields snapshot).session
  | _ -> failwith "session.get returned the wrong result"
;;

let pending client session_id =
  match internal client (Permission_list { session_id; page; state = Some Pending }) with
  | Permission_list { items; _ } -> List.hd items
  | _ -> failwith "permission.list returned the wrong result"
;;

let respond_to env client ~session_id ~attachment_id ~tool_name ~choice =
  let permission = await env (fun () -> pending client session_id) in
  require
    (String.equal permission.tool_name tool_name)
    "interactive permission belonged to a different native tool";
  require
    (not (String.is_substring permission.invocation_display ~substring:note))
    "interactive projection exposed private native input";
  match
    internal
      client
      (Permission_respond
         { session_id
         ; attachment_id
         ; permission_id = permission.id
         ; permission_generation = permission.generation
         ; choice
         ; reason = None
         ; idempotency_key = key tool_name
         })
  with
  | Permission_respond { permission; _ } ->
    require
      (P.Permission.equal_state permission.state Approved)
      "approval was not committed"
  | _ -> failwith "permission.respond returned the wrong result"
;;

let observe_state env client session_id ~f =
  await env (fun () -> Option.some_if (f (session client session_id).observed_state) ())
;;

let executable env ~variable ~name =
  match Sys.getenv variable with
  | Some path ->
    if Filename.is_absolute path then path else Eio_posix.Low_level.realpath path
  | None ->
    Filename.concat
      (Eio.Path.native_exn (Eio.Stdenv.cwd env))
      ("_build/default/bin/" ^ name ^ ".exe")
;;

let only_session client =
  let list =
    P.Session.List_request.of_json (`Object (P.Page.Request.to_fields page)) |> ok
  in
  match internal client (Session_list list) with
  | Session_list { items = [ entry ]; _ } -> entry.session.id
  | _ -> failwith "stock local stdio did not create exactly one session"
;;

let interactive env provider ~sw ~environment ~cwd ~prompt_file ~store =
  let clock = Eio.Stdenv.clock env in
  let child =
    Stdio.spawn
      ~sw
      ~env
      ~cwd
      ~environment
      ~max_output_bytes:65536
      [ executable env ~variable:"OCHAT_E2E_STDIO_EXE" ~name:"ochat_agent_stdio"
      ; "--local"
      ; "--prompt"
      ; prompt_file
      ; "--workspace"
      ; Eio.Path.native_exn cwd
      ; "--data-root"
      ; store
      ]
  in
  let client = Client.create ~process:child ~clock in
  ignore (Client.initialize client |> ok : _);
  let session_id = only_session client in
  let attachment_id =
    match
      request
        client
        (Session_attach
           { session_id
           ; requested_mode = Read_write
           ; subscribe = true
           ; after_sequence = None
           ; reclaim_token = None
           ; idempotency_key = key "headless-approval-attach"
           })
    with
    | Session_attach result -> result.attachment.id
    | _ -> failwith "session.attach returned the wrong result"
  in
  ignore
    (internal
       client
       (Session_send_message
          { session_id
          ; attachment_id
          ; content =
              { kind = Plain_text
              ; text = "Prepare the scoped note read."
              ; attachments = []
              }
          ; timing = Safe_boundary
          ; idempotency_key = key "headless-approval-input"
          })
     : P.Method_result.t);
  let provider_request = Provider.await_request provider env in
  Notes.function_call (provider_request 0) ~id:"write" ~name:"recovery_note_write" write;
  respond_to
    env
    client
    ~session_id
    ~attachment_id
    ~tool_name:"recovery_note_write"
    ~choice:Approve_once;
  let response = provider_request 1 in
  (match Notes.output response ~id:"write" with
   | Complete `Null -> ()
   | outcome ->
     raise_s
       [%sexp "private write was not acknowledged", (outcome : P.Invocation.outcome)]);
  Notes.function_call
    response
    ~id:"initial-read"
    ~name:"recovery_note_read"
    (read ~stop:(String.length note));
  respond_to
    env
    client
    ~session_id
    ~attachment_id
    ~tool_name:"recovery_note_read"
    ~choice:Approve_prefix;
  let response = provider_request 2 in
  check_read response ~id:"initial-read";
  Provider.reply_summary response "The interactive host approved the named read tool.";
  (match
     internal
       client
       (Grant_list
          { session_id = Some session_id; page; principal_id = None; state = Some Active })
   with
   | Grant_list { items = [ grant ]; _ } ->
     require
       (String.equal grant.tool_name "recovery_note_read"
        && P.Grant.equal_scope grant.scope Prefix_session)
       "the store retained an unexpected native tool grant"
   | _ -> failwith "interactive execution did not issue exactly one named-tool grant");
  observe_state env client session_id ~f:(function
    | Idle -> true
    | _ -> false);
  ignore
    (internal
       client
       (Session_stop
          { session_id
          ; attachment_id
          ; mode = Graceful
          ; idempotency_key = key "headless-approval-stop"
          })
     : P.Method_result.t);
  observe_state env client session_id ~f:(function
    | Stopped -> true
    | _ -> false);
  Stdio.close_stdin child;
  let exit = Eio.Time.with_timeout_exn clock 10. (fun () -> Stdio.await child) in
  require
    (Process.equal_exit exit.exit (Exited 0))
    "stock local stdio did not close cleanly";
  session_id
;;

let managed env provider ~sw ~environment ~cwd ~prompt_file ~store ~session_id =
  let main = executable env ~variable:"OCHAT_E2E_MAIN_EXE" ~name:"main" in
  let output_path = Eio.Path.(cwd / "exact-headless-output.chatmd") in
  let child =
    Process.spawn
      ~sw
      ~env
      ~cwd
      ~environment
      ~max_output_bytes:65536
      [ main
      ; "chat-completion"
      ; "-prompt-file"
      ; prompt_file
      ; "-storage-root"
      ; store
      ; "-output-file"
      ; Eio.Path.native_exn output_path
      ; "-session-id"
      ; P.Id.Session.to_string session_id
      ; "-message"
      ; "Read the exact retained note."
      ]
  in
  let provider_request = Provider.await_request provider env in
  Notes.function_call
    (provider_request 3)
    ~id:"ungranted-write"
    ~name:"recovery_note_write"
    write;
  let denied = provider_request 4 in
  (match Notes.output denied ~id:"ungranted-write" with
   | Fail { code = "invocation.permission_denied"; _ } -> ()
   | outcome ->
     raise_s
       [%sexp
         "ungranted write did not use deny fallback", (outcome : P.Invocation.outcome)]);
  Notes.function_call
    denied
    ~id:"same-range"
    ~name:"recovery_note_read"
    (read ~stop:(String.length note));
  let accepted = provider_request 5 in
  check_read accepted ~id:"same-range";
  Provider.reply_summary accepted "The headless run used the named read grant.";
  let result =
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () -> Process.await child)
  in
  require
    (Process.equal_exit result.exit (Exited 0))
    ("managed completion did not finish: " ^ result.stderr.contents);
  require
    (String.is_substring
       (Eio.Path.load output_path)
       ~substring:"The headless run used the named read grant.")
    "managed completion did not export the provider's final response";
  require
    (not (String.is_substring (Eio.Path.load output_path) ~substring:note))
    "private original was persisted in the exported history"
;;

let run env temporary =
  let fixture = Fixture.create env temporary "headless-recovery-permission" in
  let port = Config.http_port fixture in
  let endpoint = sprintf "http://127.0.0.1:%d" port in
  let secret = "headless-private-synthetic-key" in
  Temp.register_secret temporary note;
  let cwd = Temp.path temporary (Config.physical_workspace fixture) in
  let prompt_file = Config.prompt_path fixture in
  Eio.Path.save ~create:(`Or_truncate 0o600) (Temp.path temporary prompt_file) prompt;
  Support.Provider_fixture.with_host ~env ~api_url:endpoint ~key:secret fixture ignore;
  let environment =
    Temp.child_environment temporary ~base:(Core_unix.environment ())
    |> Array.filter ~f:(fun entry ->
      not
        (String.is_prefix entry ~prefix:"API_URL="
         || String.is_prefix entry ~prefix:"PWD="))
    |> Fn.flip Array.append [| "API_URL=" ^ endpoint; "PWD=" ^ Eio.Path.native_exn cwd |]
  in
  Eio.Switch.run (fun sw ->
    let provider = Provider.start ~sw ~env ~port in
    let store = Config.data_dir fixture in
    let session_id = interactive ~sw env provider ~environment ~cwd ~prompt_file ~store in
    managed ~sw env provider ~environment ~cwd ~prompt_file ~store ~session_id)
;;
