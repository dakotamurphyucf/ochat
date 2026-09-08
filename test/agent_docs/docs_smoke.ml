open! Core

let shell_examples env root =
  let module S = Chatmd_shell_spec.Chatmd_script_spec in
  let module C = Shell_runtime.Chatml_extension in
  let module L = Chatml.Chatml_lang in
  let file = "docs-src/guide/chatmd-shell-examples.md" in
  let guide = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file) in
  let pattern =
    Re.Perl.compile_pat
      ~opts:[ `Dotall ]
      "<script id=\"([^\"]+)\" language=\"chatml\" kind=\"([^\"]+)\">(.*?)</script>"
  in
  let scripts = Re.all pattern guide in
  if List.length scripts <> 3 then failwith "expected three shell example scripts";
  List.iter scripts ~f:(fun group ->
    let id = Re.Group.get group 1 in
    let source = Re.Group.get group 3 in
    let position = Chatmd_shell_spec.Source_ref.{ offset = 0; line = 1; column = 0 } in
    let source_ref =
      Chatmd_shell_spec.Source_ref.create
        ~file
        ~source_dir:"."
        ~prompt_dir:"."
        ~namespace:None
        ~start_pos:position
        ~end_pos:position
        ~source
    in
    let diagnostic_exn = function
      | Ok value -> value
      | Error diagnostic -> failwith (Chatmd_shell_spec.Diagnostic.to_string diagnostic)
    in
    let kind = S.kind_of_string source_ref (Re.Group.get group 2) |> diagnostic_exn in
    let script =
      S.
        { id
        ; language = "chatml"
        ; kind
        ; source = Inline source
        ; source_ref
        ; source_sha256 = Chatmd_shell_spec.Source_ref.digest source
        ; limits = default_limits
        }
    in
    let compiled =
      match C.compile ~script with
      | Ok value -> value
      | Error diagnostics ->
        failwith
          (String.concat
             ~sep:"\n"
             (List.map diagnostics ~f:Chatmd_shell_spec.Diagnostic.to_string))
    in
    let instance = C.instantiate ~env ~lifecycle:Session compiled |> diagnostic_exn in
    (* No process or provider capability is installed. Only argv is read by the hook. *)
    let event =
      L.VRecord
        (String.Map.of_alist_exn
           [ "argv", L.VArray [| L.VString "python3"; L.VString "-V" |] ])
    in
    let actual =
      C.call instance ~context:(Docs_chatml.context (S.kind_to_string kind)) ~event
      |> diagnostic_exn
    in
    let expected =
      match kind with
      | S.Shell_before_interceptor ->
        C.Intercept_rewrite [ "/opt/ochat-tools/safe-python"; "-V" ]
      | Shell_reviewer -> Review_defer
      | Shell_audit_filter -> Audit_keep
      | _ -> failwith ("unexpected shell example kind: " ^ id)
    in
    if not (C.equal_action actual expected)
    then failwith ("shell example action mismatch: " ^ id))
;;

let batch env root scratch =
  let module CM = Prompt.Chat_markdown in
  let load file = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file) in
  let guide = load "docs-src/cli/chat-completion.md" in
  let copy =
    Re.Perl.compile_pat "cp ([^ \\n]+) \"\\$OCHAT_BATCH/prompt\\.chatmd\""
    |> fun pattern -> Re.exec pattern guide
  in
  let template = load (Re.Group.get copy 1) in
  let messages =
    Re.all (Re.Perl.compile_pat "<user>[^\\n]*?</user>") guide
    |> List.map ~f:(fun group -> Re.Group.get group 0)
  in
  let first, second =
    match messages with
    | [ first; second ] -> first, second
    | _ -> failwith "batch guide must provide two complete user messages"
  in
  let dir = Eio.Path.(Eio.Stdenv.fs env / scratch) in
  Io.save_doc ~dir "batch-prompt.chatmd" (template ^ "\n" ^ first ^ "\n");
  Io.append_doc ~dir "batch-session.chatmd" (Io.load_doc ~dir "batch-prompt.chatmd");
  let check expected_users =
    let nodes =
      CM.parse_chat_inputs
        ~source:"batch-session.chatmd"
        ~dir
        (Io.load_doc ~dir "batch-session.chatmd")
    in
    let users =
      List.count nodes ~f:(function
        | CM.User _ -> true
        | _ -> false)
    in
    let configs =
      List.count nodes ~f:(function
        | CM.Config _ -> true
        | _ -> false)
    in
    let developers =
      List.count nodes ~f:(function
        | CM.Developer _ -> true
        | _ -> false)
    in
    if users <> expected_users || configs <> 1 || developers <> 1
    then failwith "batch preparation/continuation changed message or template counts";
    if
      List.exists nodes ~f:(function
        | CM.Tool _ | CM.Script _ -> true
        | _ -> false)
    then failwith "batch hello example must stay tool-free and script-free"
  in
  check 1;
  Io.append_doc ~dir "batch-session.chatmd" (second ^ "\n");
  check 2
;;

let shell_actions env root =
  let load file = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file) in
  let source = load "lib/chatml/chatml_builtin_spec.ml" in
  let guide = load "docs-src/guide/chatmd-shell-extensions.md" in
  let pattern =
    Re.Perl.compile_pat
      "~op:\"((?:Match|Review|Intercept|Result|Effect|Audit)\\.[a-z_]+)\""
  in
  Re.all pattern source
  |> List.map ~f:(fun group -> Re.Group.get group 1)
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:(fun name ->
    if not (String.is_substring guide ~substring:(name ^ "("))
    then failwith ("undocumented shell ChatML action: " ^ name))
;;

let protocol_exn = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let snapshot connection session_id =
  match
    Agent_client.Connection.request
      connection
      (Session_get { session_id; history = None })
    |> protocol_exn
  with
  | Session_get value -> value
  | _ -> failwith "expected snapshot"
;;

let verify_timer connection session_id =
  let page = Agent_protocol.Page.Request.create ~limit:10 () |> protocol_exn in
  match
    Agent_client.Connection.request
      connection
      (Schedule_list { session_id; page; status = None })
    |> protocol_exn
  with
  | Schedule_list { items = [ schedule ]; _ } ->
    (match schedule.status with
     | Delivered when schedule.delivery_count = 1 -> ()
     | _ -> failwith "tutorial timer was not delivered exactly once")
  | _ -> failwith "expected one tutorial schedule"
;;

let timer env root workspace =
  Eio.Switch.run (fun sw ->
    let options =
      Agent_server.Embedded.
        { prompt_file =
            Filename.concat root "docs-src/examples/agent-server/prompts/timer.chatmd"
        ; workspace
        ; tool_dir = workspace
        ; home = workspace
        ; data_root = None
        ; start_immediately = true
        ; permission_profile = default_permission_profile
        ; attachment_mode = Read_write
        ; event_capacity = 128
        }
    in
    let host = Agent_server.Embedded.start ~sw ~env options |> protocol_exn in
    Fun.protect
      ~finally:(fun () -> Agent_server.Embedded.close host)
      (fun () ->
         let connection = Agent_server.Embedded.connection host in
         let id = Agent_server.Embedded.session_id host in
         Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
           let rec await () =
             match (snapshot connection id).session.observed_state with
             | Stopped -> ()
             | Failed error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
             | _ ->
               Eio.Time.sleep (Eio.Stdenv.clock env) 0.05;
               await ()
           in
           await ());
         verify_timer connection id))
;;

let shell env root =
  let file = "docs-src/examples/agent-server/shell/pwd.chatmd" in
  let source = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file) in
  let nodes =
    Prompt.Chat_markdown.parse_chat_inputs
      ~source:file
      ~dir:Eio.Path.(Eio.Stdenv.fs env / root)
      source
  in
  let runtimes =
    List.filter_map nodes ~f:(function
      | Prompt.Chat_markdown.Shell_runtime x -> Some x
      | _ -> None)
  in
  let tools =
    List.filter_map nodes ~f:(function
      | Prompt.Chat_markdown.Tool (Shell x) -> Some x
      | _ -> None)
  in
  let input =
    Chatmd_shell_spec.Manifest_compiler.
      { runtimes
      ; tools
      ; scripts = []
      ; legacy_tools = []
      ; moderator_runtime = None
      ; platform = Macos
      ; supported_features = Chatmd_shell_spec.Feature.phase1
      }
  in
  match Chatmd_shell_spec.Manifest_compiler.compile input with
  | Ok _ -> ()
  | Error errors ->
    failwith
      (String.concat_lines (List.map errors ~f:Chatmd_shell_spec.Diagnostic.to_string))
;;

let shell_syntax env root =
  let file = "docs-src/guide/chatmd-shell-examples.md" in
  let source = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file) in
  let pattern =
    Re.(compile (seq [ str "```xml\n"; group (non_greedy (rep any)); str "\n```" ]))
  in
  Re.all pattern source
  |> List.iteri ~f:(fun index block ->
    try
      ignore
        (Chatmd_parser.document
           (Chatmd_lexer.create ())
           (Lexing.from_string (Re.Group.get block 1))
         : Chatmd_ast.document)
    with
    | exn ->
      failwith (sprintf "shell example XML block %d: %s" (index + 1) (Exn.to_string exn)))
;;
