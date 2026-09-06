open! Core

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
