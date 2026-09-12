open! Core

let load env root file = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / file)

let save env root file text =
  Eio.Path.save
    ~create:(`Or_truncate 0o644)
    Eio.Path.(Eio.Stdenv.fs env / root / file)
    text
;;

let rec files env root relative =
  let path = Eio.Path.(Eio.Stdenv.fs env / root / relative) in
  Eio.Path.read_dir path
  |> List.sort ~compare:String.compare
  |> List.concat_map ~f:(fun name ->
    let file = Filename.concat relative name in
    match Eio.Path.kind ~follow:false Eio.Path.(path / name) with
    | `Directory
      when (not (String.is_prefix name ~prefix:".")) && not (String.equal name "_build")
      -> files env root file
    | `Regular_file -> [ file ]
    | _ -> [])
;;

let protocol_types env root =
  let header =
    "# Protocol types and codec reference\n\n\
     Generated from the current public interfaces. Do not hand-edit the excerpts.\n\
     The [protocol guide](protocol.md) explains operation semantics and authorization.\n\
     Each section includes the complete typed contract and a link to its JSON codec;\n\
     wire tags/defaults are defined by that codec, not OCaml constructor spelling.\n\n"
  in
  let sections =
    files env root "lib/agent_protocol"
    |> List.filter ~f:(fun file -> String.is_suffix file ~suffix:".mli")
    |> List.map ~f:(fun file ->
      let name = Filename.basename file |> Filename.chop_extension in
      sprintf
        "## %s\n\n\
         [JSON codec](../../%s.ml) · [interface](../../%s)\n\n\
         ```ocaml\n\
         %s\n\
         ```\n\n"
        name
        (Filename.chop_extension file)
        file
        (String.rstrip (load env root file)))
  in
  String.rstrip (header ^ String.concat sections) ^ "\n"
;;

let owner name =
  let name = String.lowercase name in
  let choices =
    [ [ "authoring_corpus" ], "../guide/authoring-topic-corpus.md"
    ; [ "authoring_sources" ], "../guide/authoring-source-bundle.md"
    ; ( [ "background_request"
        ; "background_execution"
        ; "background_delivery"
        ; "background_job_event"
        ; "completion_contract"
        ; "completion_projection"
        ; "job_result_reference"
        ; "standalone_delivery"
        ; "authoring"
        ; "generated_admission"
        ; "generated_definition"
        ; "generated_session_request"
        ; "generated_session_tool"
        ; "managed_session_service"
        ; "session_management"
        ; "managed_session_tool"
        ; "managed_send_tool"
        ; "managed_read_tool"
        ; "managed_wait_tool"
        ; "managed_stop"
        ; "managed_output_cursor"
        ; "managed_output_page"
        ; "managed_submission"
        ; "agent_tool_contract"
        ; "authored_agent"
        ; "delegation_store"
        ; "delegation_authority"
        ; "delegation_lifecycle"
        ; "runtime_activity"
        ; "delegated_runtime"
        ; "independent_resources"
        ]
      , "../agent-server/extensibility-foundations.md" )
    ; [ "request_channel" ], "../bin/ochat_agent_helper.doc.md"
    ; [ "shell" ], "../guide/chatmd-shell-host-integration.md"
    ; ( [ "transport"; "protocol"; "http"; "stdio"; "client" ]
      , "../agent-server/protocol.md" )
    ; ( [ "persist"; "store"; "recovery"; "migration"; "retention" ]
      , "../agent-server/operations.md" )
    ; ( [ "permission"; "auth"; "grant"; "security" ]
      , "../agent-server/permissions-and-security.md" )
    ; ( [ "chatml"; "job"; "schedule"; "moderator" ]
      , "../agent-server/chatml-orchestration.md" )
    ; [ "tui"; "terminal" ], "../guide/chat_tui.md"
    ; ( [ "workspace"; "session"; "prompt"; "history" ]
      , "../agent-server/sessions-and-workspaces.md" )
    ]
  in
  List.find_map choices ~f:(fun (words, target) ->
    if List.exists words ~f:(fun substring -> String.is_substring name ~substring)
    then Some target
    else None)
  |> Option.value ~default:"../agent-server/concepts.md"
;;

let spec_rows env root name =
  let file = "docs-src/design/" ^ name in
  load env root file
  |> String.split_lines
  |> List.filter ~f:(fun line -> String.is_prefix line ~prefix:"## ")
  |> List.map ~f:(fun line ->
    let title = String.drop_prefix line 3 in
    sprintf
      "| %s: %s | [spec](../design/%s) | [guide](%s) | Cross-reference; exact \
       subcontracts also in protocol/interfaces. |\n"
      (if String.is_substring name ~substring:"implementation"
       then "Implementation"
       else "Architecture")
      title
      name
      (owner title))
  |> String.concat
;;

let module_rows env root =
  files env root "lib"
  |> List.filter ~f:(fun file -> String.is_suffix file ~suffix:".mli")
  |> List.filter ~f:(fun file ->
    String.is_prefix file ~prefix:"lib/agent_"
    || List.exists
         [ "authoring_sources/"
         ; "chat_tui/"
         ; "chat_response/"
         ; "chatmd/"
         ; "chatml/"
         ; "shell_runtime/"
         ; "shell_access/"
         ; "chatmd_shell_spec/"
         ; "openai/responses"
         ; "history_entry"
         ; "session_store"
         ; "source_loader"
         ]
         ~f:(fun substring -> String.is_substring file ~substring))
  |> List.map ~f:(fun file ->
    sprintf
      "| `%s` | [contract](../../%s) | [integration](%s) | Public interface + current \
       host guide. |\n"
      file
      file
      (owner file))
  |> String.concat
;;

let document_rows env root =
  files env root "docs-src"
  |> List.filter ~f:(fun file -> String.is_suffix file ~suffix:".md")
  |> List.map ~f:(fun file ->
    let state =
      if String.is_substring file ~substring:"agent-server/"
      then "Current reference/tutorial; offline checker applies."
      else if String.is_substring file ~substring:"design/"
      then "Canonical design; conceptual/compatibility qualifications retained."
      else
        "Retained reference; host-sensitive entry points reconciled; unrelated \
         algorithms not rewritten."
    in
    sprintf
      "| [page](../%s) | `%s` | %s |\n"
      (String.chop_prefix_exn file ~prefix:"docs-src/")
      file
      state)
  |> String.concat
;;

let coverage env root =
  "# Documentation coverage ledger\n\n\
   Generated inventory for the current tree, not a claim that every historical code\n\
   example was executed. Regenerate with `docs_check --refresh ROOT`. Read the\n\
   [worklog](documentation-worklog.md) for actual verification and qualifications.\n\
   The [code/documentation audit](code-documentation-audit.md) records scope and \
   boundaries.\n\n\
   ## Specification sections\n\n\
   | Section | Source | Current owner | Disposition |\n\
   |---|---|---|---|\n"
  ^ spec_rows env root "ochat-agent-server-spec.md"
  ^ spec_rows env root "ochat-agent-server-implementation-spec.md"
  ^ "\n\
     ## Acceptance criteria\n\n\
     The 35 implementation acceptance criteria remain normative in\n\
     [section \
     58](../design/ochat-agent-server-implementation-spec.md#58-implementation-acceptance-checklist).\n\
     Documentation ownership follows: 1–4 concepts/path and shell host guides; 5–13\n\
     sessions/workspaces and operations; 14–19 orchestration and permissions; 20–21\n\
     operations; 22–27 protocol and transports; 28–29 TUI; 30–31 tools/MCP compatibility;\n\
     32–34 embedding, security and operations; 35 testing. This is documentation\n\
     coverage, not a fresh assertion that every historical acceptance gate ran here.\n\n\
     ## Contract inventories\n\n\
     All supported methods, scopes, events and typed payloads are indexed in the\n\
     [protocol](../agent-server/protocol.md) and generated \
     [types](../agent-server/protocol-types.md).\n\
     All eight routes are in [HTTP](../agent-server/transports/http.md).\n\
     [Configuration](../agent-server/configuration.md), \
     [environment](../agent-server/environment.md),\n\
     and [executable references](../bin/chat_tui.doc.md) own operator settings.\n\n"
  ^ "\n\
     ## Public agent interfaces\n\n\
     | Surface | Source | Guide | Coverage |\n\
     |---|---|---|---|\n"
  ^ module_rows env root
  ^ "\n\
     ## Document inventory\n\n\
     This includes retained historical and unrelated documentation; retention is not\n\
     a claim that all examples apply to native/daemon hosts. Changed host semantics\n\
     are qualified in entry points, with complete current tutorials in agent-server.\n\n\
     | Document | Path | Disposition |\n\
     |---|---|---|\n"
  ^ document_rows env root
;;

let refresh env root =
  save env root "docs-src/agent-server/protocol-types.md" (protocol_types env root);
  save env root "docs-src/development/documentation-coverage.md" (coverage env root)
;;

let contract_sources =
  [ "lib/agent_server/config.mli"
  ; "lib/agent_protocol/scope.ml"
  ; "lib/agent_transport_http/request_contract.ml"
  ]
;;

let cli_flags source =
  let literal = Re.Perl.compile_pat "\"(-{1,2}[a-z][a-z0-9-]*)\"" in
  let command = Re.Perl.compile_pat "\\bflag\\s+\"([a-z][a-z0-9-]*)\"" in
  let matches pattern =
    Re.all pattern source |> List.map ~f:(fun item -> Re.Group.get item 1)
  in
  matches literal @ List.map (matches command) ~f:(fun name -> "-" ^ name)
  |> List.dedup_and_sort ~compare:String.compare
;;

let cli_inventory env root =
  [ "bin/chat_tui.ml"; "bin/ochat_agent_server.ml"; "bin/ochat_agent_stdio.ml" ]
  |> List.map ~f:(fun file ->
    let flags = cli_flags (load env root file) in
    sprintf
      "## %s flag inventory\n\n[Parser/normalizer](../../%s).\n\n%s\n\n"
      (Filename.basename file)
      file
      (String.concat ~sep:", " (List.map flags ~f:(sprintf "`%s`"))))
  |> String.concat
;;

let route_inventory env root =
  load env root "lib/agent_transport_http/server.ml"
  |> String.split_lines
  |> List.filter ~f:(fun line ->
    List.exists [ "| `GET, ["; "| `POST, ["; "| `DELETE, [" ] ~f:(fun substring ->
      String.is_substring line ~substring))
  |> String.concat_lines
  |> sprintf
       "## HTTP route inventory\n\n\
        [Dispatcher](../../lib/agent_transport_http/server.ml).\n\n\
        ```ocaml\n\
        %s\n\
        ```\n"
;;

let contracts env root =
  let header =
    "# Operator contract source appendix\n\n\
     Generated from the current tree; do not hand-edit.\n\n\
     Read [configuration](configuration.md), [protocol](protocol.md),\n\
     [HTTP](transports/http.md) and [environment](environment.md) first.\n\
     These complete excerpts pin the config record, scope codec and HTTP header/body\n\
     validation contract so documentation checks detect contract drift. They are\n\
     reference source, not standalone compilable examples.\n\n\
     Flag inventories collect literal option strings and named Core Command flag\n\
     declarations (displayed with a leading dash). Generated help/version options\n\
     and parser-added aliases are not enumerated; consult each executable's help\n\
     and [command reference](../bin/README.md) for accepted combinations.\n\n"
  in
  let text =
    header
    ^ cli_inventory env root
    ^ route_inventory env root
    ^ String.concat
        (List.map contract_sources ~f:(fun file ->
           sprintf
             "## %s\n\n[Source](../../%s)\n\n```ocaml\n%s\n```\n\n"
             (Filename.basename file)
             file
             (String.rstrip (load env root file))))
  in
  String.rstrip text ^ "\n"
;;

let refresh env root =
  refresh env root;
  save env root "docs-src/agent-server/operator-contracts.md" (contracts env root)
;;
