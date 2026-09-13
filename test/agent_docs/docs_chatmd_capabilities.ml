open Core
module CM = Prompt.Chat_markdown
module Content = CM
module V = Chat_response.Authoring_validation

let guide = "guide/chatmd-authoring-capabilities.md"

let examples text =
  let rec body acc = function
    | [] -> failwith "unclosed root declaration example"
    | "```" :: rest -> String.concat ~sep:"\n" (List.rev acc), rest
    | line :: rest -> body (line :: acc) rest
  in
  let rec scan acc = function
    | [] -> List.rev acc
    | "```xml authoring=root" :: rest ->
      let source, rest = body [] rest in
      scan (source :: acc) rest
    | line :: _ when String.is_prefix line ~prefix:"```" ->
      failwith "every capability guide fence must be a checked root declaration"
    | _ :: rest -> scan acc rest
  in
  scan [] (String.split_lines text)
;;

let summary elements =
  List.concat_map elements ~f:(function
    | CM.Developer _ -> []
    | Tool (Read_file spec) ->
      [ "roots:" ^ String.concat ~sep:"," (List.map spec.roots ~f:(fun root -> root.id)) ]
    | Shell_runtime _ -> [ "shell-runtime" ]
    | Tool (Shell { name = "git_status"; runtime = "readonly"; mode = Fixed _; _ }) ->
      [ "fixed-shell-tool" ]
    | Tool (Persistent_agent ({ name = "review"; is_local = true; _ }, Optional)) ->
      [ "optional-agent-tool" ]
    | Tool
        (Mcp
           { names = Some [ "lookup" ]
           ; strict = true
           ; client_id_env = Some "EXAMPLE_MCP_CLIENT_ID"
           ; client_secret_env = Some "EXAMPLE_MCP_CLIENT_SECRET"
           ; _
           }) -> [ "selected-mcp-tool" ]
    | User { content = Some (Items items); _ } ->
      List.filter_map items ~f:(function
        | Content.Agent _ -> Some "inline-agent"
        | Basic { document_url = Some _; _ } -> Some "document"
        | Basic { image_url = Some _; _ } -> Some "image"
        | Basic _ -> None)
    | Tool_call { tool_call_id = Some "call_example"; _ } -> [ "stored-call" ]
    | Tool_response { tool_call_id = Some "call_example"; _ } -> [ "stored-response" ]
    | _ ->
      failwith "capability guide declaration changed; review its semantic expectation")
;;

let run env root =
  let sources = Authoring_sources.installed () |> Result.ok_or_failwith in
  let corpus = Authoring_corpus.runtime_foundation ~sources |> Result.ok_or_failwith in
  let document =
    Authoring_sources.document sources ~path:guide |> Result.ok_or_failwith
  in
  let text = Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / root / "docs-src" / guide) in
  assert (String.equal text document.text);
  Docs_chatml_authoring.check_topic_coverage corpus ~path:guide ~text;
  let examples = examples text in
  let expected =
    [ [ "roots:project,docs" ], "delegation.tool_reconfiguration"
    ; [ "shell-runtime"; "fixed-shell-tool" ], "delegation.execution_configuration"
    ; [ "optional-agent-tool" ], "delegation.tool_reconfiguration"
    ; [ "selected-mcp-tool" ], "delegation.tool_reconfiguration"
    ; [ "document"; "image"; "inline-agent" ], "delegation.message_admission"
    ; [ "stored-call"; "stored-response" ], "delegation.history_admission"
    ]
  in
  let host =
    V.create_host
      ~runtime_identity:"root-capability-doc-check-v1"
      ~targets:[ Generated_chatmd ]
      ~moderator_surface:Delegated
      ~compilation:Chatml_compilation.default_limits
    |> Result.ok_or_failwith
  in
  let capabilities = Docs_child_authoring.fixture_capabilities () in
  List.iter2_exn examples expected ~f:(fun source (expected, rejected_code) ->
    let parsed =
      CM.parse_chat_inputs ~source:"agent.chatmd" ~dir:(Eio.Stdenv.cwd env) source
    in
    [%test_eq: string list] expected (summary parsed);
    let report =
      V.validate
        ~env
        ~host
        ~capabilities
        (`Object
            [ "version", `Number "1"
            ; "target", `String "generated_chatmd"
            ; "root_file", `String "agent.chatmd"
            ; ( "sources"
              , `Array
                  [ `Object [ "path", `String "agent.chatmd"; "text", `String source ]
                  ; `Object
                      [ "path", `String "reviewer.chatmd"
                      ; "text", `String "<developer>Review supplied text.</developer>"
                      ]
                  ] )
            ; "tools", `Array []
            ])
    in
    match
      List.exists report.diagnostics ~f:(fun issue ->
        String.equal issue.diagnostic.code rejected_code)
    with
    | true -> ()
    | false -> failwith (Jsonaf.to_string (V.to_json report)));
  Eio.Flow.copy_string
    (sprintf
       "ChatMD capability reference: %d root examples parse and reject at the \
        generated-child boundary without resource execution PASS (offline)\n"
       (List.length examples))
    (Eio.Stdenv.stdout env)
;;
