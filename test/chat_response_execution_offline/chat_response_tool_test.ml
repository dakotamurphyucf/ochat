open! Core
module CM = Prompt.Chat_markdown
module Event = Chat_response.Tool_execution_event
module Tool = Chat_response.Tool

let source =
  let position : Chatmd_shell_spec.Source_ref.position =
    { offset = 0; line = 1; column = 1 }
  in
  Chatmd_shell_spec.Source_ref.create
    ~file:"test.chatmd"
    ~source_dir:"."
    ~prompt_dir:"."
    ~namespace:None
    ~start_pos:position
    ~end_pos:position
    ~source:""
;;

let classification = function
  | None -> "hidden"
  | Some (name, Event.Subagent) -> name ^ ":agent"
  | Some (name, Event.Shell_script) -> name ^ ":shell"
;;

let%expect_test "Agent page includes subagents and manifest shell tools" =
  [ CM.Agent
      { name = "research"
      ; description = None
      ; agent = "researcher.chatmd"
      ; is_local = true
      }
  ; CM.Custom { name = "legacy"; description = None; command = "printf"; source }
  ; CM.Builtin "read_file"
  ]
  |> List.map ~f:(fun declaration ->
    Tool.agent_page_classification declaration |> classification)
  |> List.iter ~f:print_endline;
  [%expect
    {|
    research:agent
    legacy:shell
    hidden
    |}]
;;
