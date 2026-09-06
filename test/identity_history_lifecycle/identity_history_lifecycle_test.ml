open Core
module CM = Prompt.Chat_markdown
module Manager = Chat_response.Moderator_manager
module Moderation = Chat_response.Moderation
module Res = Openai.Responses

let ok_or_fail = function
  | Ok value -> value
  | Error error -> failwith error
;;

let input_text text = Res.Input_message.Text { text; _type = "input_text" }

let output_text text =
  { Res.Output_message.annotations = []; text; _type = "output_text" }
;;

let stub_run_agent ?prompt_dir:_ ?session_id:_ ~ctx:_ _prompt _items = "nested"

let materialize_prompt ~env ~allocator =
  let dir = Eio.Stdenv.cwd env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Chat_response.Ctx.create ~env ~dir ~cache ~tool_dir:dir in
  let source =
    "<developer>policy-a<img \
     src=\"https://example.test/image\"/>policy-b</developer><user>start</user>"
  in
  CM.parse_chat_inputs ~dir source
  |> Chat_tui.History_materialization.from_prompt
       ~allocator
       ~ctx
       ~run_agent:stub_run_agent
  |> ok_or_fail
;;

let function_events () =
  let call : Res.Function_call.t =
    { name = "echo"
    ; arguments = ""
    ; call_id = "call-f"
    ; _type = "function_call"
    ; id = Some "provider-function"
    ; status = Some "in_progress"
    }
  in
  [ Res.Response_stream.Output_item_added
      { item = Function_call call
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Res.Response_stream.Function_call_arguments_done
      { arguments = "function"
      ; item_id = "provider-function"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.done"
      }
  ]
;;

let custom_events () =
  let call : Res.Custom_tool_call.t =
    { name = "echo"
    ; input = ""
    ; call_id = "call-c"
    ; _type = "custom_tool_call"
    ; id = Some "provider-custom"
    }
  in
  [ Res.Response_stream.Output_item_added
      { item = Custom_function call
      ; output_index = 1
      ; type_ = "response.output_item.added"
      }
  ; Res.Response_stream.Custom_tool_call_input_done
      { input = "custom"
      ; item_id = "provider-custom"
      ; output_index = 1
      ; type_ = "response.custom_tool_call_input.done"
      }
  ]
;;

let final_events () =
  let reasoning : Res.Reasoning.t =
    { summary = [ { text = "thought"; _type = "summary_text" } ]
    ; id = "provider-reasoning"
    ; status = Some "completed"
    ; _type = "reasoning"
    }
  in
  let message : Res.Output_message.t =
    { role = Assistant
    ; id = "provider-assistant"
    ; content = [ output_text "done" ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
  in
  [ Res.Response_stream.Output_item_added
      { item = Reasoning reasoning
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Res.Response_stream.Output_item_done
      { item = Reasoning reasoning
      ; output_index = 0
      ; type_ = "response.output_item.done"
      }
  ; Res.Response_stream.Output_item_added
      { item = Output_message message
      ; output_index = 1
      ; type_ = "response.output_item.added"
      }
  ; Res.Response_stream.Output_item_done
      { item = Output_message message
      ; output_index = 1
      ; type_ = "response.output_item.done"
      }
  ]
;;

let stream_history ~env ~allocator history =
  let responses =
    Queue.of_list [ function_events () @ custom_events (); final_events () ]
  in
  let post_stream ~sw:_ ~inputs:_ = Queue.dequeue_exn responses |> Stdlib.List.to_seq in
  let tools = Hashtbl.create (module String) in
  Hashtbl.set tools ~key:"echo" ~data:(fun ~invocation:_ payload ->
    Res.Tool_output.Output.Text payload);
  Chat_response.In_memory_stream.run_completion_stream_in_memory_entries
    ~env
    ~allocator
    ~history
    ~tools:(Some [])
    ~tool_tbl:tools
    ~parallel_tool_calls:true
    ~post_stream
    ()
;;

let find_entry history predicate =
  List.find_exn history ~f:(fun entry -> predicate (History_entry.item entry))
;;

let moderator_artifact ~replace_id ~delete_id =
  let source =
    sprintf
      {|
        type state = { seen : int }
        type event = [ `Turn_start ]
        let initial_state = { seen = 0 }
        let on_event : context -> state -> event -> state task =
          fun ctx st ev ->
            match ev with
            | `Turn_start ->
              Task.bind(Turn.prepend_system("moderator-policy"), fun ignored_prepend ->
              Task.bind(
                Turn.replace_item(
                  "%s",
                  Item.input_text_message("replacement", "developer", "rewritten")),
                fun ignored_replace ->
              Task.bind(Turn.delete_item("%s"), fun ignored_delete ->
              Task.bind(
                Turn.append_item(
                  Item.output_text_message("append", "moderator-tail")),
                fun ignored_append ->
              Task.pure({ seen = st.seen + 1 })))))
      |}
      (History_entry.Id.to_string replace_id)
      (History_entry.Id.to_string delete_id)
  in
  let script =
    CM.
      { id = "identity-lifecycle"
      ; language = "chatml"
      ; kind = "moderator"
      ; source = Inline source
      }
  in
  Manager.Registry.compile_script Manager.Registry.empty script |> ok_or_fail |> snd
;;

let provenance_name = function
  | Moderation.Effective_entry.Canonical -> "canonical"
  | Moderator_inserted _ -> "inserted"
  | Moderator_replacement _ -> "replacement"
;;

let entry_kind entry =
  match History_entry.item entry with
  | Res.Item.Input_message { role = Developer; _ } -> "developer"
  | Input_message _ -> "input"
  | Function_call _ -> "function-call"
  | Custom_tool_call _ -> "custom-call"
  | Function_call_output _ -> "function-output"
  | Custom_tool_call_output _ -> "custom-output"
  | Reasoning _ -> "reasoning"
  | Output_message _ -> "assistant"
  | Web_search_call _ -> "web-search"
  | File_search_call _ -> "file-search"
;;

let call_id = function
  | Res.Item.Function_call call -> Some call.call_id
  | Custom_tool_call call -> Some call.call_id
  | Function_call_output output -> Some output.call_id
  | Custom_tool_call_output output -> Some output.call_id
  | Input_message _
  | Output_message _
  | Reasoning _
  | Web_search_call _
  | File_search_call _ -> None
;;

let%expect_test
    "identity lifecycle survives streaming, moderation, compaction, and reload"
  =
  Eio_main.run
  @@ fun env ->
  let allocator =
    History_entry.Allocator.create ~namespace:"identity-lifecycle" ~next_sequence:0
    |> ok_or_fail
  in
  let initial = materialize_prompt ~env ~allocator in
  let developer = List.hd_exn initial in
  let multipart_parts =
    match History_entry.item developer with
    | Res.Item.Input_message message -> List.length message.content
    | _ -> 0
  in
  let streamed = stream_history ~env ~allocator initial in
  let reasoning =
    find_entry streamed (function
      | Res.Item.Reasoning _ -> true
      | _ -> false)
  in
  let artifact =
    moderator_artifact
      ~replace_id:(History_entry.id developer)
      ~delete_id:(History_entry.id reasoning)
  in
  let manager =
    Manager.create_entries
      ~artifact
      ~capabilities:Moderation.Capabilities.default
      ~allocator
      ()
    |> ok_or_fail
  in
  let subscription = Manager.subscribe_committed_changes manager ~on_wakeup:ignore in
  ignore
    (Manager.handle_event_entries
       manager
       ~session_id:"identity-lifecycle"
       ~now_ms:13
       ~history:streamed
       ~available_tools:[]
       ~session_meta:`Null
       ~event:Moderation.Event.Turn_start
     |> ok_or_fail
     : Moderation.Outcome.t);
  let change_count = Manager.drain_committed_changes subscription |> List.length in
  let effective = Manager.effective_entries manager streamed in
  let projection = Chat_tui.Conversation.project_effective_entries effective in
  let selected_id =
    Chat_tui.Projected_message.Id.canonical (History_entry.id developer)
  in
  let selected_before =
    Option.is_some (Chat_tui.Conversation.index_of_id projection selected_id)
  in
  let tool_relations =
    List.filter_map streamed ~f:(fun entry ->
      Option.map
        (call_id (History_entry.item entry))
        ~f:(fun call_id -> call_id, History_entry.Id.to_string (History_entry.id entry)))
  in
  let compacted =
    Context_compaction.Compactor.For_testing.compact_entries_with
      ~summarise:(fun ~relevant_items:_ ~env:_ -> Ok "compacted")
      ~allocator
      ~env:None
      ~history:streamed
    |> Result.ok_exn
  in
  let snapshot = Manager.identity_snapshot manager |> ok_or_fail in
  let session =
    Session.create
      ~id:"identity-lifecycle"
      ~prompt_file:"prompt.chatmd"
      ~history:compacted
      ~next_history_sequence:(History_entry.Allocator.next_sequence allocator)
      ~moderator_state:
        { legacy_snapshot = None; identity_snapshot = Some snapshot; extensions = [] }
      ()
  in
  let tmp =
    Filename.concat
      Filename.temp_dir_name
      ("identity-lifecycle-" ^ Int.to_string (Random.int 1_000_000))
  in
  Core_unix.mkdir_p tmp;
  let path = Eio.Path.(Eio.Stdenv.fs env / Filename.concat tmp "snapshot.bin") in
  Session.Io.File.write path session;
  let loaded = Session.Io.File.read path in
  let restored_allocator = Session.allocator loaded |> ok_or_fail in
  let restored =
    Manager.create_entries
      ~artifact
      ~capabilities:Moderation.Capabilities.default
      ~allocator:restored_allocator
      ~snapshot:(Option.value_exn loaded.moderator_state.identity_snapshot)
      ()
    |> ok_or_fail
  in
  let restored_effective = Manager.effective_entries restored loaded.history in
  let restored_projection =
    Chat_tui.Conversation.project_effective_entries restored_effective
  in
  let selected_after =
    Option.is_some (Chat_tui.Conversation.index_of_id restored_projection selected_id)
  in
  let ids_preserved =
    List.equal
      History_entry.Id.equal
      (List.map compacted ~f:History_entry.id)
      (List.map loaded.history ~f:History_entry.id)
  in
  let relation_counts =
    List.map [ "call-f"; "call-c" ] ~f:(fun expected ->
      List.count tool_relations ~f:(fun (actual, _) -> String.equal actual expected))
  in
  print_s
    [%sexp
      { prompt_entries = (List.length initial : int)
      ; multipart_parts : int
      ; streamed_kinds = (List.map streamed ~f:entry_kind : string list)
      ; unique_stream_ids =
          (not
             (List.contains_dup
                (List.map streamed ~f:History_entry.id)
                ~compare:History_entry.Id.compare)
           : bool)
      ; relation_counts : int list
      ; change_count : int
      ; provenance =
          (List.map restored_effective ~f:(fun entry -> provenance_name entry.provenance)
           : string list)
      ; selected_before : bool
      ; selected_after : bool
      ; compacted_kinds = (List.map loaded.history ~f:entry_kind : string list)
      ; ids_preserved : bool
      ; watermark =
          (( loaded.next_history_sequence
           , History_entry.Allocator.next_sequence restored_allocator )
           : int * int)
      }];
  Manager.unsubscribe subscription;
  [%expect
    {|
    ((prompt_entries 2) (multipart_parts 3)
     (streamed_kinds
      (developer input function-call custom-call function-output custom-output
       reasoning assistant))
     (unique_stream_ids true) (relation_counts (2 2)) (change_count 1)
     (provenance (inserted replacement canonical inserted))
     (selected_before true) (selected_after true)
     (compacted_kinds (developer input)) (ids_preserved true)
     (watermark (11 11)))
    |}]
;;
