open Core
module CM = Prompt.Chat_markdown

let stub_run_agent ?prompt_dir:_ ?session_id:_ ~ctx:_ _prompt _items = "nested"

let id_exn namespace sequence =
  History_entry.Id.create ~namespace ~sequence |> Result.ok_or_failwith
;;

let allocator_exn namespace next_sequence =
  History_entry.Allocator.create ~namespace ~next_sequence |> Result.ok_or_failwith
;;

let materialize ~env ~namespace source =
  let dir = Eio.Stdenv.cwd env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Chat_response.Ctx.create ~env ~dir ~cache ~tool_dir:dir in
  let elements = CM.parse_chat_inputs ~dir source in
  Chat_tui.History_materialization.from_prompt
    ~allocator:(allocator_exn namespace 0)
    ~ctx
    ~run_agent:stub_run_agent
    elements
;;

let reasoning_item id : Openai.Responses.Item.t =
  Openai.Responses.Item.Reasoning { summary = []; id; status = None; _type = "reasoning" }
;;

let%expect_test "ID-less prompts allocate IDs and explicit duplicates fail" =
  Eio_main.run
  @@ fun env ->
  let fresh =
    materialize ~env ~namespace:"new" "<user>same</user><user>same</user>"
    |> Result.ok_or_failwith
  in
  let explicit = History_entry.Id.to_string (id_exn "imported" 7) in
  let duplicate_source =
    sprintf
      "<user ochat-history-id=%S>a</user><assistant ochat-history-id=%S>b</assistant>"
      explicit
      explicit
  in
  let duplicate_rejected =
    Result.is_error (materialize ~env ~namespace:"new" duplicate_source)
  in
  print_s
    [%sexp
      { ids =
          (List.map fresh ~f:(fun entry ->
             History_entry.id entry |> History_entry.Id.to_string)
           : string list)
      ; duplicate_rejected : bool
      }];
  [%expect {| ((ids (3:new:0 3:new:1)) (duplicate_rejected true)) |}]
;;

let%expect_test "same-namespace explicit IDs advance fresh allocation" =
  Eio_main.run
  @@ fun env ->
  let explicit = History_entry.Id.to_string (id_exn "same" 7) in
  let source =
    sprintf
      "<user ochat-history-id=%S>explicit</user><assistant>fresh</assistant>"
      explicit
  in
  let entries = materialize ~env ~namespace:"same" source |> Result.ok_or_failwith in
  print_s
    [%sexp
      (List.map entries ~f:(fun entry ->
         History_entry.id entry |> History_entry.Id.to_string)
       : string list)];
  [%expect {| (4:same:7 4:same:8) |}]
;;

let%expect_test "multipart developer input remains one history entry" =
  Eio_main.run
  @@ fun env ->
  let source =
    "<developer>first<img src=\"https://example.test/image\"/>second</developer>"
  in
  let entries = materialize ~env ~namespace:"multipart" source |> Result.ok_or_failwith in
  let part_count =
    match History_entry.items entries with
    | [ Openai.Responses.Item.Input_message message ] -> List.length message.content
    | _ -> 0
  in
  print_s [%sexp { entry_count = (List.length entries : int); part_count : int }];
  [%expect {| ((entry_count 1) (part_count 3)) |}]
;;

let%expect_test "developer document import remains one history entry" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(dir / "identity-materialization-doc.txt")
    "document body";
  let source =
    "<developer>before<doc src=\"identity-materialization-doc.txt\" \
     local/>after</developer>"
  in
  let entries = materialize ~env ~namespace:"document" source |> Result.ok_or_failwith in
  let text_parts =
    match History_entry.items entries with
    | [ Openai.Responses.Item.Input_message message ] ->
      List.filter_map message.content ~f:(function
        | Text { text; _ } -> Some text
        | Image _ -> None)
    | _ -> []
  in
  print_s [%sexp { entry_count = (List.length entries : int); text_parts : string list }];
  [%expect
    {|
    ((entry_count 1) (text_parts (before "document body" after)))
    |}]
;;

let%expect_test "import collisions report both source files" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let encoded = History_entry.Id.to_string (id_exn "imports" 1) in
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(dir / "identity-import-a.chatmd")
    (sprintf "<user ochat-history-id=%S>a</user>" encoded);
  Eio.Path.save
    ~create:(`Or_truncate 0o600)
    Eio.Path.(dir / "identity-import-b.chatmd")
    (sprintf "<assistant ochat-history-id=%S>b</assistant>" encoded);
  let source =
    "<import src=\"identity-import-a.chatmd\"/><import src=\"identity-import-b.chatmd\"/>"
  in
  let error =
    match materialize ~env ~namespace:"new" source with
    | Error error -> error
    | Ok _ -> failwith "expected duplicate imported IDs"
  in
  print_s
    [%sexp
      { first_source =
          (String.is_substring error ~substring:"identity-import-a.chatmd" : bool)
      ; second_source =
          (String.is_substring error ~substring:"identity-import-b.chatmd" : bool)
      }];
  [%expect {| ((first_source true) (second_source true)) |}]
;;

let%expect_test "entry export/import preserves identity separately from provider metadata"
  =
  Eio_main.run
  @@ fun env ->
  let allocator = allocator_exn "session" 0 in
  let call : Openai.Responses.Item.t =
    Function_call
      { arguments = {|{"x":1}|}
      ; call_id = "call-1"
      ; name = "tool"
      ; _type = "function_call"
      ; id = Some "provider-call"
      ; status = None
      }
  in
  let output : Openai.Responses.Item.t =
    Function_call_output
      { call_id = "call-1"
      ; output = Openai.Responses.Tool_output.Output.Text "ok"
      ; _type = "function_call_output"
      ; id = None
      ; status = None
      }
  in
  let entries =
    [ History_entry.create ~allocator call |> Result.ok_or_failwith
    ; History_entry.create ~allocator output |> Result.ok_or_failwith
    ]
  in
  let chatmd =
    Chat_tui.Persistence.history_entries_as_chatmd
      ~moderator_snapshot:None
      ~history:entries
  in
  let imported = materialize ~env ~namespace:"unused" chatmd |> Result.ok_or_failwith in
  let ids history =
    List.map history ~f:(fun entry ->
      History_entry.id entry |> History_entry.Id.to_string)
  in
  let provider_and_call_id_preserved =
    match History_entry.items imported with
    | [ Function_call call; Function_call_output output ] ->
      Option.equal String.equal call.id (Some "provider-call")
      && String.equal call.call_id output.call_id
    | _ -> false
  in
  print_s
    [%sexp
      { ids_preserved = (List.equal String.equal (ids entries) (ids imported) : bool)
      ; provider_and_call_id_preserved : bool
      }];
  [%expect {| ((ids_preserved true) (provider_and_call_id_preserved true)) |}]
;;

let%expect_test "resume and checkpoints use persisted identities" =
  let allocator = allocator_exn "resume" 0 in
  let first =
    History_entry.create ~allocator (reasoning_item "same") |> Result.ok_or_failwith
  in
  let second =
    History_entry.create ~allocator (reasoning_item "same") |> Result.ok_or_failwith
  in
  let checkpoint = Chat_tui.Persistence.Checkpoint.of_entries [ first ] in
  let replaced = History_entry.with_item first (reasoning_item "replacement") in
  let suffix =
    Chat_tui.Persistence.entries_after_checkpoint checkpoint [ second; first; replaced ]
  in
  print_s
    [%sexp
      (List.map suffix ~f:(fun entry ->
         History_entry.id entry |> History_entry.Id.to_string)
       : string list)];
  [%expect {| (6:resume:1 6:resume:0) |}]
;;

let%expect_test "nonempty staged session resumes without converting prompt" =
  Eio_main.run
  @@ fun env ->
  let allocator = allocator_exn "resume-session" 0 in
  let entry =
    History_entry.create ~allocator (reasoning_item "persisted") |> Result.ok_or_failwith
  in
  let session : Session.V4.t =
    { version = Session.V4.version
    ; id = "resume-session"
    ; prompt_file = "prompt.chatmd"
    ; local_prompt_copy = None
    ; history = [ entry ]
    ; next_history_sequence = 1
    ; tasks = []
    ; moderator_state = Session.V4.Moderator_state.of_legacy None
    ; kv_store = []
    ; vfs_root = "vfs"
    }
  in
  let dir = Eio.Stdenv.cwd env in
  let cache = Chat_response.Cache.create ~max_size:5 () in
  let ctx = Chat_response.Ctx.create ~env ~dir ~cache ~tool_dir:dir in
  let run_agent ?prompt_dir:_ ?session_id:_ ~ctx:_ _prompt _items =
    failwith "prompt conversion should not run"
  in
  let resumed =
    Chat_tui.History_materialization.resume_or_materialize
      ~session:(Some session)
      ~allocator
      ~ctx
      ~run_agent
      []
    |> Result.ok_or_failwith
  in
  print_s
    [%sexp
      (History_entry.Id.equal
         (History_entry.id entry)
         (History_entry.id (List.hd_exn resumed))
       : bool)];
  [%expect {| true |}]
;;

let%expect_test "duplicate payload export/import keeps occurrence IDs" =
  Eio_main.run
  @@ fun env ->
  let allocator = allocator_exn "duplicates" 0 in
  let payload = reasoning_item "same-provider" in
  let first = History_entry.create ~allocator payload |> Result.ok_or_failwith in
  let second = History_entry.create ~allocator payload |> Result.ok_or_failwith in
  let entries = [ first; second ] in
  let exported =
    Chat_tui.Persistence.history_entries_as_chatmd
      ~moderator_snapshot:None
      ~history:entries
  in
  let imported = materialize ~env ~namespace:"import" exported |> Result.ok_or_failwith in
  print_s
    [%sexp
      (List.map imported ~f:(fun entry ->
         History_entry.id entry |> History_entry.Id.to_string)
       : string list)];
  [%expect {| (10:duplicates:0 10:duplicates:1) |}]
;;

let%expect_test "persisting a compacted history uses identity and revision" =
  Eio_main.run
  @@ fun env ->
  let dir = Eio.Stdenv.cwd env in
  let allocator = allocator_exn "compaction" 0 in
  let retained =
    History_entry.create ~allocator (reasoning_item "retained") |> Result.ok_or_failwith
  in
  let replaced = History_entry.with_item retained (reasoning_item "replacement") in
  let summary =
    History_entry.create ~allocator (reasoning_item "summary") |> Result.ok_or_failwith
  in
  let checkpoint = Chat_tui.Persistence.Checkpoint.of_entries [ retained ] in
  let prompt_file = "identity-compaction-export.chatmd" in
  Io.save_doc ~dir prompt_file "<system>prompt</system>\n";
  Chat_tui.Persistence.persist_entries
    ~dir
    ~prompt_file
    ~checkpoint
    ~moderator_snapshot:None
    ~history:[ replaced; summary ];
  let exported = Io.load_doc ~dir prompt_file in
  let retained_id = History_entry.id retained |> History_entry.Id.to_string in
  let summary_id = History_entry.id summary |> History_entry.Id.to_string in
  print_s
    [%sexp
      { replacement_exported =
          (String.is_substring exported ~substring:retained_id : bool)
      ; summary_exported = (String.is_substring exported ~substring:summary_id : bool)
      ; original_omitted =
          (not (String.is_substring exported ~substring:"retained") : bool)
      }];
  [%expect
    {|
    ((replacement_exported true) (summary_exported true) (original_omitted true))
    |}]
;;
