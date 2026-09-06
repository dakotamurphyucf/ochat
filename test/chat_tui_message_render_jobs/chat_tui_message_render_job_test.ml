open Core
module Job = Chat_tui.Chat_message_render_job

let image_to_string image =
  let width = Notty.I.width image in
  let height = Notty.I.height image in
  let buffer = Buffer.create 1024 in
  Notty.Render.to_buffer buffer Notty.Cap.ansi (0, 0) (width, height) image;
  Buffer.contents buffer
;;

let create_job
      ?(message_index = 3)
      ?(message_revision = 5)
      ?(width = 48)
      ?(role = "assistant")
      ?(text = "hello")
      ?tool_output
      ?tool_call_outcome
      ?(grammar_generation = 7)
      ()
  =
  let row_id =
    Chat_tui.Projected_message.Id.local
      ~namespace:"render-job-test"
      ~local_id:(Int.to_string message_index)
    |> Result.ok_or_failwith
  in
  Job.create
    ~transcript_generation:11
    ~row_id
    ~row_revision:message_revision
    ~message_index
    ~message_revision
    ~width
    ~role
    ~text
    ~tool_output
    ~tool_call_outcome
    ~theme_generation:2
    ~grammar_generation
    ~geometry_generation:17
    ~request_generation:13
    ~render_generation:29
    ~submission_generation:31
    ~semantic_seed:None
    ~priority:Visible
;;

let check_parity
      ?width
      ?role
      ?text
      ?(selected = false)
      ?search_query
      ?tool_output
      ?tool_call_outcome
      ()
  =
  let job = create_job ?width ?role ?text ?tool_output ?tool_call_outcome () in
  let key = job.key in
  let hi_engine = Chat_tui.Renderer_highlight_engine.get () in
  Chat_tui.Renderer_component_message.clear_code_cache ();
  let synchronous =
    Chat_tui.Renderer_component_message.render_message
      ~width:key.width
      ~selected
      ~tool_output:key.tool_output
      ~role:key.role
      ~text:key.text
      ~hi_engine
      ?search_query
      ?tool_call_outcome:key.tool_call_outcome
      ()
  in
  let runtime =
    Job.Runtime.create
      ~hi_engine
      ~theme_generation:key.theme_generation
      ~grammar_generation:key.grammar_generation
      ~code_cache:(Job.Code_cache.create ~capacity:16)
      ()
  in
  let detached = Chat_tui.Renderer_component_message.render_detached ~runtime job in
  let detached_image =
    Chat_tui.Renderer_component_message.apply_overlay
      ~selected
      ~search_query:(Option.join search_query)
      detached.layout
  in
  [%test_eq: string] (image_to_string synchronous) (image_to_string detached_image);
  [%test_eq: int] (Notty.I.height synchronous) detached.height
;;

let%test_unit "detached rendering matches synchronous message formatting" =
  check_parity
    ~text:"Markdown **bold**, _italic_, and `code`.\n\n```ocaml\nlet x = 1\n```"
    ();
  check_parity ~width:11 ~role:"tool" ~text:"read_file({\"file\":\"lib/example.ml\"})" ();
  check_parity
    ~width:18
    ~role:"tool"
    ~text:
      "read_file({\n\
      \  \"file\":\"lib/example.ml\",\n\
      \  \"offset\":12,\n\
      \  \"flags\":[true,null]\n\
       })"
    ();
  check_parity
    ~width:18
    ~role:"tool"
    ~text:"read_file({\"file\":\"lib/example.ml\",\"offset\":"
    ();
  check_parity
    ~role:"tool_output"
    ~text:"Patch applied\n\n*** Begin Patch\n+hello\n*** End Patch"
    ~tool_output:Chat_tui.Types.Apply_patch
    ();
  check_parity
    ~role:"tool_output"
    ~text:
      "*** Begin Patch\n\
       *** Update File: lib/x.ml\n\
       *** Move to: lib/y.ml\n\
       @@ let f\n\
       -old\n\
       +new\n\
      \ context\n\
       *** End Patch"
    ~tool_output:Chat_tui.Types.Apply_patch
    ();
  check_parity
    ~role:"tool_output"
    ~text:"Patch generated:\n*** Begin Patch\n*** Delete File: old.ml\n*** End Patch"
    ~tool_output:Chat_tui.Types.Apply_patch
    ();
  check_parity
    ~role:"tool_output"
    ~text:"let answer = 42"
    ~tool_output:(Read_file { path = Some "answer.ml" })
    ();
  check_parity
    ~role:"tool_output"
    ~text:"one\ntwo/"
    ~tool_output:(Read_directory { path = Some "." })
    ();
  check_parity ~selected:true ~search_query:(Some "needle") ~text:"before needle after" ();
  check_parity
    ~role:"tool"
    ~text:"run({})"
    ~tool_call_outcome:Ochat_function.Trace.Raised
    ();
  check_parity ~text:"malformed \xFF text" ()
;;

let%test_unit "tool call arguments retain JSON syntax attributes" =
  let text =
    {|shell({"script":"set -eu\nDOC_DIR=$(opam var eio:doc)\nprintf 'doc_dir=%s\\n' \"$DOC_DIR\"","rationale":"Locate Eio documentation.","retry":false})|}
  in
  let render hi_engine =
    let job = create_job ~width:160 ~role:"tool" ~text () in
    Chat_tui.Renderer_component_message.render_synchronously ~hi_engine job
  in
  let has_attr result expected text =
    let spans = List.concat result.Job.layout.lines in
    List.exists spans ~f:(fun (attr, span_text) ->
      Notty.A.equal attr expected && String.is_substring span_text ~substring:text)
  in
  let key_attr = Chat_tui.Highlight_styles.fg_hex "#B392F0" in
  let string_attr = Chat_tui.Highlight_styles.fg_hex "#9ECBFF" in
  let registered = render (Chat_tui.Renderer_highlight_engine.get ()) in
  let fallback =
    render
      (Chat_tui.Highlight_tm_engine.create ~theme:Chat_tui.Highlight_theme.github_dark)
  in
  List.iter [ registered; fallback ] ~f:(fun result ->
    assert (has_attr result key_attr "script");
    assert (has_attr result string_attr "set -eu"))
;;

let%test_unit "detached result repeats stale-work identity" =
  let job =
    create_job
      ~message_index:17
      ~message_revision:19
      ~width:31
      ~role:"tool_output"
      ~text:"contents"
      ~tool_output:(Read_file { path = Some "x.ml" })
      ~tool_call_outcome:Ochat_function.Trace.Cancelled
      ~grammar_generation:23
      ()
  in
  let runtime =
    Job.Runtime.create
      ~hi_engine:(Chat_tui.Renderer_highlight_engine.get ())
      ~theme_generation:job.key.theme_generation
      ~grammar_generation:job.key.grammar_generation
      ()
  in
  let result = Chat_tui.Renderer_component_message.render_detached ~runtime job in
  [%test_eq: int] result.key.transcript_generation 11;
  [%test_eq: int] result.key.message_index 17;
  [%test_eq: int] result.key.message_revision 19;
  [%test_eq: int] result.key.width 31;
  [%test_eq: string] result.key.role "tool_output";
  [%test_eq: string] result.key.text "contents";
  (match result.key.tool_output with
   | Some (Read_file { path = Some "x.ml" }) -> ()
   | _ -> assert false);
  (match result.key.tool_call_outcome with
   | Some Ochat_function.Trace.Cancelled -> ()
   | _ -> assert false);
  [%test_eq: int] result.key.theme_generation 2;
  [%test_eq: int] result.key.grammar_generation 23;
  [%test_eq: int] result.geometry_generation 17;
  [%test_eq: int] result.request_generation 13;
  [%test_eq: int] result.render_generation 29;
  [%test_eq: int] result.submission_generation 31;
  [%test_eq: int] result.height (Notty.I.height result.image);
  assert (Job.result_matches result job);
  let newer_request = { job with request_generation = job.request_generation + 1 } in
  let newer_render = { job with render_generation = job.render_generation + 1 } in
  let newer_submission =
    { job with submission_generation = job.submission_generation + 1 }
  in
  assert (not (Job.result_matches result newer_request));
  assert (not (Job.result_matches result newer_render));
  assert (not (Job.result_matches result newer_submission))
;;

let%test_unit "runtime generation mismatch rejects rendering" =
  let job = create_job () in
  let runtime =
    Job.Runtime.create
      ~hi_engine:(Chat_tui.Renderer_highlight_engine.get ())
      ~theme_generation:job.key.theme_generation
      ~grammar_generation:(job.key.grammar_generation + 1)
      ()
  in
  match Chat_tui.Renderer_component_message.render_detached ~runtime job with
  | exception Invalid_argument _ -> ()
  | _ -> assert false
;;

let%test_unit "prepared message cache is keyed by row identity and revision" =
  let cache = Job.Prepared_cache.create ~capacity:4 in
  let runtime =
    Job.Runtime.create
      ~hi_engine:(Chat_tui.Renderer_highlight_engine.get ())
      ~theme_generation:2
      ~grammar_generation:7
      ~prepared_cache:cache
      ()
  in
  let job =
    create_job
      ~text:"status\n\n*** Begin Patch\n+line"
      ~tool_output:Chat_tui.Types.Apply_patch
      ()
  in
  ignore (Chat_tui.Renderer_component_message.render_detached ~runtime job : Job.result);
  [%test_eq: int] (Job.Prepared_cache.length cache) 1;
  ignore
    (Chat_tui.Renderer_component_message.render_detached
       ~runtime
       { job with key = { job.key with width = 96 } }
     : Job.result);
  [%test_eq: int] (Job.Prepared_cache.length cache) 1;
  ignore
    (Chat_tui.Renderer_component_message.render_detached
       ~runtime
       { job with
         key =
           { job.key with
             row_revision = job.key.row_revision + 1
           ; message_revision = job.key.message_revision + 1
           ; text = job.key.text ^ "\n"
           }
       }
     : Job.result);
  [%test_eq: int] (Job.Prepared_cache.length cache) 1
;;

let%test_unit "tool JSON highlights are width-independent and generation-scoped" =
  let cache = Job.Highlight_cache.create ~capacity:16 in
  let render ~width ~grammar_generation =
    let job =
      create_job
        ~width
        ~role:"tool"
        ~text:"read_file({\"file\":\"lib/x.ml\",\"offset\":12})"
        ~grammar_generation
        ()
    in
    let runtime =
      Job.Runtime.create
        ~hi_engine:(Chat_tui.Renderer_highlight_engine.get ())
        ~theme_generation:job.key.theme_generation
        ~grammar_generation
        ~highlight_cache:cache
        ()
    in
    Chat_tui.Renderer_component_message.render_detached ~runtime job
  in
  let first = render ~width:30 ~grammar_generation:7 in
  let after_first = Job.Highlight_cache.length cache in
  let second = render ~width:60 ~grammar_generation:7 in
  [%test_eq: int] (Job.Highlight_cache.length cache) after_first;
  let third = render ~width:60 ~grammar_generation:8 in
  assert (Job.Highlight_cache.length cache > after_first);
  assert (not (List.is_empty first.highlights));
  assert (not (List.is_empty second.highlights));
  assert (not (List.is_empty third.highlights))
;;

let render_result ?(width = 48) text =
  let job = create_job ~width ~text () in
  let runtime =
    Job.Runtime.create
      ~hi_engine:(Chat_tui.Renderer_highlight_engine.get ())
      ~theme_generation:job.key.theme_generation
      ~grammar_generation:job.key.grammar_generation
      ()
  in
  Chat_tui.Renderer_component_message.render_detached ~runtime job
;;

let%test_unit "layout reuse requires identical recorded wrap breaks" =
  let wrapped = render_result ~width:4 "abcdef" in
  let same_height_different_breaks = render_result ~width:5 "abcdef" in
  [%test_eq: int] wrapped.height same_height_different_breaks.height;
  assert (Job.Layout_plan.allows wrapped.layout_plan ~width:4);
  assert (not (Job.Layout_plan.allows wrapped.layout_plan ~width:5));
  let unwrapped = render_result ~width:5 "abc" in
  assert (Job.Layout_plan.allows unwrapped.layout_plan ~width:3);
  assert (Job.Layout_plan.allows unwrapped.layout_plan ~width:12);
  assert (not (Job.Layout_plan.allows unwrapped.layout_plan ~width:2))
;;

let%test_unit "selection and search reuse cached wrapped visual lines" =
  let cache = Job.Wrapped_cache.create ~capacity:8 in
  let runtime =
    Job.Runtime.create
      ~hi_engine:(Chat_tui.Renderer_highlight_engine.get ())
      ~theme_generation:2
      ~grammar_generation:7
      ~wrapped_cache:cache
      ()
  in
  let render selected search_query =
    let job = create_job ~width:12 ~text:"before needle after" () in
    let result = Chat_tui.Renderer_component_message.render_detached ~runtime job in
    ( Chat_tui.Renderer_component_message.apply_overlay
        ~selected
        ~search_query
        result.layout
    , result )
  in
  let _, base = render false None in
  let selected_image, selected = render true None in
  let searched_image, searched = render true (Some "needle") in
  let hits, misses = Job.Wrapped_cache.stats cache in
  [%test_eq: int] misses 1;
  assert (hits >= 2);
  [%test_eq: int] base.height selected.height;
  [%test_eq: int] base.height searched.height;
  [%test_eq: int] base.height (Notty.I.height selected_image);
  [%test_eq: int] base.height (Notty.I.height searched_image)
;;

let%test_unit "late search overlay covers fenced and read-file code" =
  let hi_engine = Chat_tui.Renderer_highlight_engine.get () in
  let check ?tool_output text =
    let job = create_job ~width:40 ~text ?tool_output () in
    let result =
      Chat_tui.Renderer_component_message.render_synchronously ~hi_engine job
    in
    let base =
      Chat_tui.Renderer_component_message.apply_overlay
        ~selected:false
        ~search_query:None
        result.layout
    in
    let searched =
      Chat_tui.Renderer_component_message.apply_overlay
        ~selected:true
        ~search_query:(Some "needle")
        result.layout
    in
    assert (not (String.equal (image_to_string base) (image_to_string searched)));
    [%test_eq: int] (Notty.I.height base) (Notty.I.height searched)
  in
  check "```ocaml\nlet needle = 1\n```";
  check
    ~tool_output:(Chat_tui.Types.Read_file { path = Some "answer.ml" })
    "let needle = 1"
;;

let%test_unit "render-job identity excludes selection and search overlays" =
  let job = create_job ~text:"needle" () in
  let result = render_result "needle" in
  assert (Job.Key.equal job.key result.key);
  let base =
    Chat_tui.Renderer_component_message.apply_overlay
      ~selected:false
      ~search_query:None
      result.layout
  in
  let selected =
    Chat_tui.Renderer_component_message.apply_overlay
      ~selected:true
      ~search_query:None
      result.layout
  in
  let searched =
    Chat_tui.Renderer_component_message.apply_overlay
      ~selected:true
      ~search_query:(Some "needle")
      result.layout
  in
  [%test_eq: int] (Notty.I.height base) (Notty.I.height selected);
  [%test_eq: int] (Notty.I.height base) (Notty.I.height searched)
;;

let make_model messages =
  Chat_tui.Model.create
    ~history_items:[]
    ~messages
    ~input_line:""
    ~auto_follow:true
    ~msg_buffers:(Hashtbl.create (module String))
    ~function_name_by_id:(Hashtbl.create (module String))
    ~reasoning_idx_by_id:(Hashtbl.create (module String))
    ~tool_output_by_index:(Hashtbl.create (module Int))
    ~tasks:[]
    ~kv_store:(Hashtbl.create (module String))
    ~fetch_sw:None
    ~scroll_box:(Notty_scroll_box.create Notty.I.empty)
    ~cursor_pos:0
    ~selection_anchor:None
    ~mode:Insert
    ~draft_mode:Plain
    ~selected_msg:None
    ~undo_stack:[]
    ~redo_stack:[]
    ~cmdline:""
    ~cmdline_cursor:0
;;

let revision_exn model index =
  Chat_tui.Model.message_revision model ~idx:index |> Option.value_exn
;;

let start_width_preparation
      model
      ~request_generation
      ~width
      ~theme_generation
      ~grammar_generation
  =
  Chat_tui.Model.start_width_preparation
    model
    ~request_generation
    ~terminal_size:(width, 24)
    ~layout:
      { Chat_tui.Model.Chat_page_state.input_box_height = 3
      ; history_height = 20
      ; sticky_height = 0
      ; scroll_height = 20
      }
    ~theme_generation
    ~grammar_generation
    ~anchor:(Chat_tui.Model.capture_resize_anchor model ~viewport_height:20)
;;

let render_preparation_job (job : Job.t) =
  let runtime =
    Job.Runtime.create
      ~hi_engine:(Chat_tui.Renderer_highlight_engine.get ())
      ~theme_generation:job.Job.key.theme_generation
      ~grammar_generation:job.key.grammar_generation
      ~highlight_cache:(Job.Highlight_cache.create ~capacity:32)
      ~prepared_cache:(Job.Prepared_cache.create ~capacity:8)
      ()
  in
  Chat_tui.Renderer_component_message.render_detached ~runtime job
;;

let%test_unit "target width jobs reuse compatible layouts and rerender incompatible wraps"
  =
  let model = make_model [ "assistant", "abc" ] in
  ignore
    (Chat_tui.Renderer_page_chat.render ~size:(5, 12) ~model : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(5, 12) ~model;
  start_width_preparation
    model
    ~request_generation:7
    ~width:4
    ~theme_generation:2
    ~grammar_generation:7;
  let jobs =
    Chat_tui.Renderer_page_chat.target_width_jobs ~indices:[ 0 ] ~priority:Visible ~model
  in
  [%test_eq: int] (List.length jobs) 0;
  let id, revision =
    Chat_tui.Model.render_row_identity model ~idx:0 |> Option.value_exn
  in
  assert (
    Option.is_some
      (Chat_tui.Model.find_width_preparation_row model ~request_generation:7 ~id));
  ignore
    (Chat_tui.Model.clear_width_preparation model ~request_generation:7
     : Chat_tui.Model.Chat_page_state.preparing_width option);
  start_width_preparation
    model
    ~request_generation:8
    ~width:2
    ~theme_generation:2
    ~grammar_generation:7;
  let jobs =
    Chat_tui.Renderer_page_chat.target_width_jobs ~indices:[ 0 ] ~priority:Visible ~model
  in
  [%test_eq: int] (List.length jobs) 1;
  let job = List.hd_exn jobs in
  [%test_eq: int] job.request_generation 8;
  [%test_eq: int] job.key.width 2;
  assert (Option.is_some job.semantic_seed);
  let result = render_preparation_job job in
  assert (
    Chat_tui.Model.commit_width_preparation_result
      model
      ~theme_generation:2
      ~grammar_generation:7
      result);
  assert (
    Option.is_some
      (Chat_tui.Model.find_width_preparation_row model ~request_generation:8 ~id));
  [%test_eq: int] revision result.key.row_revision
;;

let%test_unit "target result rejects stale generations and changed row metadata" =
  let model = make_model [ "assistant", "abc" ] in
  start_width_preparation
    model
    ~request_generation:11
    ~width:2
    ~theme_generation:2
    ~grammar_generation:7;
  let job =
    Chat_tui.Renderer_page_chat.target_width_jobs ~indices:[ 0 ] ~priority:Visible ~model
    |> List.hd_exn
  in
  let result = render_preparation_job job in
  assert (
    not
      (Chat_tui.Model.commit_width_preparation_result
         model
         ~theme_generation:2
         ~grammar_generation:8
         result));
  Chat_tui.Model.set_messages model [ "assistant", "changed" ];
  assert (
    not
      (Chat_tui.Model.commit_width_preparation_result
         model
         ~theme_generation:2
         ~grammar_generation:7
         result))
;;

let%test_unit "exact visible corridor publishes atomically after bounded target work" =
  let messages =
    List.init 100 ~f:(fun index -> "assistant", sprintf "row %d with wrapping text" index)
  in
  let model = make_model messages in
  ignore
    (Chat_tui.Renderer_page_chat.render ~size:(40, 24) ~model : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(40, 24) ~model;
  Chat_tui.Model.follow_chat_bottom model ~viewport_height:20;
  start_width_preparation
    model
    ~request_generation:19
    ~width:24
    ~theme_generation:0
    ~grammar_generation:(Chat_tui.Highlight_registry.generation ());
  let plan, batches =
    Chat_tui.Renderer_page_chat.initial_target_width_batches ~model () |> Option.value_exn
  in
  Chat_tui.Model.set_chat_materialization_resizing model;
  assert (
    not (Chat_tui.Model.publish_width_preparation_corridor model ~request_generation:19));
  let jobs =
    List.concat_map batches ~f:(fun (_, jobs) ->
      List.map jobs ~f:(fun ranked -> ranked.Chat_tui.Chat_render_worker.job))
  in
  List.iter jobs ~f:(fun job ->
    let result = render_preparation_job job in
    assert (
      Chat_tui.Model.commit_width_preparation_result
        model
        ~theme_generation:0
        ~grammar_generation:(Chat_tui.Highlight_registry.generation ())
        result));
  assert (Chat_tui.Model.publish_width_preparation_corridor model ~request_generation:19);
  [%test_eq: int option] (Chat_tui.Model.active_history_width model) (Some 24);
  assert (
    Poly.equal
      (Chat_tui.Model.chat_materialization model)
      Chat_tui.Model.Chat_page_state.Corridor);
  let cache = Chat_tui.Model.corridor_history_cache model |> Option.value_exn in
  [%test_eq: int] cache.width 24;
  [%test_eq: int]
    (Notty.I.height cache.image)
    (Chat_tui.Renderer_virtual_list.Geometry.total_height
       (Chat_tui.Model.chat_render_geometry model));
  let viewport =
    Chat_tui.Renderer_virtual_list.Viewport.compute
      ~geometry:(Chat_tui.Model.chat_render_geometry model)
      ~requested_scroll:(Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model))
      ~height:20
      ~follow_bottom:(Chat_tui.Model.auto_follow model)
  in
  assert (
    Chat_tui.Renderer_virtual_list.Viewport.is_exact
      ~geometry:(Chat_tui.Model.chat_render_geometry model)
      viewport);
  assert (Chat_tui.History_chunk.Range.length plan.scheduled_rows < List.length messages);
  let first_scroll, last_scroll =
    Chat_tui.Model.prepared_scroll_interval model ~viewport_height:20 |> Option.value_exn
  in
  assert (
    Chat_tui.Model.requested_scroll_is_prepared
      model
      ~viewport_height:20
      ~requested_scroll:first_scroll);
  assert (
    Chat_tui.Model.requested_scroll_is_prepared
      model
      ~viewport_height:20
      ~requested_scroll:last_scroll);
  let (up, clamped, repeated, down), materialized =
    Chat_tui.Renderer_page_chat.For_testing.capture_materialized_indices (fun () ->
      let up = Chat_tui.Model.scroll_chat model ~viewport_height:20 (-1) in
      let clamped = Chat_tui.Model.scroll_chat model ~viewport_height:20 (-1_000_000) in
      let repeated = Chat_tui.Model.scroll_chat model ~viewport_height:20 (-1_000_000) in
      let down = Chat_tui.Model.scroll_chat model ~viewport_height:20 1_000_000 in
      up, clamped, repeated, down)
  in
  assert up.changed;
  assert clamped.changed;
  assert clamped.clamped;
  assert (not repeated.changed);
  assert repeated.clamped;
  assert down.changed;
  assert (Chat_tui.Model.auto_follow model);
  [%test_eq: int] (Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model)) last_scroll;
  [%test_eq: int list] materialized []
;;

let%test_unit "stable destination plans bounded exact work and rejects stale revision" =
  let messages =
    List.init 100 ~f:(fun index -> "assistant", sprintf "row %d with wrapping text" index)
  in
  let model = make_model messages in
  ignore
    (Chat_tui.Renderer_page_chat.render ~size:(40, 24) ~model : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(40, 24) ~model;
  Chat_tui.Model.follow_chat_bottom model ~viewport_height:20;
  start_width_preparation
    model
    ~request_generation:23
    ~width:24
    ~theme_generation:0
    ~grammar_generation:(Chat_tui.Highlight_registry.generation ());
  let id, revision =
    Chat_tui.Model.render_row_identity model ~idx:0 |> Option.value_exn
  in
  assert (
    Chat_tui.Model.set_width_preparation_destination
      model
      ~request_generation:23
      (Some
         { Chat_tui.Model.Chat_page_state.Destination.id
         ; revision
         ; reason = Earlier_conversation
         ; placement = Top
         }));
  let plan, batches =
    Chat_tui.Renderer_page_chat.destination_target_width_batches ~model ()
    |> Option.value_exn
  in
  assert (plan.visible_rows.first = 0);
  assert (Chat_tui.History_chunk.Range.length plan.scheduled_rows < List.length messages);
  let jobs =
    List.concat_map batches ~f:(fun (_, jobs) ->
      List.map jobs ~f:(fun ranked -> ranked.Chat_tui.Chat_render_worker.job))
  in
  List.iter jobs ~f:(fun job ->
    assert (
      Chat_tui.Model.commit_width_preparation_result
        model
        ~theme_generation:0
        ~grammar_generation:(Chat_tui.Highlight_registry.generation ())
        (render_preparation_job job)));
  assert (Chat_tui.Model.publish_width_preparation_corridor model ~request_generation:23);
  [%test_eq: int] (Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model)) 0;
  start_width_preparation
    model
    ~request_generation:24
    ~width:24
    ~theme_generation:0
    ~grammar_generation:(Chat_tui.Highlight_registry.generation ());
  assert (
    Chat_tui.Model.set_width_preparation_destination
      model
      ~request_generation:24
      (Some
         { Chat_tui.Model.Chat_page_state.Destination.id
         ; revision = revision + 1
         ; reason = Search_result
         ; placement = Center
         }));
  let preparation = Chat_tui.Model.width_preparation model |> Option.value_exn in
  assert (not (Chat_tui.Model.width_preparation_destination_is_current model preparation));
  assert (
    not (Chat_tui.Model.publish_width_preparation_corridor model ~request_generation:24))
;;

let%test_unit "complete target width promotes to exact Warm history" =
  let messages =
    List.init 33 ~f:(fun index -> "assistant", sprintf "row %d with wrapping text" index)
  in
  let model = make_model messages in
  ignore
    (Chat_tui.Renderer_page_chat.render ~size:(40, 24) ~model : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(40, 24) ~model;
  start_width_preparation
    model
    ~request_generation:31
    ~width:24
    ~theme_generation:0
    ~grammar_generation:(Chat_tui.Highlight_registry.generation ());
  let batches =
    Chat_tui.Renderer_page_chat.remaining_target_width_batches ~model ()
    |> Option.value_exn
  in
  let jobs =
    List.concat_map batches ~f:(fun (_, jobs) ->
      List.map jobs ~f:(fun ranked -> ranked.Chat_tui.Chat_render_worker.job))
  in
  List.iter jobs ~f:(fun job ->
    assert (
      Chat_tui.Model.commit_width_preparation_result
        model
        ~theme_generation:0
        ~grammar_generation:(Chat_tui.Highlight_registry.generation ())
        (render_preparation_job job)));
  let preparation = Chat_tui.Model.width_preparation model |> Option.value_exn in
  [%test_eq: int]
    (Chat_tui.Model.width_preparation_exact_row_count model preparation)
    (List.length messages);
  assert (Chat_tui.Model.width_preparation_is_exact model preparation);
  assert (
    Chat_tui.Renderer_page_chat.promote_width_preparation
      ~size:(24, 24)
      ~model
      ~request_generation:31);
  assert (
    Poly.equal
      (Chat_tui.Model.chat_materialization model)
      Chat_tui.Model.Chat_page_state.Warm);
  assert (
    Chat_tui.Renderer_virtual_list.Geometry.all_exact
      (Chat_tui.Model.chat_render_geometry model));
  [%test_eq: int option] (Chat_tui.Model.active_history_width model) (Some 24);
  assert (Option.is_none (Chat_tui.Model.width_preparation model));
  assert (Option.is_none (Chat_tui.Model.corridor_history_cache model));
  assert (Option.is_some (Chat_tui.Model.history_image_cache model));
  let restored = Chat_tui.Model.restore_width model ~width:24 in
  assert restored;
  let remaining = Chat_tui.Renderer_page_chat.remaining_target_width_batches ~model () in
  assert (Option.is_none remaining);
  let synchronous = make_model messages in
  ignore
    (Chat_tui.Renderer_page_chat.render ~size:(24, 24) ~model:synchronous
     : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(24, 24) ~model:synchronous;
  let progressive_cache = Chat_tui.Model.history_image_cache model |> Option.value_exn in
  let synchronous_cache =
    Chat_tui.Model.history_image_cache synchronous |> Option.value_exn
  in
  [%test_eq: string]
    (image_to_string progressive_cache.image)
    (image_to_string synchronous_cache.image);
  [%test_eq: int array]
    (Chat_tui.Renderer_virtual_list.Geometry.heights
       (Chat_tui.Model.chat_render_geometry model))
    (Chat_tui.Renderer_virtual_list.Geometry.heights
       (Chat_tui.Model.chat_render_geometry synchronous));
  [%test_eq: int array]
    (Chat_tui.Renderer_virtual_list.Geometry.prefix
       (Chat_tui.Model.chat_render_geometry model))
    (Chat_tui.Renderer_virtual_list.Geometry.prefix
       (Chat_tui.Model.chat_render_geometry synchronous))
;;

let%test_unit "model render identities track structural, text, and metadata changes" =
  let model = make_model [ "tool", "call({})" ] in
  let generation = Chat_tui.Model.transcript_generation model in
  [%test_eq: int] (revision_exn model 0) 0;
  ignore
    (Chat_tui.Model.apply_patch
       model
       (Append_text { id = "assistant"; role = "assistant"; text = "delta" })
     : Chat_tui.Model.t);
  assert (Chat_tui.Model.transcript_generation model > generation);
  let assistant_index = Array.length (Chat_tui.Model.render_messages model) - 1 in
  [%test_eq: int] (revision_exn model assistant_index) 1;
  let output_index = Array.length (Chat_tui.Model.render_messages model) in
  ignore
    (Chat_tui.Model.apply_patch
       model
       (Set_function_name { id = "output"; name = "read_file" })
     : Chat_tui.Model.t);
  ignore
    (Chat_tui.Model.apply_patch
       model
       (Set_function_output { id = "output"; output = "let x = 1" })
     : Chat_tui.Model.t);
  [%test_eq: int] (revision_exn model output_index) 2;
  Chat_tui.Model.set_messages model [ "user", "replacement" ];
  [%test_eq: int] (revision_exn model 0) 0;
  assert (Chat_tui.Model.transcript_generation model > generation)
;;

let%test_unit "render commits reuse stale-geometry images and one invariant height" =
  let model = make_model [ "assistant", "text" ] in
  Chat_tui.Model.set_active_history_width model (Some 40);
  let geometry = Chat_tui.Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.initialize_estimated
    geometry
    ~length:1
    ~estimated_height_at_index:(fun _ -> 5);
  let generation = Chat_tui.Renderer_virtual_list.Geometry.generation geometry in
  let make geometry_generation =
    let row_id, row_revision =
      Chat_tui.Model.render_row_identity model ~idx:0 |> Option.value_exn
    in
    Job.create
      ~transcript_generation:(Chat_tui.Model.transcript_generation model)
      ~row_id
      ~row_revision
      ~message_index:0
      ~message_revision:(revision_exn model 0)
      ~width:40
      ~role:"assistant"
      ~text:"text"
      ~tool_output:None
      ~tool_call_outcome:None
      ~theme_generation:0
      ~grammar_generation:0
      ~geometry_generation
      ~request_generation:0
      ~render_generation:(Chat_tui.Model.render_generation model)
      ~submission_generation:0
      ~semantic_seed:None
      ~priority:Visible
  in
  let stale = Job.result (make (generation - 1)) ~image:(Notty.I.void 40 9) in
  assert (Chat_tui.Model.commit_render_result model stale);
  [%test_eq: int] (Chat_tui.Model.msg_heights model).(0) 5;
  assert (not (List.is_empty (Chat_tui.Model.take_and_clear_dirty_height_rows model)));
  Chat_tui.Model.select_message model (Some 0);
  let current = Job.result (make generation) ~image:(Notty.I.void 40 9) in
  assert (Chat_tui.Model.commit_render_result model current);
  [%test_eq: int] (Chat_tui.Model.msg_heights model).(0) 9
;;

let%test_unit "render commit preserves a manual viewport anchor" =
  let model =
    make_model [ "assistant", "above"; "assistant", "anchor"; "assistant", "below" ]
  in
  Chat_tui.Model.set_active_history_width model (Some 40);
  let geometry = Chat_tui.Model.chat_render_geometry model in
  Chat_tui.Renderer_virtual_list.Geometry.initialize_estimated
    geometry
    ~length:3
    ~estimated_height_at_index:(fun _ -> 5);
  let generation = Chat_tui.Renderer_virtual_list.Geometry.generation geometry in
  Chat_tui.Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Chat_tui.Model.scroll_box model) 5;
  let job =
    let row_id, row_revision =
      Chat_tui.Model.render_row_identity model ~idx:0 |> Option.value_exn
    in
    Job.create
      ~transcript_generation:(Chat_tui.Model.transcript_generation model)
      ~row_id
      ~row_revision
      ~message_index:0
      ~message_revision:(revision_exn model 0)
      ~width:40
      ~role:"assistant"
      ~text:"above"
      ~tool_output:None
      ~tool_call_outcome:None
      ~theme_generation:0
      ~grammar_generation:0
      ~geometry_generation:generation
      ~request_generation:0
      ~render_generation:(Chat_tui.Model.render_generation model)
      ~submission_generation:0
      ~semantic_seed:None
      ~priority:Background
  in
  let result = Job.result job ~image:(Notty.I.void 40 9) in
  assert (Chat_tui.Model.commit_render_result model result);
  [%test_eq: int] (Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model)) 9;
  let viewport =
    Chat_tui.Renderer_virtual_list.Viewport.compute
      ~geometry
      ~requested_scroll:(Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model))
      ~height:5
      ~follow_bottom:false
  in
  [%test_eq: int option]
    (Chat_tui.Renderer_virtual_list.Viewport.first_visible viewport)
    (Some 1)
;;

let%test_unit "code cache isolates grammar generations" =
  let cache = Job.Code_cache.create ~capacity:4 in
  let lines = [ [ Notty.A.empty, "cached" ] ] in
  Job.Code_cache.set
    cache
    ~role_class:Userlike
    ~grammar_generation:1
    ~lang:(Some "ocaml")
    ~code:"let x = 1"
    ~width:40
    lines;
  assert (
    Option.is_some
      (Job.Code_cache.find
         cache
         ~role_class:Userlike
         ~grammar_generation:1
         ~lang:(Some "ocaml")
         ~code:"let x = 1"
         ~width:40));
  assert (
    Option.is_none
      (Job.Code_cache.find
         cache
         ~role_class:Userlike
         ~grammar_generation:2
         ~lang:(Some "ocaml")
         ~code:"let x = 1"
         ~width:40))
;;

let%test_unit "highlight cache is width-independent and isolates generations" =
  let cache = Job.Highlight_cache.create ~capacity:4 in
  let lines = [ [ Notty.A.empty, "let x = 1" ] ] in
  Job.Highlight_cache.set_plain
    cache
    ~theme_generation:1
    ~grammar_generation:2
    ~lang:(Some "ocaml")
    ~text:"let x = 1"
    lines;
  [%test_eq: int] (Job.Highlight_cache.length cache) 1;
  assert (
    Option.is_some
      (Job.Highlight_cache.find_plain
         cache
         ~theme_generation:1
         ~grammar_generation:2
         ~lang:(Some "ocaml")
         ~text:"let x = 1"));
  assert (
    Option.is_none
      (Job.Highlight_cache.find_plain
         cache
         ~theme_generation:2
         ~grammar_generation:2
         ~lang:(Some "ocaml")
         ~text:"let x = 1"));
  assert (
    Option.is_none
      (Job.Highlight_cache.find_plain
         cache
         ~theme_generation:1
         ~grammar_generation:3
         ~lang:(Some "ocaml")
         ~text:"let x = 1"))
;;

let%test_unit "auto-follow retargets a manual resize corridor to streamed tail output" =
  let messages =
    List.init 100 ~f:(fun index -> "assistant", sprintf "row %d with wrapping text" index)
  in
  let model = make_model messages in
  ignore
    (Chat_tui.Renderer_page_chat.render ~size:(40, 24) ~model : Notty.I.t * (int * int));
  Chat_tui.Renderer_page_chat.warm_history_synchronously ~size:(40, 24) ~model;
  Chat_tui.Model.set_auto_follow model false;
  Notty_scroll_box.scroll_to (Chat_tui.Model.scroll_box model) 120;
  start_width_preparation
    model
    ~request_generation:41
    ~width:24
    ~theme_generation:0
    ~grammar_generation:(Chat_tui.Highlight_registry.generation ());
  let _, initial_batches =
    Chat_tui.Renderer_page_chat.initial_target_width_batches ~model () |> Option.value_exn
  in
  Chat_tui.Model.set_chat_materialization_resizing model;
  List.concat_map initial_batches ~f:(fun (_, jobs) ->
    List.map jobs ~f:(fun ranked -> ranked.Chat_tui.Chat_render_worker.job))
  |> List.iter ~f:(fun job ->
    assert (
      Chat_tui.Model.commit_width_preparation_result
        model
        ~theme_generation:0
        ~grammar_generation:(Chat_tui.Highlight_registry.generation ())
        (render_preparation_job job)));
  assert (Chat_tui.Model.publish_width_preparation_corridor model ~request_generation:41);
  Chat_tui.Model.set_auto_follow model true;
  ignore
    (Chat_tui.Model.apply_patch
       model
       (Append_text
          { id = "streamed-tail"
          ; role = "assistant"
          ; text = "VISIBLE WITHOUT ANOTHER RESIZE"
          })
     : Chat_tui.Model.t);
  let _, tail_batches =
    Chat_tui.Renderer_page_chat.current_target_width_batches ~model () |> Option.value_exn
  in
  List.concat_map tail_batches ~f:(fun (_, jobs) ->
    List.map jobs ~f:(fun ranked -> ranked.Chat_tui.Chat_render_worker.job))
  |> List.iter ~f:(fun job ->
    assert (
      Chat_tui.Model.commit_width_preparation_result
        model
        ~theme_generation:0
        ~grammar_generation:(Chat_tui.Highlight_registry.generation ())
        (render_preparation_job job)));
  assert (Chat_tui.Model.publish_width_preparation_corridor model ~request_generation:41);
  let row_count = Array.length (Chat_tui.Model.render_messages model) in
  let cache = Chat_tui.Model.corridor_history_cache model |> Option.value_exn in
  assert (cache.rows.past = row_count);
  assert (String.is_substring (image_to_string cache.image) ~substring:"VISIBLE");
  assert (Chat_tui.Model.auto_follow model);
  [%test_eq: int]
    (Notty_scroll_box.scroll (Chat_tui.Model.scroll_box model))
    (Chat_tui.Model.chat_max_scroll model ~viewport_height:20);
  let latest_role, latest_text = (Chat_tui.Model.render_messages model).(row_count - 1) in
  [%test_eq: string] latest_role "assistant";
  [%test_eq: string] latest_text "VISIBLE WITHOUT ANOTHER RESIZE"
;;
