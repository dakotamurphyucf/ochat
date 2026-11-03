open! Core
module State = Chatml_repl_terminal.State
module Render = Chatml_repl_terminal.Render
module P = Agent_protocol
module W = P.Developer_repl_wire
module H = Terminal_highlighting

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let state ?(source = "") () =
  let limits =
    State.Limits.create
      ~max_source_bytes:131_072
      ~max_source_lines:1_024
      ~max_history_entries:16
      ~max_history_bytes:131_072
      ~max_notice_bytes:1_024
    |> ok
  in
  let state = State.create ~limits ~context:"captured project" |> ok in
  let editor = State.Editor.insert (State.editor state) ~limits source |> ok in
  State.with_editor state editor |> ok
;;

let render ?(max_display_bytes = 8_192) ?(width = 80) ?(height = 12) state =
  let renderer = Render.create ~max_display_bytes |> ok in
  Render.image renderer state ~width ~height
;;

let dump ?(cap = Notty.Cap.dumb) image ~width ~height =
  let buffer = Buffer.create 256 in
  Notty.Render.to_buffer buffer cap (0, 0) (width, height) image;
  Buffer.contents buffer
;;

let source_row image ~row ~text =
  let image =
    Notty.I.vcrop row (Int.max 0 (Notty.I.height image - row - 1)) image
    |> Notty.I.hcrop 2 0
  in
  dump
    ~cap:Notty.Cap.ansi
    image
    ~width:(Notty.I.width (Notty.I.string Notty.A.empty text))
    ~height:1
;;

let has_style image ~row ~text attr =
  let expected =
    dump
      ~cap:Notty.Cap.ansi
      (Notty.I.string attr text)
      ~width:(Notty.I.width (Notty.I.string Notty.A.empty text))
      ~height:1
  in
  String.equal (source_row image ~row ~text) expected
;;

let%expect_test "multiline highlighting keeps nested-comment and string context" =
  let text = "let x = 1" in
  let check source scope =
    let image, _ = render (state ~source ()) in
    has_style
      image
      ~row:5
      ~text
      (H.Highlight_theme.attr_of_scopes H.Highlight_theme.github_dark ~scopes:[ scope ])
  in
  print_s
    [%sexp
      { nested_comment =
          (check ("(* outer\n(* inner\n" ^ text) "comment.block.chatml" : bool)
      ; multiline_string =
          (check ("\"first\nsecond\n" ^ text) "string.quoted.double.chatml" : bool)
      }];
  [%expect {| ((nested_comment true) (multiline_string true)) |}]
;;

let%expect_test "large drafts fall back to plain spans while retaining the prompt cursor" =
  let text = "let x = 1" in
  let image, cursor =
    render
      ~max_display_bytes:2_048
      (state ~source:(String.make 4_096 'x' ^ "\n" ^ text) ())
  in
  print_s
    [%sexp
      { plain = (has_style image ~row:4 ~text Notty.A.empty : bool)
      ; cursor : (int * int) option
      }];
  [%expect {| ((plain true) (cursor ((11 4)))) |}]
;;

let%expect_test "prompt cursor counts Unicode columns and escaped source controls" =
  let unicode = state ~source:"界é\t" () in
  let _, cursor = render unicode in
  let _, narrow = render ~width:6 unicode in
  let escaped = state ~source:"\027" () in
  let image, escaped_cursor = render escaped in
  let output = dump image ~width:80 ~height:12 in
  print_s
    [%sexp
      { unicode_cursor = (cursor : (int * int) option)
      ; narrow_cursor = (narrow : (int * int) option)
      ; escaped_cursor : (int * int) option
      ; escaped_source = (String.is_substring output ~substring:"# \\u{001b}" : bool)
      ; literal_escape = (String.contains output '\027' : bool)
      }];
  [%expect
    {|
    ((unicode_cursor ((9 3))) (narrow_cursor ((5 3))) (escaped_cursor ((10 3)))
     (escaped_source true) (literal_escape false))
    |}]
;;

let fixture () =
  let document =
    Document_schema.Limits.create
      ~max_bytes:16_384
      ~max_depth:32
      ~max_fields:1_024
      ~max_nodes:2_048
    |> function
    | Ok value -> value
    | Error _ -> failwith "invalid fixture document limits"
  in
  let limits =
    W.Limits.create
      ~request:document
      ~result:document
      ~page:document
      ~response:document
      ~max_source_bytes:512
      ~max_reference_bytes:384
      ~max_output_bytes:512
      ~max_page_items:2
      ~max_event_bytes:256
      ~max_registry_entries:16
    |> ok
  in
  let epoch = W.Wire_id.Epoch.of_string "re_render_types" |> ok in
  let revision = W.Wire_id.Revision.of_string "rv_render_types" |> ok in
  let generator = P.Id.Generator.create ~bytes:(fun count -> String.make count 't') in
  let view =
    W.View.create
      ~limits
      ~instance:(W.Wire_id.Instance.of_string "ri_render_types" |> ok)
      ~epoch
      ~revision
      ~run_id:(P.Id.Developer_run.create_with generator)
      ~capture_fingerprint:(Chatml_source.Digest.of_contents "types" ~checkpoint:Fn.id)
    |> ok
  in
  limits, epoch, revision, State.with_view (state ()) view
;;

let%expect_test
    "expression types pair with their own values and bindings never inherit unit"
  =
  let limits, epoch, revision, state = fixture () in
  let summary ~expression_type ~binding_count ~preview =
    W.Submission.Summary.create
      ~limits
      ~outcome:Succeeded
      ~value_preview:(Some preview)
      ~diagnostic:None
      ~expression_type
      ~binding_count
      ~task_retention:Not_a_task
      ~text_limited:false
    |> ok
  in
  let expression_type =
    W.Submission.Rendered_type.create
      ~limits
      ~kind:Type
      ~completeness:Complete
      ~text:"int"
    |> ok
  in
  let submission =
    W.Submission.create
      ~limits
      ~status:Finished
      ~details:Available
      ~completed_statements:2
      ~statement_count:2
      ~epoch
      ~revision
      ~summaries:
        [ summary ~expression_type:(Some expression_type) ~binding_count:0 ~preview:"42"
        ; summary ~expression_type:None ~binding_count:1 ~preview:"()"
        ]
      ~task_reference_count:0
      ~task_references:[]
    |> ok
  in
  let state = State.with_submission state submission |> ok in
  let image, _ = render state in
  let output = dump image ~width:80 ~height:12 in
  print_s
    [%sexp
      { typed_expression = (String.is_substring output ~substring:"- : int = 42" : bool)
      ; binding_accepted =
          (String.is_substring output ~substring:"2. binding accepted" : bool)
      ; fabricated_binding_value = (String.is_substring output ~substring:"()" : bool)
      ; finished = (String.is_substring output ~substring:"finished" : bool)
      }];
  [%expect
    {|
    ((typed_expression true) (binding_accepted true)
     (fabricated_binding_value false) (finished true))
    |}]
;;

let%expect_test
    "completion and reverse-search bars stay bounded and search hides the draft cursor"
  =
  let completion = State.complete (state ~source:"le" ()) in
  let image, completion_cursor = render completion in
  let output = dump image ~width:80 ~height:12 in
  let mixed =
    State.with_session_completion (state ~source:"ma" ()) ~names:[ "max" ] ~notice:None
  in
  let mixed_image, _ = render mixed in
  let mixed_output = dump mixed_image ~width:80 ~height:12 in
  let search =
    state ()
    |> fun state ->
    State.submitted state ~source:"let saved = 42" ~preserve_editor:false
    |> State.start_history_search
    |> fun state ->
    State.edit_history_search state ~f:(fun search ->
      Chatml_repl_terminal.History_search.insert search "saved")
    |> Or_error.ok_exn
  in
  let searched, search_cursor = render search in
  let search_output = dump searched ~width:80 ~height:12 in
  print_s
    [%sexp
      { local_suggestions = (String.is_substring output ~substring:"local:" : bool)
      ; mixed_suggestions =
          (String.is_substring mixed_output ~substring:"session + local:" : bool)
      ; cursor_visible = (Option.is_some completion_cursor : bool)
      ; matched_history =
          (String.is_substring
             search_output
             ~substring:"reverse-search saved: let saved = 42"
           : bool)
      ; search_cursor : (int * int) option
      }];
  [%expect
    {|
    ((local_suggestions true) (mixed_suggestions true) (cursor_visible true)
     (matched_history true) (search_cursor ()))
    |}]
;;

let%expect_test
    "multiline values cannot push trailing Task action hints out of the result view"
  =
  let limits, epoch, revision, state = fixture () in
  let summary =
    W.Submission.Summary.create
      ~limits
      ~outcome:Succeeded
      ~value_preview:
        (Some (String.concat ~sep:"\n" (List.init 30 ~f:(fun _ -> "nested value"))))
      ~diagnostic:None
      ~expression_type:None
      ~binding_count:0
      ~task_retention:Not_a_task
      ~text_limited:false
    |> ok
  in
  let submission =
    W.Submission.create
      ~limits
      ~status:Finished
      ~details:Available
      ~completed_statements:1
      ~statement_count:1
      ~epoch
      ~revision
      ~summaries:[ summary ]
      ~task_reference_count:2
      ~task_references:[ "opaque1"; "opaque2" ]
    |> ok
  in
  let image, _ = render (State.with_submission state submission |> ok) in
  let output = dump image ~width:80 ~height:12 in
  print_s
    [%sexp
      (List.map [ 1; 2 ] ~f:(fun index ->
         String.is_substring
           output
           ~substring:(Printf.sprintf "Task %d: :run %d to execute" index index))
       : bool list)];
  [%expect {| (true true) |}]
;;
