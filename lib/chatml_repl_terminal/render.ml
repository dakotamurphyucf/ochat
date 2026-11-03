open! Core
module W = Agent_protocol.Developer_repl_wire
module H = Terminal_highlighting

type t =
  { max_display_bytes : int
  ; highlighter : H.Highlight_tm_engine.t
  }

let create ~max_display_bytes =
  if max_display_bytes < 256 || max_display_bytes > 64 * 1024
  then Error (Agent_protocol.Error.invalid_request "invalid terminal display bound")
  else (
    let registry = H.Highlight_tm_loader.create_registry () in
    match H.Highlight_grammars.add_chatml registry with
    | Error error ->
      Error
        (Agent_protocol.Error.invalid_request
           ("cannot load ChatML terminal grammar: " ^ Error.to_string_hum error))
    | Ok () ->
      let highlighter =
        H.Highlight_tm_engine.create ~theme:H.Highlight_theme.github_dark
        |> H.Highlight_tm_engine.with_registry ~registry
      in
      Ok { max_display_bytes; highlighter })
;;

let safe_text ?max_bytes t text =
  let max_bytes =
    Option.value_map
      max_bytes
      ~default:t.max_display_bytes
      ~f:(Int.min t.max_display_bytes)
  in
  let buffer = Stdlib.Buffer.create (Int.min max_bytes 1_024) in
  let truncated = ref false in
  let add fragment =
    if not !truncated
    then
      if String.length fragment <= max_bytes - Stdlib.Buffer.length buffer
      then Stdlib.Buffer.add_string buffer fragment
      else truncated := true
  in
  Uutf.String.fold_utf_8
    (fun () _ -> function
       | `Malformed _ -> add "�"
       | `Uchar uchar ->
         let code = Stdlib.Uchar.to_int uchar in
         if code = 10
         then add "\n"
         else if code = 9
         then add "    "
         else if code < 32 || code = 127 || (code >= 128 && code <= 159)
         then add (Printf.sprintf "\\u{%04x}" code)
         else (
           let encoded = Stdlib.Buffer.create 4 in
           Stdlib.Buffer.add_utf_8_uchar encoded uchar;
           add (Stdlib.Buffer.contents encoded)))
    ()
    text;
  let result = Stdlib.Buffer.contents buffer in
  if !truncated && String.length result <= max_bytes - 3 then result ^ "..." else result
;;

let text_row attr text = Notty.I.string attr text

let status_text = function
  | State.Ready -> "ready"
  | Busy operation -> "busy " ^ Sexp.to_string ([%sexp_of: State.operation] operation)
  | Uncertain operation ->
    "uncertain " ^ Sexp.to_string ([%sexp_of: State.keyed_operation] operation)
  | Needs_reset -> "revision unknown: reset or create"
  | Closed -> "closed"
;;

let task_reference_line index reference =
  Printf.sprintf "Task %d: :run %d to execute  %s" (index + 1) (index + 1) reference
;;

let rendered_type_text rendered =
  let text = W.Submission.Rendered_type.text rendered in
  match W.Submission.Rendered_type.completeness rendered with
  | Complete -> text
  | Truncated _ -> text ^ " [type truncated]"
;;

let binding_line inspection =
  Option.map (W.Type_inspection.binding_name inspection) ~f:(fun name ->
    "val " ^ name ^ " : " ^ rendered_type_text (W.Type_inspection.rendered inspection))
;;

let submission_lines t state submission =
  let status =
    match W.Submission.status submission with
    | Need_more _ -> "need more input"
    | Rejected diagnostic -> "rejected: " ^ W.Submission.Diagnostic.message diagnostic
    | Finished -> "finished"
    | Stopped diagnostic ->
      "stopped"
      ^ Option.value_map diagnostic ~default:"" ~f:(fun diagnostic ->
        ": " ^ W.Submission.Diagnostic.message diagnostic)
  in
  let inferred = State.inferred_bindings state in
  let summaries =
    W.Submission.summaries submission
    |> List.concat_mapi ~f:(fun index summary ->
      match W.Submission.Summary.outcome summary with
      | Failed _ ->
        [ Option.value_map
            (W.Submission.Summary.diagnostic summary)
            ~default:(Printf.sprintf "%d. failed" (index + 1))
            ~f:(fun diagnostic -> "Error: " ^ W.Submission.Diagnostic.message diagnostic)
        ]
      | Succeeded ->
        (match W.Submission.Summary.expression_type summary with
         | Some rendered ->
           [ "- : "
             ^ rendered_type_text rendered
             ^ Option.value_map
                 (W.Submission.Summary.value_preview summary)
                 ~default:""
                 ~f:(fun value -> " = " ^ value)
           ]
         | None ->
           let bindings =
             State.Inferred_bindings.entries inferred
             |> List.filter_map ~f:(fun entry ->
               if entry.statement_index = index
               then binding_line entry.inspection
               else None)
           in
           if not (List.is_empty bindings)
           then bindings
           else if W.Submission.Summary.binding_count summary > 0
           then [ Printf.sprintf "%d. binding accepted" (index + 1) ]
           else
             [ Printf.sprintf
                 "%d. ok%s"
                 (index + 1)
                 (Option.value_map
                    (W.Submission.Summary.value_preview summary)
                    ~default:""
                    ~f:(fun value -> " " ^ value))
             ]))
  in
  let inspection_notice =
    (if State.Inferred_bindings.limited inferred
     then [ "Binding type display limited; use :type to inspect more" ]
     else [])
    @ Option.to_list
        (Option.map (State.Inferred_bindings.error inferred) ~f:(fun error ->
           "Binding type unavailable: " ^ error.message))
  in
  let references =
    W.Submission.task_references submission |> List.mapi ~f:task_reference_line
  in
  List.map ((status :: summaries) @ inspection_notice @ references) ~f:(safe_text t)
;;

let task_lines t task =
  let phase =
    match W.Task_view.phase task with
    | Reserved -> "Task reserved"
    | Waiting_permission -> "Task waiting permission"
    | Execution_started -> "Task running"
    | Terminal { outcome; _ } ->
      (match outcome with
       | Succeeded preview -> "Task succeeded " ^ Option.value preview ~default:""
       | Failed { error; _ } -> "Task failed: " ^ error.message
       | Limited { error; _ } -> "Task limited: " ^ error.message
       | Interrupted -> "Task interrupted"
       | Cancelled -> "Task cancelled")
  in
  [ safe_text t phase ]
;;

let row_of_spans spans =
  List.fold spans ~init:Notty.I.empty ~f:(fun image (attr, text) ->
    Notty.I.(image <|> Notty.I.string attr text))
;;

let stack rows =
  List.fold rows ~init:Notty.I.empty ~f:(fun image row -> Notty.I.(image <-> row))
;;

let image t state ~width ~height =
  if width <= 0 || height <= 0
  then Notty.I.empty, None
  else (
    let width = Int.clamp_exn width ~min:1 ~max:512 in
    let height = Int.clamp_exn height ~min:1 ~max:64 in
    let row_budget = Int.max 32 (t.max_display_bytes / height) in
    let clean text = safe_text ~max_bytes:row_budget t text in
    let editor = State.editor state in
    let accepted_prefix = Option.value (State.continuation_source state) ~default:"" in
    let source = accepted_prefix ^ State.Editor.source editor in
    let cursor_byte = String.length accepted_prefix + State.Editor.cursor_byte editor in
    let header = clean ("[" ^ status_text (State.activity state) ^ "]") in
    let context_rows =
      State.context state
      |> String.split ~on:'\n'
      |> fun rows ->
      List.take rows 3
      |> List.map ~f:(fun row ->
        text_row (H.Highlight_styles.fg_hex "#768390") (clean row))
    in
    let assistance_rows =
      match State.history_search state, State.completion state with
      | Some search, _ ->
        let query = History_search.query search |> clean in
        let preview =
          Option.value_map
            (History_search.matched_source search)
            ~default:"[no match]"
            ~f:(fun source ->
              String.split source ~on:'\n' |> List.hd |> Option.value ~default:"" |> clean)
        in
        [ text_row
            H.Highlight_styles.(fg_hex "#eac55f")
            (clean ("reverse-search " ^ query ^ ": " ^ preview))
        ]
      | None, Some completion ->
        let selected = Completion.selected_index completion in
        let first = Int.max 0 (selected - 2) in
        let items =
          Completion.items completion
          |> fun items -> List.drop items first |> fun items -> List.take items 5
        in
        let session, local =
          List.fold items ~init:(false, false) ~f:(fun (session, local) item ->
            match Completion.Item.origin item with
            | Session_binding -> true, local
            | Command | Keyword | Accepted_binding -> session, true)
        in
        let origin =
          match session, local with
          | true, true -> "session + local: "
          | true, false -> "session: "
          | false, (true | false) -> "local: "
        in
        let spans =
          (H.Highlight_styles.fg_hex "#768390", origin)
          :: List.mapi items ~f:(fun index item ->
            let attr =
              if index + first = selected
              then H.Highlight_styles.(fg_hex "#22272e" ++ bg_hex "#96d0ff" ++ bold)
              else H.Highlight_styles.fg_hex "#adbac7"
            in
            attr, " " ^ clean (Completion.Item.label item) ^ " ")
        in
        [ row_of_spans spans ]
      | None, None -> []
    in
    let assistance_rows =
      assistance_rows
      @
      if Option.is_some (State.history_search state)
      then []
      else
        Option.value_map (State.completion_notice state) ~default:[] ~f:(fun notice ->
          [ text_row (H.Highlight_styles.fg_hex "#eac55f") (clean notice) ])
    in
    let assistance_room = List.length assistance_rows in
    let notice =
      Option.to_list (State.notice state)
      |> List.concat_map ~f:(fun text -> String.split text ~on:'\n')
      |> fun lines -> List.take lines 16 |> List.map ~f:clean
    in
    let submission =
      Option.value_map (State.submission state) ~default:[] ~f:(submission_lines t state)
    in
    let task = Option.value_map (State.task state) ~default:[] ~f:(task_lines t) in
    let page =
      Option.value_map (State.reference_page state) ~default:[] ~f:(fun page ->
        W.Reference_page.items page
        |> List.mapi ~f:(fun index reference ->
          safe_text t (task_reference_line index reference)))
    in
    let output_room =
      Int.max 0 (height - 3 - List.length context_rows - assistance_room)
    in
    let ordinary_output =
      submission @ task @ page @ notice
      |> List.concat_map ~f:(fun row -> String.split row ~on:'\n')
      |> List.map ~f:(fun row ->
        let attr =
          if
            String.is_prefix row ~prefix:"Error: "
            || String.is_prefix row ~prefix:"rejected: "
            || String.is_prefix row ~prefix:"stopped: "
          then H.Highlight_styles.fg_hex "#f47067"
          else H.Highlight_styles.fg_hex "#adbac7"
        in
        text_row attr (clean row))
    in
    let diagnostic_rows =
      Option.value_map (State.submission state) ~default:[] ~f:(fun submission ->
        let diagnostics =
          match W.Submission.status submission with
          | Rejected diagnostic | Stopped (Some diagnostic) -> [ diagnostic ]
          | Need_more _ | Finished | Stopped None ->
            W.Submission.summaries submission
            |> List.filter_map ~f:W.Submission.Summary.diagnostic
        in
        let submitted_source =
          Option.both (State.submitted_source state) (State.submitted_source_digest state)
        in
        diagnostics
        |> fun diagnostics ->
        List.take diagnostics 4
        |> List.concat_map ~f:(fun diagnostic ->
          let error_attr = H.Highlight_styles.fg_hex "#f47067" in
          match
            Diagnostic_excerpt.create
              diagnostic
              ~submitted_source
              ~max_line_bytes:row_budget
          with
          | None -> []
          | Some (Location_only location) -> [ text_row error_attr (clean location) ]
          | Some (Excerpt { location; prefix; before; marked; after; marker }) ->
            [ text_row (H.Highlight_styles.fg_hex "#768390") (clean location)
            ; row_of_spans
                [ H.Highlight_styles.fg_hex "#768390", prefix
                ; H.Highlight_styles.fg_hex "#adbac7", before
                ; ( H.Highlight_styles.(fg_hex "#f47067" ++ bg_hex "#51202a" ++ underline)
                  , marked )
                ; H.Highlight_styles.fg_hex "#adbac7", after
                ]
            ; text_row error_attr marker
            ]))
    in
    let output_rows =
      ordinary_output @ diagnostic_rows
      |> List.rev
      |> fun rows -> List.take rows output_room |> List.rev
    in
    let lines = String.split source ~on:'\n' in
    let before_cursor = String.prefix source cursor_byte in
    let cursor_line = String.count before_cursor ~f:(Char.equal '\n') in
    let cursor_in_line =
      match String.rfindi before_cursor ~f:(fun _ char -> Char.equal char '\n') with
      | None -> String.length before_cursor
      | Some index -> String.length before_cursor - index - 1
    in
    let editor_room =
      Int.max
        1
        (height - List.length context_rows - List.length output_rows - assistance_room - 2)
    in
    let first_line = Int.max 0 (cursor_line - editor_room + 1) in
    let visible_lines =
      lines
      |> fun lines ->
      List.drop lines first_line |> fun lines -> List.take lines editor_room
    in
    let line_chunk line ~cursor =
      let raw_budget = Int.max 1 (row_budget / 8) in
      let boundaries =
        Uuseg_string.fold_utf_8
          `Grapheme_cluster
          (fun (offset, boundaries) cluster ->
             let offset = offset + String.length cluster in
             offset, offset :: boundaries)
          (0, [ 0 ])
          line
        |> snd
        |> List.rev
      in
      let floor pos =
        boundaries
        |> List.take_while ~f:(fun offset -> offset <= pos)
        |> List.last
        |> Option.value ~default:0
      in
      let first = floor (Int.max 0 (cursor - (raw_budget / 2))) in
      let last = floor (Int.min (String.length line) (first + raw_budget)) in
      String.sub line ~pos:first ~len:(last - first), first
    in
    (* Tokenize the complete lexical context before clipping. Above the bounded
       highlighting budget, plain spans preserve text without guessing the
       lexical state of an omitted prefix. Escaping happens after span slicing. *)
    let highlighted_lines =
      if String.length source <= t.max_display_bytes
      then
        H.Highlight_tm_engine.highlight_text
          t.highlighter
          ~lang:(Some "chatml")
          ~text:source
      else []
    in
    let spans_in_chunk spans ~first ~last =
      let _, spans =
        List.fold spans ~init:(0, []) ~f:(fun (offset, acc) (attr, text) ->
          let next = offset + String.length text in
          let start = Int.max first offset in
          let stop = Int.min last next in
          let acc =
            if start < stop
            then
              (attr, clean (String.sub text ~pos:(start - offset) ~len:(stop - start)))
              :: acc
            else acc
          in
          next, acc)
      in
      List.rev spans
    in
    let prompt_width = if width >= 3 then 2 else 0 in
    let source_width = width - prompt_width in
    let editor_rows =
      visible_lines
      |> List.mapi ~f:(fun index line ->
        let line_index = index + first_line in
        let cursor = if line_index = cursor_line then cursor_in_line else 0 in
        let chunk, first = line_chunk line ~cursor in
        let spans =
          match List.nth highlighted_lines line_index with
          | None -> [ Notty.A.empty, clean chunk ]
          | Some spans -> spans_in_chunk spans ~first ~last:(first + String.length chunk)
        in
        let prompt =
          if prompt_width = 0 then "" else if line_index = 0 then "# " else "  "
        in
        prompt, row_of_spans spans)
    in
    let cursor, horizontal =
      match List.nth lines cursor_line with
      | None -> None, 0
      | Some line ->
        let chunk, first = line_chunk line ~cursor:cursor_in_line in
        let prefix = String.prefix chunk (Int.max 0 (cursor_in_line - first)) |> clean in
        let x = Notty.I.width (Notty.I.string Notty.A.empty prefix) in
        let y =
          2
          + List.length context_rows
          + List.length output_rows
          + cursor_line
          - first_line
        in
        let horizontal = Int.max 0 (x - source_width + 1) in
        ( (if y >= height then None else Some (prompt_width + x - horizontal, y))
        , horizontal )
    in
    let editor_rows =
      List.map editor_rows ~f:(fun (prompt, image) ->
        let source =
          Notty.I.hcrop
            horizontal
            (Int.max 0 (Notty.I.width image - horizontal - source_width))
            image
        in
        Notty.I.(text_row H.Highlight_styles.(fg_hex "#96d0ff" ++ bold) prompt <|> source))
    in
    let rows =
      [ text_row H.Highlight_styles.(fg_hex "#adbac7" ++ bg_hex "#22272e") header ]
      @ context_rows
      @ output_rows
      @ [ text_row
            (H.Highlight_styles.fg_hex "#768390")
            (if Option.is_some (State.continuation_source state)
             then
               "-- Accepted prefix is read-only; Enter=append suffix; :reset=discard --"
             else
               "-- Enter=submit  Alt-Enter=newline  Tab=complete  Ctrl-R=search  :help --")
        ]
      @ editor_rows
      @ assistance_rows
    in
    let image = stack rows in
    let image = Notty.I.hcrop 0 (Int.max 0 (Notty.I.width image - width)) image in
    let image = Notty.I.vcrop 0 (Int.max 0 (Notty.I.height image - height)) image in
    image, if Option.is_some (State.history_search state) then None else cursor)
;;
