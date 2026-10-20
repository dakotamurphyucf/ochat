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

let submission_lines t submission =
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
  let summaries =
    W.Submission.summaries submission
    |> List.mapi ~f:(fun index summary ->
      let outcome =
        match W.Submission.Summary.outcome summary with
        | Succeeded -> "ok"
        | Failed _ -> "failed"
      in
      let preview =
        Option.value (W.Submission.Summary.value_preview summary) ~default:""
      in
      Printf.sprintf "%d. %s %s" (index + 1) outcome preview)
  in
  let references =
    W.Submission.task_references submission
    |> List.mapi ~f:(fun index reference ->
      Printf.sprintf "Task %d: %s  (:run %d to execute)" (index + 1) reference (index + 1))
  in
  List.map ((status :: summaries) @ references) ~f:(safe_text t)
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
    let source = State.Editor.source editor in
    let header = clean ("[" ^ status_text (State.activity state) ^ "]") in
    let context_rows =
      State.context state
      |> String.split ~on:'\n'
      |> fun rows ->
      List.take rows 3
      |> List.map ~f:(fun row ->
        text_row (H.Highlight_styles.fg_hex "#768390") (clean row))
    in
    let notice =
      Option.to_list (State.notice state)
      |> List.concat_map ~f:(fun text -> String.split text ~on:'\n')
      |> fun lines -> List.take lines 16 |> List.map ~f:clean
    in
    let submission =
      Option.value_map (State.submission state) ~default:[] ~f:(submission_lines t)
    in
    let task = Option.value_map (State.task state) ~default:[] ~f:(task_lines t) in
    let page =
      Option.value_map (State.reference_page state) ~default:[] ~f:(fun page ->
        W.Reference_page.items page
        |> List.mapi ~f:(fun index reference ->
          safe_text
            t
            (Printf.sprintf
               "Task %d: %s  (:run %d to execute)"
               (index + 1)
               reference
               (index + 1))))
    in
    let output_rows =
      submission @ task @ page @ notice
      |> List.rev
      |> fun rows ->
      List.take rows (Int.max 0 (height - 3 - List.length context_rows))
      |> List.rev
      |> List.concat_map ~f:(fun row -> String.split row ~on:'\n')
      |> fun rows ->
      List.take rows (Int.max 0 (height - 3 - List.length context_rows))
      |> List.map ~f:(fun row ->
        text_row (H.Highlight_styles.fg_hex "#adbac7") (clean row))
    in
    let lines = String.split source ~on:'\n' in
    let before_cursor = String.prefix source (State.Editor.cursor_byte editor) in
    let cursor_line = String.count before_cursor ~f:(Char.equal '\n') in
    let cursor_in_line =
      match String.rfindi before_cursor ~f:(fun _ char -> Char.equal char '\n') with
      | None -> String.length before_cursor
      | Some index -> String.length before_cursor - index - 1
    in
    let editor_room =
      Int.max 1 (height - List.length context_rows - List.length output_rows - 2)
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
    let editor_rows =
      visible_lines
      |> List.mapi ~f:(fun index line ->
        let cursor = if index + first_line = cursor_line then cursor_in_line else 0 in
        let chunk, _ = line_chunk line ~cursor in
        let source = clean chunk in
        H.Highlight_tm_engine.highlight_text
          t.highlighter
          ~lang:(Some "chatml")
          ~text:source
        |> List.hd
        |> Option.value ~default:[]
        |> row_of_spans)
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
        let horizontal = Int.max 0 (x - width + 1) in
        (if y >= height then None else Some (x - horizontal, y)), horizontal
    in
    let editor_rows =
      List.map editor_rows ~f:(fun image ->
        Notty.I.hcrop
          horizontal
          (Int.max 0 (Notty.I.width image - horizontal - width))
          image)
    in
    let rows =
      [ text_row H.Highlight_styles.(fg_hex "#adbac7" ++ bg_hex "#22272e") header ]
      @ context_rows
      @ output_rows
      @ [ text_row
            (H.Highlight_styles.fg_hex "#768390")
            "-- Enter=newline  :submit=send  :help --"
        ]
      @ editor_rows
    in
    let image = stack rows in
    let image = Notty.I.hcrop 0 (Int.max 0 (Notty.I.width image - width)) image in
    let image = Notty.I.vcrop 0 (Int.max 0 (Notty.I.height image - height)) image in
    image, cursor)
;;
