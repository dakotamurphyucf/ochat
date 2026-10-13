open Core
module Snapshot = Language_highlighting.Highlight_snapshot

module Language = struct
  type t =
    | Chatml
    | Chatmd
  [@@deriving equal]

  let of_hint = function
    | None -> None
    | Some hint ->
      (match String.lowercase hint with
       | "chatml" | "chatmli" -> Some Chatml
       | "chatmd" -> Some Chatmd
       | _ -> None)
  ;;
end

type error =
  | Too_large
  | Not_renderable

let max_source_bytes = 65_536
let max_lines = 2048
let max_line_bytes = 4096

let sanitize text ~checkpoint =
  let result = Buffer.create (String.length text) in
  let seen = ref 0 in
  ignore
    (Uutf.String.fold_utf_8
       (fun () _ -> function
          | `Malformed _ ->
            Int.incr seen;
            if !seen mod 4096 = 0 then checkpoint ();
            Buffer.add_char result '?'
          | `Uchar uchar ->
            Int.incr seen;
            if !seen mod 4096 = 0 then checkpoint ();
            let code = Stdlib.Uchar.to_int uchar in
            if code < 32 || (code >= 127 && code < 160)
            then Buffer.add_char result '?'
            else Uutf.Buffer.add_utf_8 result uchar)
       ()
       text
     : unit);
  Buffer.contents result
;;

let sanitize_rows rows ~checkpoint =
  List.map rows ~f:(fun row ->
    checkpoint ();
    List.map row ~f:(fun (attr, text) -> attr, sanitize text ~checkpoint))
;;

let truncation_marker = " [source preview truncated]"

let append_marker rows =
  match List.rev rows with
  | [] -> [ [ Notty.A.empty, truncation_marker ] ]
  | last :: before -> List.rev ((last @ [ Notty.A.empty, truncation_marker ]) :: before)
;;

let plain_preview ~text ~is_cancelled =
  let checkpoint () = if is_cancelled () then raise Exit in
  checkpoint ();
  let bounded = String.prefix text max_source_bytes in
  let all_lines = String.split_lines bounded in
  let lines = List.take all_lines max_lines in
  let truncated = ref (String.length bounded < String.length text) in
  if List.length all_lines > max_lines then truncated := true;
  let rows =
    List.map lines ~f:(fun line ->
      checkpoint ();
      if String.length line > max_line_bytes then truncated := true;
      [ Notty.A.empty, String.prefix line max_line_bytes ])
    |> sanitize_rows ~checkpoint
  in
  if !truncated then append_marker rows else rows
;;

let highlight language ~text ~theme ~is_cancelled =
  if String.length text > max_source_bytes
  then Error Too_large
  else (
    let checkpoint () = if is_cancelled () then raise Exit in
    let limits =
      Snapshot.Limits.create
        ~max_source_bytes
        ~max_identity_bytes:4096
        ~max_spans:max_source_bytes
        ~max_issues:64
        ~max_work_units:2_000_000
        ~max_nesting:128
      |> Or_error.ok_exn
    in
    let snapshot =
      match language with
      | Language.Chatml ->
        let id =
          Chatml_source.Id.create
            ~package_namespace:"chat-tui-display"
            ~relative_path:"fragment.chatml"
          |> Or_error.ok_exn
        in
        let source =
          Chatml_source.create
            id
            ~contents:text
            ~expected_digest:None
            ~max_bytes:(Some max_source_bytes)
            ~checkpoint
          |> Or_error.ok_exn
        in
        Snapshot.scan_chatml source ~limits ~checkpoint
      | Language.Chatmd ->
        let input =
          Chatmd_parser_input.create
            ~role:Chatmd_parser_input.Role.Root
            ~path:"chat-tui-display/fragment.chatmd"
            ~authored:text
            ~effective:text
            ~max_input_bytes:max_source_bytes
            ~checkpoint
          |> Result.ok
          |> Option.value_exn
        in
        Snapshot.scan_chatmd
          input
          ~view:Snapshot.Source_owner.Chatmd_view.Authored
          ~limits
          ~checkpoint
    in
    match snapshot with
    | Error _ -> Error Too_large
    | Ok snapshot ->
      let render_limits =
        Terminal_source_highlighting.Limits.create ~max_lines ~max_line_bytes
        |> Or_error.ok_exn
      in
      (match
         Terminal_source_highlighting.render
           snapshot
           ~theme
           ~limits:render_limits
           ~checkpoint
       with
       | Error _ -> Error Not_renderable
       | Ok rendered ->
         let rows =
           Terminal_source_highlighting.lines rendered |> sanitize_rows ~checkpoint
         in
         let rows =
           match Snapshot.status snapshot with
           | Snapshot.Status.Reached_eof -> rows
           | Snapshot.Status.Truncated _ -> append_marker rows
         in
         Ok rows))
;;
