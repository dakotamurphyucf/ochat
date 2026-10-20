open! Core
module P = Agent_protocol
module W = P.Developer_repl_wire
module Client = Agent_client.Developer_repl_connection

let invalid message = Error (P.Error.invalid_request message)

let valid_utf8 text =
  Uutf.String.fold_utf_8
    (fun valid _ -> function
       | `Uchar _ -> valid
       | `Malformed _ -> false)
    true
    text
;;

module Limits = struct
  type t =
    { max_source_bytes : int
    ; max_source_lines : int
    ; max_history_entries : int
    ; max_history_bytes : int
    ; max_notice_bytes : int
    }

  let create
        ~max_source_bytes
        ~max_source_lines
        ~max_history_entries
        ~max_history_bytes
        ~max_notice_bytes
    =
    if
      max_source_bytes < 1
      || max_source_bytes > 8 * 1024 * 1024
      || max_source_lines < 1
      || max_source_lines > 65_536
      || max_history_entries < 1
      || max_history_entries > 1_024
      || max_history_bytes < 1
      || max_history_bytes > 8 * 1024 * 1024
      || max_notice_bytes < 1
      || max_notice_bytes > 64 * 1024
    then invalid "invalid terminal editor/history limits"
    else
      Ok
        { max_source_bytes
        ; max_source_lines
        ; max_history_entries
        ; max_history_bytes
        ; max_notice_bytes
        }
  ;;

  let max_source_bytes t = t.max_source_bytes
  let max_source_lines t = t.max_source_lines
  let max_notice_bytes t = t.max_notice_bytes
end

module Editor = struct
  type t =
    { source : string
    ; cursor_byte : int
    }

  type motion =
    | Left
    | Right
    | Home
    | End
  [@@deriving equal, sexp_of]

  let empty = { source = ""; cursor_byte = 0 }
  let source t = t.source
  let cursor_byte t = t.cursor_byte

  let boundaries source =
    Uuseg_string.fold_utf_8
      `Grapheme_cluster
      (fun (offset, offsets) cluster ->
         let offset = offset + String.length cluster in
         offset, offset :: offsets)
      (0, [ 0 ])
      source
    |> snd
    |> List.rev
  ;;

  let floor source pos =
    boundaries source
    |> List.take_while ~f:(fun offset -> offset <= pos)
    |> List.last
    |> Option.value ~default:0
  ;;

  let ceil source pos =
    boundaries source
    |> List.find ~f:(fun offset -> offset >= pos)
    |> Option.value ~default:(String.length source)
  ;;

  let previous source pos = floor source (pos - 1)
  let next source pos = ceil source (pos + 1)
  let lines source = 1 + String.count source ~f:(Char.equal '\n')

  let insert t ~limits insertion =
    if not (valid_utf8 insertion)
    then invalid "terminal source must be valid UTF-8"
    else if
      String.length insertion > limits.Limits.max_source_bytes - String.length t.source
    then invalid "terminal source exceeds editor byte limit"
    else if lines insertion - 1 > limits.Limits.max_source_lines - lines t.source
    then invalid "terminal source exceeds editor line limit"
    else (
      let before = String.prefix t.source t.cursor_byte in
      let after = String.drop_prefix t.source t.cursor_byte in
      let source = before ^ insertion ^ after in
      let cursor_byte = ceil source (t.cursor_byte + String.length insertion) in
      Ok { source; cursor_byte })
  ;;

  let remove_range t ~first ~last =
    let source = String.prefix t.source first ^ String.drop_prefix t.source last in
    { source; cursor_byte = floor source first }
  ;;

  let backspace t =
    if t.cursor_byte = 0
    then t
    else remove_range t ~first:(previous t.source t.cursor_byte) ~last:t.cursor_byte
  ;;

  let delete t =
    if t.cursor_byte = String.length t.source
    then t
    else remove_range t ~first:t.cursor_byte ~last:(next t.source t.cursor_byte)
  ;;

  let move t = function
    | Left -> { t with cursor_byte = previous t.source t.cursor_byte }
    | Right -> { t with cursor_byte = next t.source t.cursor_byte }
    | Home ->
      let before = String.prefix t.source t.cursor_byte in
      let cursor_byte =
        match String.rfindi before ~f:(fun _ c -> Char.equal c '\n') with
        | None -> 0
        | Some index -> index + 1
      in
      { t with cursor_byte = floor t.source cursor_byte }
    | End ->
      let after = String.drop_prefix t.source t.cursor_byte in
      let cursor_byte =
        match String.findi after ~f:(fun _ c -> Char.equal c '\n') with
        | None -> String.length t.source
        | Some (index, _) -> t.cursor_byte + index
      in
      { t with cursor_byte = floor t.source cursor_byte }
  ;;

  let final_line t =
    match String.rfindi t.source ~f:(fun _ c -> Char.equal c '\n') with
    | None -> t.source
    | Some index -> String.drop_prefix t.source (index + 1)
  ;;

  let source_before_final_line t =
    match String.rfindi t.source ~f:(fun _ c -> Char.equal c '\n') with
    | None -> ""
    | Some index -> String.prefix t.source index
  ;;

  let without_final_line t =
    match String.rfindi t.source ~f:(fun _ c -> Char.equal c '\n') with
    | None -> empty
    | Some index ->
      let source = String.prefix t.source (index + 1) in
      { source; cursor_byte = String.length source }
  ;;

  let of_source source = { source; cursor_byte = String.length source }
end

type keyed_operation =
  | Create
  | Submit
  | Run_task
  | Reset
  | Close
[@@deriving equal, sexp_of]

type operation =
  | Keyed of keyed_operation
  | Inspect_type
  | References
  | Observe
  | Cancel
[@@deriving equal, sexp_of]

type activity =
  | Ready
  | Busy of operation
  | Uncertain of keyed_operation
  | Needs_reset
  | Closed
[@@deriving equal, sexp_of]

type t =
  { limits : Limits.t
  ; context : string
  ; editor : Editor.t
  ; activity : activity
  ; view : W.View.t option
  ; submission : W.Submission.t option
  ; reference_page : W.Reference_page.t option
  ; task : W.Task_view.t option
  ; pending : Client.pending
  ; history : string list
  ; history_position : int option
  ; history_draft : string option
  ; notice : string option
  }

let empty_pending : Client.pending =
  { creates = []; submits = []; tasks = []; resets = []; closes = [] }
;;

let create ~limits ~context =
  if String.length context > limits.Limits.max_notice_bytes || not (valid_utf8 context)
  then invalid "invalid terminal context display"
  else
    Ok
      { limits
      ; context
      ; editor = Editor.empty
      ; activity = Ready
      ; view = None
      ; submission = None
      ; reference_page = None
      ; task = None
      ; pending = empty_pending
      ; history = []
      ; history_position = None
      ; history_draft = None
      ; notice = None
      }
;;

let limits t = t.limits
let context t = t.context
let editor t = t.editor
let activity t = t.activity
let view t = t.view
let submission t = t.submission
let reference_page t = t.reference_page
let task t = t.task
let pending t = t.pending
let history t = t.history
let notice t = t.notice

let current_revision t =
  match t.submission, t.view with
  | Some submission, _ -> Some (W.Submission.revision submission)
  | None, Some view -> Some (W.View.revision view)
  | None, None -> None
;;

let with_editor t editor =
  let source = Editor.source editor in
  if
    (not (valid_utf8 source))
    || String.length source > t.limits.Limits.max_source_bytes
    || Editor.lines source > t.limits.Limits.max_source_lines
    || not
         (List.mem
            (Editor.boundaries source)
            (Editor.cursor_byte editor)
            ~equal:Int.equal)
  then invalid "editor is outside this terminal state's limits"
  else Ok { t with editor; history_position = None; history_draft = None }
;;

let history_previous t =
  let position = Option.value t.history_position ~default:(-1) + 1 in
  match List.nth t.history position with
  | None -> t
  | Some source ->
    { t with
      editor = Editor.of_source source
    ; history_position = Some position
    ; history_draft =
        (match t.history_position with
         | None -> Some (Editor.source t.editor)
         | Some _ -> t.history_draft)
    }
;;

let history_next t =
  match t.history_position with
  | None -> t
  | Some 0 ->
    { t with
      editor = Editor.of_source (Option.value t.history_draft ~default:"")
    ; history_position = None
    ; history_draft = None
    }
  | Some position ->
    (match List.nth t.history (position - 1) with
     | None -> t
     | Some source ->
       { t with editor = Editor.of_source source; history_position = Some (position - 1) })
;;

let with_view t view =
  { t with
    view = Some view
  ; submission = None
  ; reference_page = None
  ; task = None
  ; editor = Editor.empty
  ; history_position = None
  ; history_draft = None
  }
;;

let with_submission t submission =
  match t.view with
  | None -> invalid "REPL submission has no current instance"
  | Some view ->
    if W.Wire_id.Epoch.equal (W.View.epoch view) (W.Submission.epoch submission)
    then Ok { t with submission = Some submission; reference_page = None }
    else invalid "REPL submission belongs to another epoch"
;;

let with_reference_page t reference_page = { t with reference_page = Some reference_page }

let task_reference t ~index =
  if index < 1
  then invalid "Task reference index must be positive"
  else (
    let references =
      match t.reference_page, t.submission with
      | Some page, _ -> W.Reference_page.items page
      | None, Some submission -> W.Submission.task_references submission
      | None, None -> []
    in
    match List.nth references (index - 1) with
    | None -> invalid "Task reference is not in the displayed page"
    | Some reference -> Ok reference)
;;

let with_task t task = { t with task = Some task }
let with_activity t activity = { t with activity }
let with_pending t pending = { t with pending }

let with_notice t notice =
  let notice = String.prefix notice t.limits.Limits.max_notice_bytes in
  { t with notice = Some notice }
;;

let submitted t ~source ~preserve_editor =
  let history =
    if
      String.is_empty source
      || (not (valid_utf8 source))
      || String.length source > t.limits.Limits.max_source_bytes
      || Editor.lines source > t.limits.Limits.max_source_lines
      || String.length source > t.limits.Limits.max_history_bytes
    then t.history
    else (
      let rec retain entries bytes count = function
        | [] -> List.rev entries
        | source :: rest ->
          let len = String.length source in
          if
            count >= t.limits.Limits.max_history_entries
            || len > t.limits.Limits.max_history_bytes - bytes
          then List.rev entries
          else retain (source :: entries) (bytes + len) (count + 1) rest
      in
      retain [] 0 0 (source :: t.history))
  in
  { t with
    editor = (if preserve_editor then t.editor else Editor.empty)
  ; history
  ; history_position = None
  ; history_draft = None
  }
;;
