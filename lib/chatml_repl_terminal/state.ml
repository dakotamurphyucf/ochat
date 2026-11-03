open! Core
module P = Agent_protocol
module W = P.Developer_repl_wire
module Client = Agent_client.Developer_repl_connection
module Inferred_bindings = Inferred_bindings

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

module Submission_source = struct
  type t =
    { suffix : string
    ; combined : string
    }

  let suffix t = t.suffix
  let combined t = t.combined
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
  ; submitted_source : string option
  ; submitted_source_digest : Chatml_source.Digest.t option
  ; continuation_source : string option
  ; inferred_bindings : Inferred_bindings.t
  ; completion : Completion.t option
  ; completion_notice : string option
  ; accepted_names : string list
  ; history_search : (History_search.t * Editor.t) option
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
      ; submitted_source = None
      ; submitted_source_digest = None
      ; continuation_source = None
      ; inferred_bindings = Inferred_bindings.empty
      ; completion = None
      ; completion_notice = None
      ; accepted_names = []
      ; history_search = None
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
let submitted_source t = t.submitted_source
let submitted_source_digest t = t.submitted_source_digest
let continuation_source t = t.continuation_source
let inferred_bindings t = t.inferred_bindings
let completion t = t.completion
let completion_notice t = t.completion_notice
let history_search t = Option.map t.history_search ~f:fst
let reference_page t = t.reference_page
let task t = t.task
let pending t = t.pending
let history t = t.history
let notice t = t.notice

let fits_source t source =
  String.length source <= t.limits.Limits.max_source_bytes
  && Editor.lines source <= t.limits.Limits.max_source_lines
  && valid_utf8 source
;;

let fits_combined t suffix =
  let prefix = Option.value t.continuation_source ~default:"" in
  String.length suffix <= t.limits.Limits.max_source_bytes - String.length prefix
  && Editor.lines suffix - 1 <= t.limits.Limits.max_source_lines - Editor.lines prefix
;;

let submission_source t ~suffix =
  if not (fits_source t suffix && fits_combined t suffix)
  then
    invalid
      "combined continuation source exceeds terminal byte/line limits; shorten the \
       suffix or :reset"
  else
    Ok
      { Submission_source.suffix
      ; combined = Option.value t.continuation_source ~default:"" ^ suffix
      }
;;

let current_revision t =
  match t.submission, t.view with
  | Some submission, _ -> Some (W.Submission.revision submission)
  | None, Some view -> Some (W.View.revision view)
  | None, None -> None
;;

let refresh_completion t =
  { t with
    completion_notice = None
  ; completion =
      (match t.continuation_source with
       | Some _ -> None
       | None ->
         Completion.query
           ~source:(Editor.source t.editor)
           ~cursor_byte:(Editor.cursor_byte t.editor)
           ~binding_names:t.accepted_names)
  }
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
  else if
    String.equal source (Editor.source t.editor)
    && Int.equal (Editor.cursor_byte editor) (Editor.cursor_byte t.editor)
  then Ok t
  else
    Ok
      (refresh_completion
         { t with editor; history_position = None; history_draft = None })
;;

let dismiss_completion t = { t with completion = None; completion_notice = None }

let with_session_completion t ~names ~notice =
  match t.continuation_source with
  | Some _ -> dismiss_completion t
  | None ->
    { t with
      completion =
        Completion.query_with_session_names
          ~source:(Editor.source t.editor)
          ~cursor_byte:(Editor.cursor_byte t.editor)
          ~binding_names:t.accepted_names
          ~session_names:names
    ; completion_notice = notice
    }
;;

let complete t =
  match t.completion with
  | Some _ -> t
  | None -> refresh_completion t
;;

let completion_next t = { t with completion = Option.map t.completion ~f:Completion.next }

let completion_previous t =
  { t with completion = Option.map t.completion ~f:Completion.previous }
;;

let accept_completion t =
  match t.completion with
  | None -> Ok (complete t)
  | Some completion ->
    (match
       Completion.accept
         completion
         ~source:(Editor.source t.editor)
         ~cursor_byte:(Editor.cursor_byte t.editor)
     with
     | None -> Ok (dismiss_completion t)
     | Some (source, cursor_byte) ->
       with_editor t { Editor.source; cursor_byte } |> Result.map ~f:dismiss_completion)
;;

let history_search_available t =
  match t.activity, t.task, t.continuation_source with
  | _, _, Some _ -> false
  | Ready, None, None -> true
  | Ready, Some task, None ->
    (match W.Task_view.phase task with
     | Terminal _ -> true
     | Reserved | Waiting_permission | Execution_started -> false)
  | (Busy _ | Needs_reset | Uncertain _ | Closed), _, None -> false
;;

let start_history_search t =
  if not (history_search_available t)
  then t
  else (
    match t.activity, t.history_search with
    | Ready, None ->
      { t with
        history_search = Some (History_search.create ~history:t.history, t.editor)
      ; completion = None
      ; completion_notice = None
      }
    | Ready, Some (search, draft) ->
      { t with history_search = Some (History_search.next search, draft) }
    | (Busy _ | Needs_reset | Uncertain _ | Closed), _ -> t)
;;

let edit_history_search t ~f =
  match t.history_search with
  | None -> Ok t
  | Some (search, draft) ->
    f search
    |> Result.map ~f:(fun search -> { t with history_search = Some (search, draft) })
;;

let cancel_history_search t =
  match t.history_search with
  | None -> t
  | Some (_, draft) ->
    { t with
      history_search = None
    ; editor = draft
    ; completion = None
    ; completion_notice = None
    }
;;

let accept_history_search t =
  match t.history_search with
  | None -> t
  | Some (search, draft) ->
    let editor =
      Option.value_map
        (History_search.matched_source search)
        ~default:draft
        ~f:Editor.of_source
    in
    { t with
      history_search = None
    ; editor
    ; completion = None
    ; completion_notice = None
    ; history_position = None
    ; history_draft = None
    }
;;

let history_previous t =
  if Option.is_some t.continuation_source
  then t
  else (
    let position = Option.value t.history_position ~default:(-1) + 1 in
    match List.nth t.history position with
    | None -> t
    | Some source ->
      { t with
        editor = Editor.of_source source
      ; completion = None
      ; completion_notice = None
      ; history_position = Some position
      ; history_draft =
          (match t.history_position with
           | None -> Some (Editor.source t.editor)
           | Some _ -> t.history_draft)
      })
;;

let history_next t =
  if Option.is_some t.continuation_source
  then t
  else (
    match t.history_position with
    | None -> t
    | Some 0 ->
      { t with
        editor = Editor.of_source (Option.value t.history_draft ~default:"")
      ; completion = None
      ; completion_notice = None
      ; history_position = None
      ; history_draft = None
      }
    | Some position ->
      (match List.nth t.history (position - 1) with
       | None -> t
       | Some source ->
         { t with
           editor = Editor.of_source source
         ; completion = None
         ; completion_notice = None
         ; history_position = Some (position - 1)
         }))
;;

let with_view t view =
  { t with
    view = Some view
  ; submission = None
  ; submitted_source = None
  ; submitted_source_digest = None
  ; continuation_source = None
  ; inferred_bindings = Inferred_bindings.empty
  ; completion = None
  ; completion_notice = None
  ; accepted_names = []
  ; history_search = None
  ; reference_page = None
  ; task = None
  ; notice = None
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
    then (
      let inferred_bindings =
        match t.submission with
        | Some previous
          when W.Wire_id.Revision.equal
                 (W.Submission.revision previous)
                 (W.Submission.revision submission) -> t.inferred_bindings
        | Some _ | None -> Inferred_bindings.empty
      in
      Ok { t with submission = Some submission; inferred_bindings; reference_page = None })
    else invalid "REPL submission belongs to another epoch"
;;

let with_inferred_bindings t inferred_bindings =
  let names =
    Inferred_bindings.entries inferred_bindings
    |> List.filter_map ~f:(fun entry ->
      W.Type_inspection.binding_name entry.Inferred_bindings.inspection)
    |> List.filter ~f:(fun name -> String.length name <= 128)
  in
  let accepted_names =
    List.dedup_and_sort (names @ t.accepted_names) ~compare:String.compare
    |> fun names -> List.take names 128
  in
  refresh_completion { t with inferred_bindings; accepted_names }
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

let with_activity t activity =
  match activity with
  | Ready -> { t with activity }
  | Busy _ ->
    { t with
      activity
    ; completion = None
    ; completion_notice = None
    ; history_search = None
    }
  | Needs_reset | Uncertain _ | Closed ->
    { t with
      activity
    ; completion = None
    ; completion_notice = None
    ; accepted_names = []
    ; history_search = None
    }
;;

let with_pending t pending = { t with pending }

let with_notice t notice =
  let notice = String.prefix notice t.limits.Limits.max_notice_bytes in
  { t with notice = Some notice }
;;

let with_submitted_source t ~source =
  let submitted_source =
    if
      valid_utf8 source
      && String.length source <= t.limits.Limits.max_source_bytes
      && Editor.lines source <= t.limits.Limits.max_source_lines
    then Some source
    else None
  in
  let submitted_source_digest =
    Option.map submitted_source ~f:(fun source ->
      Chatml_source.Digest.of_contents source ~checkpoint:Fn.id)
  in
  { t with submitted_source; submitted_source_digest }
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
  let t = with_submitted_source t ~source in
  { t with
    editor = (if preserve_editor then t.editor else Editor.empty)
  ; continuation_source = None
  ; task = None
  ; notice = None
  ; history
  ; completion = None
  ; completion_notice = None
  ; history_search = None
  ; history_position = None
  ; history_draft = None
  }
;;

let acknowledge_source t ~source ~preserve_editor ~continue_line submission =
  let combined = Submission_source.combined source in
  match W.Submission.status submission with
  | Rejected _ | Finished | Stopped _ -> submitted t ~source:combined ~preserve_editor
  | Need_more _ ->
    let t = submitted t ~source:"" ~preserve_editor in
    let t = with_submitted_source t ~source:combined in
    let t = { t with continuation_source = Some combined } in
    if preserve_editor
    then (
      let suffix = Submission_source.suffix source in
      let extends_submitted = String.is_prefix (Editor.source t.editor) ~prefix:suffix in
      let t =
        if extends_submitted
        then
          { t with
            editor =
              Editor.of_source
                (String.drop_prefix (Editor.source t.editor) (String.length suffix))
          }
        else t
      in
      if fits_combined t (Editor.source t.editor)
      then
        if extends_submitted
        then t
        else
          with_notice
            t
            "Exact request acknowledged; edited draft kept as suffix. Review it before \
             appending, or :reset"
      else
        with_notice t "Draft exceeds remaining continuation budget; shorten it or :reset")
    else if continue_line && fits_combined t "\n"
    then (
      match
        Editor.insert t.editor ~limits:t.limits "\n" |> Result.bind ~f:(with_editor t)
      with
      | Ok t -> t
      | Error _ ->
        with_notice t "Continuation source is at its limit; :reset to discard it")
    else if continue_line
    then with_notice t "Continuation source is at its limit; :reset to discard it"
    else t
;;
