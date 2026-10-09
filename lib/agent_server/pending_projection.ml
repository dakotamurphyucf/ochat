open! Core
module P = Agent_protocol
module H = P.Public_history
module Q = P.Pending_query

let require principal =
  if P.Principal.has_scope principal View_session_transcript
  then Ok ()
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"pending results require current transcript visibility"
         ~retryable:false
         ())
;;

let history principal (entry : H.t) =
  match entry.body with
  | Visible _ | Redacted _ -> Ok entry
  | Full payload ->
    if P.Principal.has_scope principal View_security_state
    then Ok entry
    else (
      let semantic = History_entry.Payload.semantic payload in
      match H.Visible.of_semantic semantic with
      | Some visible ->
        H.visible
          entry.id
          ~content_revision:entry.content_revision
          ~provenance:entry.provenance
          visible
      | None ->
        H.redacted
          entry.id
          ~content_revision:entry.content_revision
          ~provenance:entry.provenance
          (H.Redaction.create
             ~disclosed_header:(Some (Transcript.Header.of_semantic semantic))))
;;

let item principal (input : Q.Item.t) =
  let%bind.Result projected = history principal input.history in
  Q.Item.create ~history:projected ~generation:input.generation ~binding:input.binding
;;

let view principal (value : Q.View.t) =
  let open Result.Let_syntax in
  let%bind () = require principal in
  let%bind items = List.map value.page.items ~f:(item principal) |> Result.all in
  Q.View.create
    ~pending_revision:value.pending_revision
    ~page:{ items; next_cursor = value.page.next_cursor }
;;

let outcome principal value =
  let open Result.Let_syntax in
  let%bind () = require principal in
  match value with
  | Q.Outcome.Pending input ->
    let%map input = item principal input in
    Q.Outcome.pending input
  | Adopted { history_id; admitted_content_revision; current } ->
    let%bind current =
      match current with
      | None -> Ok None
      | Some entry -> Result.map (history principal entry) ~f:Option.some
    in
    Q.Outcome.adopted ~history_id ~admitted_content_revision ~current
  | Cancelled _ | Retired _ | Unavailable _ -> Ok value
;;

let control principal (value : P.Pending_control.Result.t) =
  let%map.Result outcome = outcome principal value.outcome in
  { value with outcome }
;;
