open! Core
module P = Agent_protocol
module Q = P.Recurrence_query
module W = P.Recurrence_control

type t =
  { connection : Connection.t
  ; session : P.Session_ref.t
  ; attachment : (unit -> P.Id.Attachment.t) option
  }

let create connection ~session ?attachment () = { connection; session; attachment }

let initialized t =
  match Connection.initialization t.connection with
  | None ->
    Error
      (P.Error.invalid_request "recurrence operations require an initialized connection")
  | Some initialized ->
    if P.Id.Server.equal initialized.server_id (P.Session_ref.server_id t.session)
    then Ok initialized
    else Error (P.Error.invalid_request "recurrence operation belongs to another host")
;;

let query t selector =
  let%bind.Result _ = initialized t in
  let attachment_id = Option.map t.attachment ~f:(fun attachment -> attachment ()) in
  Connection.request_without_history
    t.connection
    (P.Command.Recurrence_query { session = t.session; attachment_id; selector })
;;

let matches_query t outcome = P.Session_ref.equal t.session (Q.Result.session outcome)

let matches_entry id outcome ~entry =
  match outcome with
  | Q.Result.Unavailable _ -> true
  | Available { value; _ } -> P.Id.Recurrence.equal id (entry value).Q.Definition.id
;;

let invalid_query () =
  Error (P.Error.invalid_request "unexpected recurrence query response")
;;

let list t =
  match query t List with
  | Ok (P.Method_result.Recurrence_query (List outcome)) when matches_query t outcome ->
    Ok outcome
  | Ok _ -> invalid_query ()
  | Error _ as error -> error
;;

let get t id =
  match query t (Get id) with
  | Ok (P.Method_result.Recurrence_query (Get outcome))
    when matches_query t outcome && matches_entry id outcome ~entry:Fn.id -> Ok outcome
  | Ok _ -> invalid_query ()
  | Error _ as error -> error
;;

let status t id =
  match query t (Status id) with
  | Ok (P.Method_result.Recurrence_query (Status outcome))
    when matches_query t outcome
         && matches_entry id outcome ~entry:(fun status -> status.Q.Status.entry) ->
    Ok outcome
  | Ok _ -> invalid_query ()
  | Error _ as error -> error
;;

let history t id =
  match query t (History id) with
  | Ok (P.Method_result.Recurrence_query (History outcome))
    when matches_query t outcome
         && matches_entry id outcome ~entry:(fun history -> history.Q.History.entry) ->
    Ok outcome
  | Ok _ -> invalid_query ()
  | Error _ as error -> error
;;

let command t (intent : W.Intent.t) =
  let open Result.Let_syntax in
  let%bind initialized = initialized t in
  let%bind attachment_id =
    match t.attachment with
    | Some attachment -> Ok (attachment ())
    | None ->
      Error (P.Error.invalid_request "recurrence command requires a writer attachment")
  in
  let%bind request = W.Request.create ~session:t.session ~attachment_id intent in
  match%bind
    Connection.request_without_history t.connection (P.Command.Recurrence_control request)
  with
  | P.Method_result.Recurrence_control receipt ->
    let%map () =
      W.Receipt.check_intent
        receipt
        ~session:t.session
        ~principal_id:initialized.principal.id
        ~intent
    in
    receipt
  | _ -> Error (P.Error.invalid_request "unexpected recurrence command response")
;;
