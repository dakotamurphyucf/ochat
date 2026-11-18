open! Core
module P = Agent_protocol

let error code message = Error (P.Error.create code ~message ~retryable:false ())

type status =
  | Writable
  | Read_only
  | Disconnected
  | Expired

let status (attachment : P.Session.Attachment.t) ~now =
  match attachment.mode, attachment.owner_lease with
  | Read_only, _ -> Read_only
  | Read_write, _ | Owner_read_write, None -> Writable
  | Owner_read_write, Some lease ->
    if Option.is_some lease.disconnect_grace_until
    then Disconnected
    else if P.Timestamp.compare lease.expires_at (now ()) <= 0
    then Expired
    else Writable
;;

let require (state : Session_state.t) ~attachment_id ~now =
  match
    List.find state.attachments ~f:(fun attachment ->
      P.Id.Attachment.equal attachment.id attachment_id)
  with
  | None -> error Invalid_request "attachment is not active"
  | Some attachment ->
    (match status attachment ~now with
     | Writable -> Ok attachment
     | Read_only ->
       error Permission_denied "read-only attachment cannot mutate the session"
     | Disconnected -> error Lease_stale "owner attachment is disconnected"
     | Expired -> error Lease_stale "owner lease has expired")
;;

let any (state : Session_state.t) ~now =
  List.exists state.attachments ~f:(fun attachment ->
    match status attachment ~now with
    | Writable -> true
    | Read_only | Disconnected | Expired -> false)
;;
