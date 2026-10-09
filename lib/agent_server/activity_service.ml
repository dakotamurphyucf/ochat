open! Core
module P = Agent_protocol

module Observation = struct
  type t =
    { snapshot : P.Snapshot.t
    ; transient : P.Session_activity.Transient.t
    ; usage : P.Inference_query.Summary.t
    }
end

type t =
  { server_id : P.Id.Server.t
  ; principal : P.Principal.t
  ; now : P.Timestamp.t
  ; read : P.Id.Session.t -> (Observation.t, P.Error.t) result
  }

let create ~server_id ~principal ~now ~read = { server_id; principal; now; read }

let authorize t server_id =
  if not (P.Id.Server.equal t.server_id server_id)
  then Error (P.Error.invalid_request "activity query names another host")
  else if
    (not (P.Principal.has_scope t.principal View_session_transcript))
    || not (P.Principal.has_scope t.principal View_security_state)
  then
    Error
      (P.Error.create
         Permission_denied
         ~message:"activity requires transcript and security visibility"
         ~retryable:false
         ())
  else Ok ()
;;

let observe t (request : P.Activity_query.t) ~catalog =
  let open Result.Let_syntax in
  let%bind () = authorize t request.server_id in
  let%bind () =
    if List.length catalog > request.scan_limit
    then
      Error
        (P.Error.create
           Resource_limit
           ~message:"activity scan bound exceeded; narrow the catalog query"
           ~retryable:false
           ())
    else Ok ()
  in
  let projection =
    Activity_projection.create ~server_id:t.server_id ~principal:t.principal ~now:t.now
  in
  let%map rows =
    List.map catalog ~f:(fun (entry : P.Session_catalog.t) ->
      let%bind observation = t.read entry.session.id in
      Activity_projection.observe
        projection
        observation.snapshot
        ~catalog:entry
        ~transient:observation.transient
        ~usage:observation.usage)
    |> Result.all
  in
  List.filter rows ~f:(fun (row : P.Session_activity.t) ->
    List.is_empty request.reasons
    || List.exists row.attention ~f:(fun attention ->
      List.mem request.reasons attention.reason ~equal:P.Session_activity.Reason.equal))
;;

let work t (request : P.Session_work.Query.t) =
  let open Result.Let_syntax in
  let%bind () = authorize t (P.Session_ref.server_id request.session) in
  let%bind observation = t.read (P.Session_ref.session_id request.session) in
  let%bind () =
    if
      P.Id.Session.equal
        observation.snapshot.session.id
        (P.Session_ref.session_id request.session)
    then Ok ()
    else Error (P.Error.invalid_request "immutable work reader returned another session")
  in
  Work_projection.work
    (Work_projection.create ~server_id:t.server_id ~principal:t.principal)
    observation.snapshot
;;
