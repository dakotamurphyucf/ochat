open! Core
module P = Agent_protocol
module A = Agent_session

type t =
  { principal : P.Principal.t
  ; read : P.Id.Session.t -> (A.Session_state.t, P.Error.t) result
  ; pagination : Pagination.t
  }

let create principal ~read ~pagination =
  if P.Principal.has_scope principal View_session_transcript
  then Ok { principal; read; pagination }
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"pending inspection requires transcript visibility"
         ~retryable:false
         ())
;;

let read t session_id =
  let%bind.Result state = t.read session_id in
  if P.Id.Session.equal session_id state.A.Session_state.identity.session_id
  then Ok state
  else Error (P.Error.invalid_request "pending reader returned another session")
;;

let list t (request : P.Pending_query.Request.t) =
  let open Result.Let_syntax in
  let%bind state = read t request.session_id in
  let%bind selected =
    Pagination.pending
      t.pagination
      t.principal
      request
      ~generation:state.identity.generation
      ~pending_revision:state.conversation.pending_revision
      state.conversation.deferred_user_entries
  in
  let%bind items =
    List.map selected.items ~f:(fun document ->
      A.Pending_inspection.item
        document
        ~project:(Principal_projection.pending_history_entry t.principal))
    |> Result.all
  in
  P.Pending_query.View.create
    ~pending_revision:state.conversation.pending_revision
    ~page:{ items; next_cursor = selected.next_cursor }
;;

let lookup t (request : P.Pending_query.Lookup_request.t) =
  let%bind.Result state = read t request.session_id in
  A.Pending_inspection.lookup
    state
    ~history_id:request.history_id
    ~project:(Principal_projection.pending_history_entry t.principal)
;;
