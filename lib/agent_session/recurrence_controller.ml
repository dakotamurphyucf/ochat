open! Core
module P = Agent_protocol
module S = Agent_store.Session_store

type t =
  { store : S.t
  ; handle : S.Handle.t
  ; session : P.Session_ref.t
  ; entry_projection : Session_state.t -> Agent_store.Session_index.Entry.t
  }

type observation =
  { current : S.Lifecycle.Current.t
  ; handle : S.Handle.t
  }

let invalid message = Error (P.Error.invalid_request message)
let session t = t.session

let create store ~handle ~entry_projection =
  let open Result.Let_syntax in
  if not (S.owns_handle store handle)
  then invalid "recurrence controller Handle belongs to another store"
  else (
    let session =
      P.Session_ref.create
        ~server_id:(S.server_id store)
        ~session_id:(S.Handle.session_id handle)
    in
    let%map _ = P.Session_ref.of_json (P.Session_ref.to_json session) in
    { store; handle; session; entry_projection })
;;

let observe t (state : Session_state.t) =
  let open Result.Let_syntax in
  let session_id = P.Session_ref.session_id t.session in
  if not (P.Id.Session.equal session_id state.identity.session_id)
  then invalid "recurrence controller state belongs to another session"
  else (
    let entry = t.entry_projection state in
    if not (P.Id.Session.equal entry.session.id session_id)
    then invalid "recurrence controller projection belongs to another session"
    else (
      let%map current =
        S.current_lifecycle t.store t.handle ~current_entry:entry
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      in
      { current; handle = t.handle }))
;;

let revision t = (S.Lifecycle.Current.entry t.current).lifecycle_revision
let archived t = (S.Lifecycle.Current.entry t.current).archived

let automatic t =
  match (S.Lifecycle.Current.entry t.current).admission with
  | Automatic -> not (archived t)
  | Explicit_resume_required -> false
;;

let is_current t = S.Lifecycle.Current.is_current t.current t.handle
let same_store a b = phys_equal a.store b.store

let resolve_delegation t reference =
  if not (S.owns_handle t.store t.handle)
  then invalid "delegation observation no longer owns its session Handle"
  else
    Agent_store.Delegation_store.resolve (S.delegations t.store) reference
    |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
;;
