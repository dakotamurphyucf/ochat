(** Authenticated, principal/query/data-bound cursors. Collection changes or host
    restart invalidate old cursors explicitly. No cursor retains server memory.
    Partial history windows advertise structural incompleteness and navigation
    cursors; they must not be used as model input without completing the window. *)
type t

val create : unit -> t

val lists
  :  t
  -> Agent_protocol.Principal.t
  -> Agent_protocol.Command.t
  -> Agent_protocol.Method_result.t
  -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result

val history
  :  t
  -> Agent_protocol.Principal.t
  -> Agent_protocol.Session.Get_request.t
  -> Agent_protocol.Snapshot.t
  -> (Agent_protocol.Snapshot.t, Agent_protocol.Error.t) result
