(** Render authoritative typed client state. Draft/activity sequence admission,
    gaps and terminal fencing belong to Agent_client, never a TUI event counter.
    Public views cannot populate writable canonical context. Actual tool results
    remain distinct from an ended operation whose tool outcome was not observed. *)
type t

val create : unit -> t

val apply
  :  t
  -> model:Model.t
  -> viewport_height:int
  -> Agent_projection.t
  -> (Model.projection_damage, Agent_protocol.Error.t) result
