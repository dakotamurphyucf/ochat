open! Core

(** Private durable state for a host-qualified session. An absent catalog is
    the historical empty state. The nested catalog owns its preserved fields. *)
type t = Recovery_notes.Catalog.t

val validate
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> (unit, Agent_protocol.Error.t) result

val for_owner
  :  t option
  -> owner:Agent_protocol.Session_ref.t
  -> (t, Agent_protocol.Error.t) result

val check_owner
  :  t option
  -> owner:Agent_protocol.Session_ref.t
  -> (unit, Agent_protocol.Error.t) result

(** Same-session replacement and adoption retain exact catalog custody. *)
val check_unchanged
  :  t option
  -> incoming:t option
  -> (unit, Agent_protocol.Error.t) result

val to_json : t -> (Jsonaf.t, Agent_protocol.Error.t) result

val of_json
  :  Jsonaf.t
  -> session_id:Agent_protocol.Id.Session.t
  -> (t, Agent_protocol.Error.t) result
