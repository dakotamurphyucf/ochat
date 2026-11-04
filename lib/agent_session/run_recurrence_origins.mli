(** Private immutable Run-origin table. Complete raw wrappers and child carriers
    are retained here; the enclosing owner treats this field as an opaque value.
    Historical absence/null is retained. The existing bounded Run index owns
    capacity and validates exact retained admission/Run associations. *)
type t

val empty : t
val entries : t -> Recurrence_run_origin.t list
val find : t -> Agent_protocol.Id.Run.t -> Recurrence_run_origin.t option
val add : t -> Recurrence_run_origin.t -> (t, Agent_protocol.Error.t) result
val fields : t -> (string * Jsonaf.t) list
val of_field : Jsonaf.t option -> (t, Agent_protocol.Error.t) result
val validate_transition : previous:t -> t -> (unit, Agent_protocol.Error.t) result
