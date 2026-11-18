(** Immutable native initial-episode command DATA. The Actor supplies the session,
    principal and actual preparation revision; none is caller-supplied here.
    Generation must match source, revisions are nonnegative. Full input validates
    through [Run_input], including its 1 MiB/depth 64 bound. The request adds
    at most 4096 envelope bytes and one depth level. Construction and decoding
    enforce the same bounds. Unknown request fields reject. *)
type t [@@deriving equal, sexp_of]

val create
  :  generation:int
  -> source:Run_source.t
  -> goal_id:Id.Goal.t
  -> expected_goal_revision:int64
  -> expected_catalog_revision:int64
  -> input:Run_input.t
  -> key:Idempotency_key.t
  -> (t, Error.t) result

val generation : t -> int
val source : t -> Run_source.t
val goal_id : t -> Id.Goal.t
val expected_goal_revision : t -> int64
val expected_catalog_revision : t -> int64
val input : t -> Run_input.t
val key : t -> Idempotency_key.t

(** Canonical SHA256 over the complete version-tagged immutable command.
    Native Start admission uses this digest, excluding later preparation CAS. *)
val digest : t -> (string, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
