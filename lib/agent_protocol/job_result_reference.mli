(** Bounded reference to an exact retained terminal result, whether inline or
    artifact-backed. A reference does not grant read authority. Consumers use
    the owning session/job service, which rechecks current access and identity. *)
type t = private
  { session_id : Id.Session.t
  ; job_id : Id.Job.t
  ; generation : int
  ; attempt : int
  ; outcome : Stored_completion.outcome
  ; byte_length : int64
  ; sha256 : string
  ; artifact : Job_artifact.t option
  }
[@@deriving sexp]

val equal : t -> t -> bool
val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val of_job : Job.t -> (t, Error.t) result
val validate_job : t -> Job.t -> (unit, Error.t) result

(** Project metadata from an actual retained completion and exact occurrence.
    Validates artifact identity/length/digest; neither reads content nor grants
    authority. Unlike success-only run completion, every terminal outcome remains
    truthful in this reference. *)
val of_completion
  :  Stored_completion.t
  -> session_id:Id.Session.t
  -> job_id:Id.Job.t
  -> generation:int
  -> attempt:int
  -> (t, Error.t) result
