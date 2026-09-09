(** Explicit async-job completion storage. Inline values retain their existing
    Completion encoding. Artifact envelopes occur only at this storage boundary,
    never by interpreting an arbitrary business JSON object's shape. *)
type outcome =
  | Succeeded
  | Failed
  | Cancelled
  | Expired
[@@deriving equal, sexp]

type t =
  | Inline of Completion.t
  | Artifact of
      { outcome : outcome
      ; reference : Job_artifact.t
      }
[@@deriving sexp]

val outcome : t -> outcome
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Bind an actual validated completion to a prepared artifact, verifying its
    serialized length and digest. Does not grant read or publication authority. *)
val artifact : Job_artifact.t -> Completion.t -> (t, Error.t) result

(** Compare a validated completion with an inline value or the artifact's outcome,
    byte length and digest. Does not read files or run effects. *)
val matches : t -> Completion.t -> (bool, Error.t) result

(** Load and revalidate an artifact's exact outcome and content. The host loader
    must enforce ownership, disclosure, bounded reads and raw byte verification. *)
val materialize
  :  load:(Job_artifact.t -> (Completion.t, Error.t) result)
  -> t
  -> (Completion.t, Error.t) result
