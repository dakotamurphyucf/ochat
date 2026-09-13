(** Versioned reference to a complete serialized terminal result. The reference
    binds its blob to one session, job generation and attempt; it is not authority
    to read a different session or bypass capability/disclosure checks. *)
type t = private
  { session_id : Id.Session.t
  ; job_id : Id.Job.t
  ; generation : int
  ; attempt : int
  ; blob : Blob.Metadata.t
  }
[@@deriving sexp]

val media_type : string

val create
  :  session_id:Id.Session.t
  -> job_id:Id.Job.t
  -> generation:int
  -> attempt:int
  -> blob:Blob.Metadata.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
val allowed_use : t -> string
