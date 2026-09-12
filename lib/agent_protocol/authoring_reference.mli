(** Persisted evidence of an exact documentation response, not an execution grant
    or proof of model delivery. Decode only as part of trusted persisted state. *)
type part =
  { index : int
  ; item_sha256 : string
  }
[@@deriving equal, sexp]

type topic =
  { topic : Authoring_guidance.topic
  ; total_parts : int
  ; parts : part list
  }
[@@deriving equal, sexp]

type t = private
  { version : int
  ; query_identity : string
  ; host_identity : string
  ; capability_fingerprint : string
  ; scope : string
  ; surface_id : string
  ; corpus_identity : string
  ; response_sha256 : string
  ; topics : topic list
  }
[@@deriving equal, sexp]

val create
  :  query_identity:string
  -> host_identity:string
  -> capability_fingerprint:string
  -> scope:string
  -> surface_id:string
  -> corpus_identity:string
  -> response_sha256:string
  -> topics:topic list
  -> (t, Error.t) result

(** Version/closed-shape, identity and coverage validation. At most 1024 topics,
    4096 total recorded parts and 1 MiB encoded metadata. Per-topic indexes must
    be strictly increasing and within the declared total. Complete means all
    parts occur in this response, never just that this is the last query page. *)
val validate : t -> (unit, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

(** Digest of the actual response JSON. Neither comparison nor decoding gives a
    caller permission to attach the receipt to an invocation/history entry. *)
val matches_response : t -> Jsonaf.t -> bool

val matches_output : t -> Jsonaf.t -> bool
val scope_for : session_id:Id.Session.t -> generation:int -> string
