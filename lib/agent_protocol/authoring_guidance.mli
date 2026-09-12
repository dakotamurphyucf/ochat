(** Host-owned provenance for guidance, never inferred from message text. Source
    identities are content hashes, not paths, credentials or executable grants. *)
type source =
  | Installed of string
  | Authored of string
[@@deriving equal, sexp]

type purpose =
  | Primer
  | Preload
  | Reference
  | Rediscovery
[@@deriving equal, sexp]

type topic =
  { id : string
  ; document_sha256 : string
  ; source : source
  ; complete : bool
  }
[@@deriving equal, sexp]

type t = private
  { version : int
  ; context_identity : string
  ; policy_fingerprint : string
  ; payload_sha256 : string
  ; purpose : purpose
  ; topics : topic list
  }
[@@deriving equal, sexp]

(** [context_identity] binds installed language/runtime, target and effective
    capability identities. [policy_fingerprint] comes from resolved author policy.
    The payload digest binds the complete provider item, including its role.
    Rediscovery pointers must not claim to contain complete topic content. *)
val create
  :  context_identity:string
  -> policy_fingerprint:string
  -> purpose:purpose
  -> topics:topic list
  -> payload:Jsonaf.t
  -> (t, Error.t) result

val validate : t -> (unit, Error.t) result
val valid_topic : topic -> bool
val topic_to_json : topic -> Jsonaf.t
val topic_of_json : Jsonaf.t -> (topic, Error.t) result
val matches_payload : t -> Jsonaf.t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
