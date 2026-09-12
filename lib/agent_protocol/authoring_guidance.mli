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
  ; fragments : fragment list [@sexp.list]
  ; surface_id : string option [@sexp.option]
  }

and part =
  { index : int
  ; item_sha256 : string
  }

and fragment =
  { topic_id : string
  ; total_parts : int
  ; parts : part list
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

(** Version-2 reference provenance. Every topic has exactly one coverage record;
    indexes are ordered and unique, and per-page [complete] requires every part.
    At most 1024 topics and 4096 parts are recorded, matching query-receipt bounds.
    Version-1 primer/preload/reference/pointer
    records remain readable and do not acquire fragment metadata. The host must
    derive this evidence from a verified query receipt and the actual provider
    item; model-supplied labels are not an authority to create provenance. *)
val create_reference
  :  context_identity:string
  -> policy_fingerprint:string
  -> topics:topic list
  -> fragments:fragment list
  -> payload:Jsonaf.t
  -> (t, Error.t) result

(** Version-3 references preserve the queried compiler surface. Version-1/2
    records remain readable without inventing a surface for legacy evidence. *)
val create_surface_reference
  :  surface_id:string
  -> context_identity:string
  -> policy_fingerprint:string
  -> topics:topic list
  -> fragments:fragment list
  -> payload:Jsonaf.t
  -> (t, Error.t) result

(** A surface-specific pointer contains no topic content or fragment coverage. *)
val create_surface_rediscovery
  :  surface_id:string
  -> context_identity:string
  -> policy_fingerprint:string
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
