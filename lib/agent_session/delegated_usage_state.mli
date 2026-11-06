(** Canonical whole-ledger accounting DATA for one actual delegated session.
    These values carry no Goal/control or execution permission. The document
    owner separately retains unknown fields and reserves aggregate growth. *)
type t

val equal : t -> t -> bool
val reference : t -> Agent_store.Delegation_store.Reference.t
val settled : t -> Goal_usage.t
val last_ledger_revision : t -> int64

(** Only an empty, exact-history canonical ledger can bootstrap a new partition.
    The actual child-creation owner authenticates the same immutable reference;
    historical absence is not evidence for this constructor. *)
val create
  :  reference:Agent_store.Delegation_store.Reference.t
  -> ledger:Inference_ledger.t
  -> (t, Agent_protocol.Error.t) result

(** Resolves exact ledger revision, coverage and cumulative settled counts. *)
val validate : t -> ledger:Inference_ledger.t -> (unit, Agent_protocol.Error.t) result

(** Every actual canonical writer edge folds ended removals and Untracked
    progress before publication. Unchanged retained observations stay solely in
    the canonical ledger and replace their prior contribution on read. *)
val reconcile
  :  t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> (t, Agent_protocol.Error.t) result

val usage
  :  t
  -> ledger:Inference_ledger.t
  -> (Goal_usage.t, Agent_protocol.Error.t) result

val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Same identity and monotone exact canonical frontier; no arbitrary settled
    overwrite. [validate_transition] additionally proves the actual fold. *)
val validate_update : t -> incoming:t -> (unit, Agent_protocol.Error.t) result

val validate_transition
  :  t
  -> incoming:t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> (unit, Agent_protocol.Error.t) result

(** Maximal known growth, for private carrier/capacity probing only. *)
val capacity_projection : t -> Jsonaf.t
