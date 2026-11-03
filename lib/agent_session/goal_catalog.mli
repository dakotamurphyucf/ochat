(** Bounded immutable subcarrier of the sole session document. At most sixteen
    retained goals, 256 immutable command receipts, 8MiB including reserved child
    growth, and one enabled foreground goal. Saturation rejects fresh commands;
    no receipt eviction, evidence deletion or execution authority is provided. *)
type t [@@deriving sexp]

val empty : t
val revision : t -> int64
val goals : t -> Agent_protocol.Goal.t list
val find : t -> Agent_protocol.Id.Goal.t -> Agent_protocol.Goal.t option
val document : t -> Agent_protocol.Id.Goal.t -> Goal_document.t option
val receipts : t -> Goal_command_receipt.t list

val receipt
  :  t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> key:Agent_protocol.Idempotency_key.t
  -> request_sha256:string
  -> (Goal_command_receipt.t option, Agent_protocol.Error.t) result

(** Pure checked metadata commit; the caller must independently authenticate the
    actual command. New goals must start held. Existing protected goal documents
    are adopted, not flattened. Fresh receipts never replace retained evidence. *)
val commit
  :  t
  -> goal:Goal_document.t
  -> receipt:Goal_command_receipt.t
  -> (t, Agent_protocol.Error.t) result

(** Checked existing-goal internal transition, independent of user receipt
    capacity. Cannot introduce a goal or discard any retained command evidence.
    This is pure structural validation; the actor must authenticate Run evidence. *)
val advance_existing : t -> goal:Goal_document.t -> (t, Agent_protocol.Error.t) result

(** At most sixteen distinct existing goals, each validated once, share one
    catalog revision. Empty or exact unchanged input returns None. No new goal,
    duplicate identity or user receipt can be introduced. *)
val advance_existing_many
  :  t
  -> goals:Goal_document.t list
  -> (t option, Agent_protocol.Error.t) result

(** Internal source/recovery disposition, independent of user receipt capacity.
    Enabled goals become held for source review; existing holds remain exact.
    Adopted source and all request/episode evidence are retained. No-op is None. *)
val hold_for_review : t -> (t option, Agent_protocol.Error.t) result

val validate
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (unit, Agent_protocol.Error.t) result

(** Existing goals and receipts are retained. A newly introduced goal must have
    its held revision-zero provenance and a matching newly retained command
    receipt at this transition's catalog frontier. Full snapshot decoding is
    separate and admits already advanced historical goals. *)
val validate_transition : t -> incoming:t -> (unit, Agent_protocol.Error.t) result

val to_jsonaf : t -> (Jsonaf.t, Agent_protocol.Error.t) result
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
val limits : Document_schema.Limits.t

(** Existing per-child settlement/hold positive growth, including catalog decimal
    width. The enclosing owner adds this to other children's reservations once. *)
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result

(** Charge actual preserved catalog unknowns plus the existing Goal_document
    positive child reservations. Whole-state owners run this after preservation. *)
val validate_encoded_capacity : t -> Jsonaf.t -> (unit, Agent_protocol.Error.t) result

(** The same positive growth reserve against the ACTUAL complete preserved host
    document and its smaller configured limits. Catalog headroom cannot consume
    bytes/fields/nodes promised by the enclosing session document. *)
val validate_host_capacity
  :  t
  -> Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) result
