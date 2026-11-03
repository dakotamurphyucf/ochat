open! Core

(** Bounded, inert captured selections in the universal session document.
    Restoring contents does not reauthorize a source or insert provider context.
    Captures and receipts are never evicted. Saturation rejects fresh commands;
    continuing requires a new session or a future explicit maintenance policy. *)
type t [@@deriving sexp]

val empty : t

(** Receipt frontier: every newly accepted command, including a semantic no-op,
    advances once. Exact retained retries never advance or consume capacity. *)
val revision : t -> int64

val project : t -> Instruction_content.Loaded.t list
val skills : t -> Instruction_content.Loaded.t list
val captures : t -> Instruction_capture.t list
val receipts : t -> Instruction_command.Receipt.t list
val summary : t -> Agent_protocol.Instruction_status.t

type decision =
  | Retained of Instruction_command.Receipt.t
  | Commit of
      { selection : t
      ; receipt : Instruction_command.Receipt.t
      }

(** Pure CAS/idempotence plan, NOT authenticated activation. Only OChat's future
    genuine source-use owner may expose this through a host-installed port.
    Principal/key conflicts are immutable, including originally no-op commands. *)
val prepare
  :  t
  -> request:Instruction_command.t
  -> principal_id:Agent_protocol.Id.Principal.t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> now:Agent_protocol.Timestamp.t
  -> (decision, Agent_protocol.Error.t) result

val validate
  :  t
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (unit, Agent_protocol.Error.t) result

(** Existing captures, exact manifests, payloads, and immutable receipts remain.
    Unknown compatible data is adopted rather than flattened. *)
val adopt : t -> incoming:t -> (t, Agent_protocol.Error.t) result

val validate_update : t -> incoming:t -> (unit, Agent_protocol.Error.t) result
val to_jsonaf : t -> (Jsonaf.t, Agent_protocol.Error.t) result
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result

(** Opaque outer ownership, as for the ledger: this child owns its complete
    document and compatible unknowns. Its validating codec/adopt, not an outer
    shape split, protects nested capture, receipt and envelope continuity. *)
val shape : Document_schema.Shape.t

val limits : Document_schema.Limits.t

(** Includes actual preserved unknowns and a positive Clear reserve against the
    selection and complete host DOCUMENT, including counter decimal growth.
    This does not reserve an independently configured Transaction/Journal frame.
    Before exposing activation, the genuine host owner must also preflight Clear
    against its actual transaction, audit, previous-hash and journal byte profile.
    Document admission alone cannot promise Clear persistence under arbitrary
    journal configuration. *)
val validate_host_capacity
  :  t
  -> Jsonaf.t
  -> limits:Document_schema.Limits.t
  -> (unit, Agent_protocol.Error.t) result

(** Internal composition of reservations with other whole-state subcarriers.
    Subtract only this positive future-growth reserve, retain max_depth, reject
    nonpositive residual bounds. Pass the unchanged actual host encoding and
    residual limits to the next owner so independent reservations add. *)
val remaining_host_limits
  :  t
  -> Document_schema.Limits.t
  -> (Document_schema.Limits.t, Document_schema.Error.t) result
