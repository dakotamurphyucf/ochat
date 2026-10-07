open! Core

(** Bounded, pure ordering owner for one attached foreground live stream.
    Drafts and activity are read views; they never authorize canonical append. *)
module Limits : sig
  type t

  val create
    :  max_operations:int
    -> max_receipts:int
    -> max_future_events:int
    -> max_event_bytes:int
    -> max_total_bytes:int
    -> draft_limits:Transcript.Draft.Limits.t
    -> (t, Agent_protocol.Error.t) result

  (** 16 operations, 1024 receipts, 256 future events, 16 MiB per event and
      64 MiB retained encoded content across receipts, future events, drafts and
      activity. Count bounds additionally bound graph overhead; this is not an
      exact OCaml heap-size promise. *)
  val default : t
end

type continuity =
  | Complete
  | Incomplete of { first_missing_sequence : int64 }
[@@deriving equal, sexp_of]

type operation_view = private
  { operation_id : Agent_protocol.Id.Operation.t
  ; continuity : continuity
  ; drafts : Transcript.Draft.t
  ; activities : Agent_protocol.Activity.Tool.summary list
  }

type t

val empty : ?limits:Limits.t -> unit -> t
val clear : t -> t

(** Admits complete typed active summaries from a snapshot under the same
    aggregate budget. Duplicate keys are rejected rather than overwritten.
    An operation already fenced in this receiver requires a fresh snapshot
    installation; seeding cannot reopen it. *)
val seed_activity
  :  t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> Agent_protocol.Activity.Tool.summary list
  -> (t, Agent_protocol.Error.t) result

(** Replaces transient components from an authoritative replacement snapshot.
    Receipts, high water, future anchors and fences survive. Cleared draft/channel
    prefixes stay explicitly unavailable; omitted text is never concatenated as
    though it had been observed. *)
val replace_snapshot
  :  t
  -> active_operation:Agent_protocol.Operation.t option
  -> Agent_protocol.Activity.Tool.summary list
  -> (t, Agent_protocol.Error.t) result

val operations : t -> operation_view list
val activities : t -> Agent_protocol.Activity.Tool.summary list

(** At most the latest observed terminal operation, frozen under the same
    retained-byte budget. Unfinished tools stay [Running], meaning their outcome
    was not observed; terminal context does not fabricate a tool outcome.
    Fresh/replacement snapshots, a new operation or [clear] retire this view.
    It never supplies snapshot active calls and subsequent live traffic remains
    fenced. *)
val terminal_view : t -> operation_view option

val retained_bytes : t -> int

(** Exact retained duplicate receipts are idempotent; retained conflicts fail.
    An unretained sequence below high-water is obsolete and ignored, without
    claiming verified equality. Future durable anchors queue within bounds.
    The authoritative foreground operation and terminal fences prevent stale
    resurrection. A gap marks missing prefixes explicitly. Errors leave the
    receiver unchanged. *)
val apply
  :  t
  -> durable_sequence:int64
  -> active_operation:Agent_protocol.Operation.t option
  -> canonical_history:Agent_protocol.Public.History.t list
  -> Agent_protocol.Event.Recoverable.t
  -> (t, Agent_protocol.Error.t) result

(** Applies actual lifecycle fences and drains reached future anchors. Root
    finalized payloads reconcile exactly against Full canonical read entries;
    nested items remain isolated and Visible/Redacted are never imported. *)
val advance_durable
  :  t
  -> Agent_protocol.Public.Durable.t
  -> previous_operation:Agent_protocol.Operation.t option
  -> active_operation:Agent_protocol.Operation.t option
  -> canonical_history:Agent_protocol.Public.History.t list
  -> (t, Agent_protocol.Error.t) result
