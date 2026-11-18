(** Snapshot plus journal recovery with hash-chain and counter validation. *)

type counters = private
  { transaction_sequence : int64
  ; transaction_hash : string option
  ; session_revision : int64
  ; event_sequence : int64
  ; generation : int
  }

(** Validate all retained transaction links, session ownership, event continuity
    and every fallback snapshot anchor, requiring agreement on the recovered head.
    Inputs must already have valid framing and
    decoded payloads. Does no IO or repair, and does not establish reference absence
    in archives or other consumers. Returns the validated retained journal head. *)
val validate_retained
  :  session_id:Agent_protocol.Id.Session.t
  -> snapshots:Snapshot.installed list
  -> transactions:Transaction.t list
  -> (counters, Store_error.t) result

(** Check exact stored-version metadata and original payload digests before
    conversion or current-domain restoration. All fallback anchors must agree. *)
val validate_stored_retained
  :  session_id:Agent_protocol.Id.Session.t
  -> snapshots:Snapshot.installed_stored list
  -> transactions:Transaction.Stored.t list
  -> (counters, Store_error.t) result

type 'state t =
  { state : 'state
  ; snapshot : Snapshot.installed option
  ; transactions : Transaction.t list
  ; latest_transaction_sequence : int64
  ; latest_transaction_hash : string option
  ; latest_session_revision : int64
  ; latest_event_sequence : int64
  ; repaired_crash_tail : bool
  }

(** [load] validates stored metadata and original-byte anchors before domain
    restoration. It restores every retained complete checkpoint, replays each
    checkpoint's later transactions through [apply], and validates every recovered
    state before repairing an incomplete journal tail. It returns the selected
    checkpoint's recovered state. Without a checkpoint it replays from [initial].
    Domain callbacks must have no mutation effects; bounded archive reads are
    permitted. *)
val load
  :  env:Eio_unix.Stdenv.base
  -> journal:Journal.t
  -> snapshot_directory:string
  -> max_snapshot_payload_length:int
  -> session_id:Agent_protocol.Id.Session.t
  -> initial:'state
  -> restore_snapshot:(Snapshot.t -> ('state, Store_error.t) result)
  -> apply:('state -> Transaction.t -> ('state, Store_error.t) result)
  -> validate_transaction:(Transaction.t -> (unit, Store_error.t) result)
  -> validate:('state -> (unit, Store_error.t) result)
  -> ('state t, Store_error.t) result

(** Same complete stored/domain/fallback/archive validation as [load], returning
    the selected recovered state without repairing an incomplete journal tail.
    No recovery metadata, state, snapshot or journal mutation occurs. Temporary
    store locking remains the caller's responsibility. [repaired_crash_tail] is
    false even when an incomplete tail was observed. Callback restrictions match
    [load]. *)
val read
  :  env:Eio_unix.Stdenv.base
  -> journal:Journal.t
  -> snapshot_directory:string
  -> max_snapshot_payload_length:int
  -> session_id:Agent_protocol.Id.Session.t
  -> initial:'state
  -> restore_snapshot:(Snapshot.t -> ('state, Store_error.t) result)
  -> apply:('state -> Transaction.t -> ('state, Store_error.t) result)
  -> validate_transaction:(Transaction.t -> (unit, Store_error.t) result)
  -> validate:('state -> (unit, Store_error.t) result)
  -> ('state t, Store_error.t) result

(** Complete restoration/retention preflight under the same existing owner;
    performs the same checks and fallback replays as [load] without repair or
    writes. Domain callbacks have the same effect restrictions as [load]. Run
    before checkpoint/journal pruning. *)
val preflight
  :  env:Eio_unix.Stdenv.base
  -> journal:Journal.t
  -> snapshot_directory:string
  -> max_snapshot_payload_length:int
  -> session_id:Agent_protocol.Id.Session.t
  -> initial:'state
  -> restore_snapshot:(Snapshot.t -> ('state, Store_error.t) result)
  -> apply:('state -> Transaction.t -> ('state, Store_error.t) result)
  -> validate_transaction:(Transaction.t -> (unit, Store_error.t) result)
  -> validate:('state -> (unit, Store_error.t) result)
  -> (unit, Store_error.t) result

(** Retention preflight with optional reuse of already validated newer tails.
    Every stored anchor, restored checkpoint, transaction domain and older prefix
    is checked as in [preflight]. A suffix is reused only after [equivalent] proves
    that the complete state and preservation carrier at that anchor agree. Every
    final fallback still runs [validate], including its bounded archive reads.

    [apply] must be deterministic and side-effect-free for immutable state and
    transaction inputs; it must not consult clocks or perform external IO.
    [equivalent] must also be deterministic and side-effect-free. [true] must
    establish complete state/carrier equality for every subsequent [apply] and
    validation, including unknown fields. Counter or digest equality alone is
    insufficient. [false] continues ordinary replay; errors refuse preflight.
    No state escapes this preflight and no repair, pruning or writes occur. *)
val preflight_shared
  :  env:Eio_unix.Stdenv.base
  -> journal:Journal.t
  -> snapshot_directory:string
  -> max_snapshot_payload_length:int
  -> session_id:Agent_protocol.Id.Session.t
  -> initial:'state
  -> restore_snapshot:(Snapshot.t -> ('state, Store_error.t) result)
  -> apply:('state -> Transaction.t -> ('state, Store_error.t) result)
  -> equivalent:('state -> 'state -> (bool, Store_error.t) result)
  -> validate_transaction:(Transaction.t -> (unit, Store_error.t) result)
  -> validate:('state -> (unit, Store_error.t) result)
  -> (unit, Store_error.t) result

(** Session-owned checkpoint preflight with an immutable successful replay proof.
    The object binds one journal, snapshot directory, session, limits and fixed
    callback/schema policy for its lifetime. It must be discarded on reopen or
    policy change; state and callbacks never escape through the existential API. *)
module Retention_preflight : sig
  type t

  module Budget : sig
    type t

    (** Bounds retained restoration entry count and aggregate original record
        bytes, not decoded OCaml heap. Negative values reject; zero disables
        restoration reuse without weakening any retention check. *)
    val create : max_entries:int -> max_original_bytes:int -> (t, Store_error.t) result

    (** 128 entries and 16 MiB of aggregate original record bytes. *)
    val default : t
  end

  (** Capture one fixed journal, directory, session, limits and callback policy.
      [restore_snapshot], conversion, [apply] and [equivalent] must be pure and
      deterministic for immutable inputs/results for this owner's lifetime, with
      no required IO, cancellation/logging effects or identity freshness. They
      must not consult mutable ambient policy. [validate] and
      [validate_transaction] remain fresh in their original order on every check;
      bounded archive reads remain permitted. No proof/cache can be imported. *)
  val create
    :  restoration_budget:Budget.t
    -> env:Eio_unix.Stdenv.base
    -> journal:Journal.t
    -> snapshot_directory:string
    -> max_snapshot_payload_length:int
    -> session_id:Agent_protocol.Id.Session.t
    -> initial:'state
    -> restore_snapshot:(Snapshot.t -> ('state, Store_error.t) result)
    -> apply:('state -> Transaction.t -> ('state, Store_error.t) result)
    -> equivalent:('state -> 'state -> (bool, Store_error.t) result)
    -> validate_transaction:(Transaction.t -> (unit, Store_error.t) result)
    -> validate:('state -> (unit, Store_error.t) result)
    -> t

  (** Freshly validates every retained original frame, metadata, digest, chain
      and checkpoint anchor before restoration reuse. The same owner may reuse
      successful immutable snapshot/transaction conversions only for exact
      original flags, digest and bytes (and the same snapshot route). Every
      supplied state and transaction validator, including initial/final archive
      checks, still runs.

      Replay-prefix reuse separately requires an identical original checkpoint
      and still-reachable identical certified head in the verified chain. Other
      routes fully replay; shared replay requires complete state/carrier equality.
      A cached original state is never substituted for a replayed head.

      Only complete success atomically replaces the proof and bounded restoration
      cache with currently observed originals. Failure/cancellation preserves the
      previous successful record and cannot bypass any fresh check. Budget
      misses/eviction affect performance only. Later pruning failure does not
      invalidate successful validation. Calls are serialized; cancellation and
      unexpected exceptions propagate. No repair, pruning or file writes occur.

      A caller retaining this owner's journal/Handle lifetime may perform a
      read-only speculative check while its canonical writer appends and snapshots
      are published. Such a check observes files non-atomically: success certifies
      only the originals and replay prefixes actually verified, and an error may
      reflect an intervening publication. It supplies no authority to prune or
      seal. Maintenance must freshly check the complete current basis again while
      excluding journal and snapshot mutation through later prune/seal operations.
      The caller must recheck its actual owner and immutable persistence basis
      before accepting a speculative result; a changed basis may defer the attempt,
      while an unchanged basis must preserve a returned error. Unexpected
      exceptions and cancellation still propagate. *)
  val check : t -> (unit, Store_error.t) result
end
