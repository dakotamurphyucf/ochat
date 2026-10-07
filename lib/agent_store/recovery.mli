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

  (** Capture the existing shared-preflight policy once. Restoration, conversion,
      application and equivalence must be deterministic for immutable inputs for
      this owner's lifetime. Application/equivalence are pure; [validate] may
      perform bounded archive reads and is repeated on every initial and final
      checkpoint state. No proof is imported or supplied by the caller. *)
  val create
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
    -> t

  (** Freshly validates every retained original frame, metadata, digest, chain,
      checkpoint anchor and current-domain transaction/snapshot before consulting
      the previous successful proof. Only an identical original checkpoint and a
      still-reachable identical certified head in that verified original-byte
      chain may seed replay at the prior head. Other routes fully replay; shared
      replay uses these effective anchors and requires complete state/carrier
      equivalence. Every route repeats final validation, including archive reads.

      Successful checks privately replace the proof with only the observed
      checkpoint routes. Failure/cancellation preserves the previous proof, which
      cannot bypass any fresh checks. Later pruning failure does not invalidate
      successful validation. Calls are serialized; cancellation and unexpected
      exceptions propagate. No tail repair, pruning or file writes occur. *)
  val check : t -> (unit, Store_error.t) result
end
