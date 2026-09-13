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

(** [load] validates storage structure, restores an optional snapshot, replays
    later transactions through [apply], and validates the resulting state. *)
val load
  :  env:Eio_unix.Stdenv.base
  -> journal:Journal.t
  -> snapshot_directory:string
  -> max_snapshot_payload_length:int
  -> session_id:Agent_protocol.Id.Session.t
  -> initial:'state
  -> restore_snapshot:(string -> ('state, Store_error.t) result)
  -> apply:('state -> Transaction.t -> ('state, Store_error.t) result)
  -> validate:('state -> (unit, Store_error.t) result)
  -> ('state t, Store_error.t) result
