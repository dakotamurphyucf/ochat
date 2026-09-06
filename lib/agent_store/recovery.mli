(** Snapshot plus journal recovery with hash-chain and counter validation. *)

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
