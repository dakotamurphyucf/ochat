(** Atomic, checksummed persistence snapshots. *)

type t =
  { schema_version : int
  ; transaction_sequence : int64
  ; transaction_hash : string option
  ; event_sequence : int64
  ; created_at : Agent_protocol.Timestamp.t
  ; prompt_artifact : string
  ; workspace_identity : string
  ; payload : string
  }

type installed =
  { filename : string
  ; snapshot : t
  }

(** [install] writes, rereads, validates, and atomically activates [snapshot]. *)
val install
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> t
  -> (installed, Store_error.t) result

(** [load_current] reads [CURRENT]. An incomplete newest snapshot falls back
    to the newest older valid snapshot. Complete corruption fails closed. *)
val load_current
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> (installed option, Store_error.t) result

(** [read_file] validates a specifically named snapshot. *)
val read_file
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> filename:string
  -> (installed, Store_error.t) result

(** Decode already bounded file bytes, checking framing and payload. Callers that
    use their own reader must also validate the filename and decoded session state. *)
val decode_file : max_payload_length:int -> string -> (t, Store_error.t) result

(** [prune_older ~env ~directory ~keep] retains the newest [keep] snapshot
    files and removes older checkpoints. [keep] must be positive so callers
    can preserve a validated fallback checkpoint. *)
val prune_older
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> keep:int
  -> (int, Store_error.t) result

(** [retention_floor ~env ~directory ~max_payload_length] validates the oldest
    retained checkpoint and returns its journal anchor. Keep this anchor and
    every later transaction so incomplete-current fallback remains recoverable. *)
val retention_floor
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> (int64, Store_error.t) result
