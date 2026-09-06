(** Segmented append-only journal management. *)

type t

type entry =
  { segment_id : Journal_segment.Id.t
  ; offset : int64
  ; next_offset : int64
  ; frame : Frame.t
  }

type scan =
  { entries : entry list
  ; current_segment : Journal_segment.Id.t
  ; crash_tail : (Journal_segment.Id.t * int64) option
  }

type append_result =
  { segment_id : Journal_segment.Id.t
  ; offset : int64
  ; next_offset : int64
  ; checksum_hex : string
  }

(** [create] creates an empty journal at an absolute directory path. *)
val create
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> max_segment_bytes:int64
  -> max_segment_frames:int
  -> (t, Store_error.t) result

(** [open_existing] opens the segment named by [CURRENT]. *)
val open_existing
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> max_payload_length:int
  -> max_segment_bytes:int64
  -> max_segment_frames:int
  -> (t, Store_error.t) result

val current_segment : t -> Journal_segment.Id.t

(** [append] frames and appends one payload, flushing according to [durability]. *)
val append
  :  t
  -> durability:Journal_segment.durability
  -> flags:int
  -> payload:string
  -> (append_result, Store_error.t) result

(** [rotate] seals the current segment and atomically installs a new [CURRENT]. *)
val rotate : t -> terminal_payload:string -> (unit, Store_error.t) result

(** [seal_checkpoint t] seals a nonempty segment after durable snapshot
    installation, allowing the next retention pass to reclaim covered deltas.
    Empty segments are unchanged. Serialize with journal writers. *)
val seal_checkpoint : t -> (unit, Store_error.t) result

(** [scan] validates every referenced segment in numeric order. Only the
    current segment may contain an incomplete final frame. *)
val scan : t -> (scan, Store_error.t) result

(** [repair_current_tail] removes a validated incomplete final frame. *)
val repair_current_tail : t -> scan -> (unit, Store_error.t) result

(** [prune_before_transaction t ~transaction_sequence] removes complete
    sealed segments strictly before the segment containing the named
    transaction. It retains the containing segment as the snapshot hash-chain
    anchor and never removes the current segment. *)
val prune_before_transaction
  :  t
  -> transaction_sequence:int64
  -> (int, Store_error.t) result
