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
val max_payload_length : t -> int

(** Read only the initial segment's first frame under the supplied shared reader
    allowance. [reader] is rooted at this caller's actual owned journal directory;
    caller excludes mutation through the same Handle/lifecycle owner. Parent and
    file links/special files reject. Missing/short first frame returns [None],
    while malformed complete headers/checksums fail closed. Admit declared frame
    length before payload allocation and debit actual bounded read chunks.
    No open_existing/current-segment scan, repair or actor construction occurs.
    Original checkpoint exceptions propagate outside expected IO handlers. *)
val initial_entry
  :  Retention_reader.t
  -> env:Eio_unix.Stdenv.base
  -> max_payload_length:int
  -> checkpoint:(unit -> unit)
  -> (entry option, Store_error.t) result

module Initial_creation : sig
  type journal = t

  (** Observed initial journal facts for one owned directory/first frame. This
      grants no source/runtime/receipt authority and has no public constructor.
      The caller retains that exact Handle/lifecycle exclusion through repair. *)
  type t

  (** Bounded Pending-only proof: exactly one complete flags0 initial transaction
      and no later transaction. Accept only the actual append-threshold rotation
      phases: initial CURRENT with absent/partial/exact flags1 sealed suffix;
      optional empty immediate successor after complete seal; or successor CURRENT
      with that same complete seal and empty successor. Partial seal bytes must
      match the existing Frame encoder exactly. Other members/tails reject.
      Existing Retention_reader streams directory enumeration and charges all
      reads against one caller aggregate allowance. No repair occurs here. *)
  val inspect
    :  Retention_reader.t
    -> env:Eio_unix.Stdenv.base
    -> max_payload_length:int
    -> checkpoint:(unit -> unit)
    -> (t option, Store_error.t) result

  val entry : t -> entry

  (** After authentic seed Created/receipt admission, recheck the exact observation
      under the same owner, then finish only the actual interrupted seal/rotation
      through existing truncation/exclusive-create/CURRENT publication owners.
      Return the actual opened Journal, without actor/source initialization.
      Invalid limits reject before mutation. A cancellation or uncertain write may
      leave a later valid phase; re-inspect on retry, never retry creation.
      The proof cannot be redirected to another directory. Checkpoint exceptions
      and unexpected failures retain their identity/backtrace. *)
  val open_journal
    :  t
    -> max_segment_bytes:int64
    -> max_segment_frames:int
    -> checkpoint:(unit -> unit)
    -> (journal, Store_error.t) result
end

(** [append] frames and appends one payload, flushing according to [durability]. *)
val append
  :  t
  -> durability:Journal_segment.durability
  -> flags:int
  -> payload:string
  -> (append_result, Store_error.t) result

(** [rotate] seals the current segment and atomically installs a new [CURRENT]. *)
val rotate : t -> terminal_payload:string -> (unit, Store_error.t) result

(** Pure feasibility check before checkpoint pruning. Empty current segments
    need no rotation. Nonempty segments require a terminal frame within the
    configured payload limit and an available successor ID. Serialize with
    journal writers through the later [seal_checkpoint] call. *)
val validate_seal_checkpoint : t -> (unit, Store_error.t) Result.t

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
