open! Core

(** Private finite facts for the actual initial append/rotation owner. The caller
    retains one Store Handle/lifecycle exclusion; no native authority is created. *)
type t

val initial_entry
  :  Retention_reader.t
  -> env:Eio_unix.Stdenv.base
  -> max_payload_length:int
  -> checkpoint:(unit -> unit)
  -> (Journal_segment.entry option, Store_error.t) result

val inspect
  :  Retention_reader.t
  -> env:Eio_unix.Stdenv.base
  -> max_payload_length:int
  -> checkpoint:(unit -> unit)
  -> (t option, Store_error.t) result

val entry : t -> Journal_segment.entry
val env : t -> Eio_unix.Stdenv.base
val directory : t -> string
val max_payload_length : t -> int

(** Shared exact existing CURRENT emitter. *)
val write_current
  :  env:Eio_unix.Stdenv.base
  -> string
  -> Journal_segment.Id.t
  -> (unit, Store_error.t) result

(** Recheck the exact charged observation, then complete only its admitted native
    append/rotation phase. Caller has already admitted actual seed receipt proof.
    No checkpoint follows successful publication; cancellation may leave an
    intermediate phase, re-inspected on retry. *)
val repair
  :  t
  -> checkpoint:(unit -> unit)
  -> (Journal_segment.t * Journal_segment.scan, Store_error.t) result
