(** One append-only journal segment. *)

module Id : sig
  type t [@@deriving compare, equal, sexp]

  val first : t
  val next : t -> (t, Store_error.t) result
  val of_int64 : int64 -> (t, Store_error.t) result
  val to_int64 : t -> int64
  val filename : t -> string
  val of_filename : string -> (t, Store_error.t) result
end

type t

type durability =
  | Buffered
  | Flush
[@@deriving compare, equal, sexp]

type entry =
  { offset : int64
  ; next_offset : int64
  ; frame : Frame.t
  }

type scan =
  { entries : entry list
  ; valid_length : int64
  ; crash_tail : bool
  }

val id : t -> Id.t
val path : t -> string

(** [create_exclusive] creates a new empty segment with mode [0o600]. *)
val create_exclusive
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> id:Id.t
  -> (t, Store_error.t) result

(** [open_existing] validates an existing regular segment file. *)
val open_existing
  :  env:Eio_unix.Stdenv.base
  -> directory:string
  -> id:Id.t
  -> (t, Store_error.t) result

(** [append] writes one already encoded frame and returns its byte range. *)
val append
  :  env:Eio_unix.Stdenv.base
  -> durability:durability
  -> t
  -> frame:string
  -> (int64 * int64, Store_error.t) result

(** [scan] validates every complete frame and identifies a short final frame. *)
val scan
  :  env:Eio_unix.Stdenv.base
  -> max_payload_length:int
  -> t
  -> (scan, Store_error.t) result

(** [truncate_crash_tail] truncates only to the validated length returned by
    [scan]. It rejects scans that do not actually contain a crash tail. *)
val truncate_crash_tail
  :  env:Eio_unix.Stdenv.base
  -> t
  -> scan
  -> (unit, Store_error.t) result
