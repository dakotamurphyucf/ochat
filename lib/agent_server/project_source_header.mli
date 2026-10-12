open Core

(** Bounded read of one trusted stable administrative credential input. Paths are
    already admitted/resolved; this performs no ambient credential lookup. *)
type error =
  | Filesystem
  | Too_large
[@@deriving equal, sexp_of]

(** Uses one fixed max_bytes+1 buffer before reading. Rejects excess without
    retaining secret contents in error. No whole-file load or stat-size trust.
    Unexpected/checkpoint exceptions preserve identity; only Eio.Io is mapped.
    The owner keeps the file/ancestors stable; this is not source-member authority
    or a descriptor-confinement claim. *)
val read
  :  Eio.Fs.dir_ty Eio.Path.t
  -> path:string
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> (string, error) Result.t
