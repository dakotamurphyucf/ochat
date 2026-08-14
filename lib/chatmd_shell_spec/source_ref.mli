open! Core

(** Source provenance retained by parsed shell declarations. *)

type position =
  { offset : int
  ; line : int
  ; column : int
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

type t =
  { file : string
  ; source_dir : string
  ; prompt_dir : string
  ; namespace : string option
  ; start_pos : position
  ; end_pos : position
  ; source_sha256 : string
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** [create ~file ~source_dir ~prompt_dir ~namespace ~start_pos ~end_pos ~source]
    records provenance and computes the SHA-256 digest of [source]. *)
val create
  :  file:string
  -> source_dir:string
  -> prompt_dir:string
  -> namespace:string option
  -> start_pos:position
  -> end_pos:position
  -> source:string
  -> t

(** [digest source] returns the lowercase SHA-256 digest of [source]. *)
val digest : string -> string

(** [qualify t id] qualifies [id] with [t.namespace] unless [id] is already
    qualified. *)
val qualify : t -> string -> string
