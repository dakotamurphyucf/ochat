open Core

(** Immutable, bounded source bytes for generated ChatMD. Creation never reads
    files, fetches URLs or preprocesses source. It is not an execution grant. *)
type limits =
  { max_source_bytes : int
  ; max_bundle_bytes : int
  ; max_files : int
  }

val default_limits : limits

type t

val create
  :  ?limits:limits
  -> root_file:string
  -> sources:(string * string) list
  -> unit
  -> (t, string) result

val root_file : t -> string
val sources : t -> (string * string) list
val limits : t -> limits
val fingerprint : t -> string
val loader : t -> root:Eio.Fs.dir_ty Eio.Path.t -> Source_loader.t
