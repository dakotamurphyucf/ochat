open Core

(** Persistent catalogue of Markdown indexes.

    Every entry links an {e index folder} – e.g. {!"docs"} – with a
    short description and the L2-normalised centroid embedding of all
    snippets stored in that index.  The data is serialised with
    {!Bin_prot} and saved as {!file:md_index_catalog.binio} in the
    root directory that also contains the individual index folders
    (defaults to {!".md_index"}).

    All public functions are non-blocking and must run within an Eio
    fibre.  Callers are expected to handle the case where the catalog
    file is missing or cannot be decoded. *)

module Entry : sig
  type t =
    { name : string
    ; description : string
    ; vector : float array
    }
  [@@deriving bin_io, sexp]
end

type t = Entry.t array [@@deriving bin_io, sexp]
type path = Eio.Fs.dir_ty Eio.Path.t

(** [load ~dir] reads {!file:md_index_catalog.binio} from [dir].

    Returns [None] when the file does not exist or cannot be parsed
    (e.g. due to version skew). *)
val load : dir:path -> t option

(** [save ~dir cat] truncates or creates
    {!file:md_index_catalog.binio} in [dir] and writes [cat] using
    {!Bin_prot}.  The operation replaces any previous file. *)
val save : dir:path -> t -> unit

(** [add_or_update ~dir ~name ~description ~vector] inserts a new entry
    or replaces the existing entry whose [name] matches.  The function
    L2-normalises [vector] before persisting so that later dot-product
    comparisons correspond to cosine similarity.*)
val add_or_update
  :  dir:path
  -> name:string
  -> description:string
  -> vector:float array
  -> unit
