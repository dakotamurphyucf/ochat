open Core

(** Catalogue of Markdown indexes.  Each entry stores the index name,
    a free-text description and the centroid vector of its snippets.
    The catalogue lives at [md_index_catalog.binio] inside the common
    [vector_db_root] directory (default [.md_index]). *)

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

(** [load ~dir] tries to read the catalogue from disk.  Returns [None]
    if the file is missing or corrupted. *)
val load : dir:path -> t option

(** [save ~dir cat] writes the whole catalogue to disk, replacing any
    previous file. *)
val save : dir:path -> t -> unit

(** [add_or_update ~dir ~name ~description ~vector] adds a new entry or
    replaces an existing one with [name].  Vectors are L2-normalised
    before persistence. *)
val add_or_update
  :  dir:path
  -> name:string
  -> description:string
  -> vector:float array
  -> unit
