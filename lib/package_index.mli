(** Package-level semantic search index using OpenAI embeddings.

    This module keeps a small vector store mapping OPAM package names to the
    embedding of a short text blurb describing the package.  It is intended to
    be used as a *coarse* first-stage filter when searching a large ODoc index:
    we score the similarity between the user query and each package blurb and
    keep only the top packages for the more expensive document-level search.

    The index is
    • built once from a list of [(package, blurb)] pairs obtained when the
      documentation is generated,
    • persisted to disk in binary protocol format, and
    • queried at run time entirely in-memory.

    All heavy lifting is delegated to the OpenAI embeddings end-point and the
    Owl library for basic linear-algebra operations.  The vectors are
    L₂-normalised so that the dot product is equal to cosine similarity.

    The public interface purposefully exposes only the operations needed by the
    rest of the code-base; callers are not expected to inspect or mutate the
    vectors directly. *)

module Entry : sig
  (** A single index entry.  End-users usually do not manipulate values of this
      type directly – it is exposed mainly for serialisation purposes. *)
  type t =
    { pkg : string (** Name of the OPAM package. *)
    ; vector : float array (** Normalised embedding of the package blurb. *)
    }
end

(** The in-memory index.  Each element is an {!Entry.t}; the array is kept
    sorted in build order (which is irrelevant for queries). *)
type t = Entry.t array

(** A convenient alias for an Eio file-system path that is already known to be
    a directory. *)
type path = Eio.Fs.dir_ty Eio.Path.t

(** {1 Building} *)

(** [build ~net ~descriptions] contacts the OpenAI embeddings API and builds an
    index from [descriptions].  Each pair [(pkg, blurb)] should contain an OPAM
    package name and a short description (the first paragraph of its README or
    documentation usually works well).

    The function blocks the calling fibre while the HTTP request is made and
    therefore must be run inside an Eio event loop.  The returned vectors are
    L₂-normalised. *)
val build : net:'a Eio.Net.t -> descriptions:(string * string) list -> t

(** {1 Query} *)

(** [query t ~embedding ~k] returns at most [k] package names ranked by
    decreasing cosine similarity between [embedding] and the vectors stored in
    [t].

    The [embedding] MUST already be L₂-normalised.

    The result list contains package names only – duplicates are not possible. *)
val query : t -> embedding:float array -> k:int -> string list

(** {1 Persistence} *)

(** [save ~dir t] writes [t] to [dir/package_index.binio] using
    {!Bin_prot_utils_eio}.  The file is created or truncated. *)
val save : dir:path -> t -> unit

(** [load ~dir] attempts to read [dir/package_index.binio].  It returns [Some
    idx] on success or [None] if the file does not exist or is unreadable. *)
val load : dir:path -> t option

(** [build_and_save ~net ~descriptions ~dir] is a convenience wrapper that
    simply calls {!build} and then {!save}.  The freshly built index is also
    returned to the caller. *)
val build_and_save
  :  net:'a Eio.Net.t
  -> descriptions:(string * string) list
  -> dir:path
  -> t
