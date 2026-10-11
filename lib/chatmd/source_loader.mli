open! Core

(** Eio-backed source resolution for ChatMD roots, imports, and referenced
    script files. *)

type t
type source

(** [filesystem ~root] preserves ordinary filesystem traversal behavior. *)
val filesystem : root:Eio.Fs.dir_ty Eio.Path.t -> t

(** [confined_filesystem ~root] rejects absolute references and lexical parent
    traversal outside [root]. This is not an OS sandbox or a symlink boundary;
    artifact hosts verify their tree separately and use [captured_filesystem]. *)
val confined_filesystem : root:Eio.Fs.dir_ty Eio.Path.t -> t

(** [captured_filesystem ~root ~sources] resolves source paths beneath [root]
    but reads only supplied captured bytes. Unknown edges, absolute imports and
    parent traversal outside the root fail closed. No filesystem fallback occurs.
    [sources] must contain unique normalized root-relative paths. Absolute agent
    declarations remain explicit external dependencies, as with [agent_reference]. *)
val captured_filesystem
  :  root:Eio.Fs.dir_ty Eio.Path.t
  -> sources:(string * string) list
  -> t

(** Captured-only loader for generated bundles. Rejects portable-path hazards
    and absolute/missing local agent sources as well as external import/script
    sources. Callers should use Chatmd_source_bundle to validate map keys and
    size limits first. No filesystem fallback or materialization occurs. *)
val generated_filesystem
  :  root:Eio.Fs.dir_ty Eio.Path.t
  -> sources:(string * string) list
  -> t

(** Add a per-read byte ceiling without replacing an existing ceiling. The
    callback returns0..8MiB and is evaluated before any read; installing multiple
    bounds takes their minimum. Observer/agent wrappers preserve it. Callback
    exceptions propagate unchanged. Both filesystem and captured reads enforce
    it; captured mode still never falls back to disk. Legacy loaders have no
    implicit bound. Hosts can supply their current remaining aggregate allowance. *)
val with_read_limit : t -> max_bytes:(source -> int) -> t

(** [with_observer t ~f] invokes [f] after each successful source read. *)
val with_observer : t -> f:(source -> string -> unit) -> t

(** [with_agent_observer t ~f] observes statically declared relative local agent
    references. Artifact builders use it to capture a bounded source closure. *)
val with_agent_observer : t -> f:(source -> unit) -> t

(** [agent_reference t ~base ~reference] resolves relative agent references at
    their declaration, without a process-cwd fallback. Absolute references remain
    explicit external dependencies and are not captured. *)
val agent_reference : t -> base:source -> reference:string -> (string, string) result

val root : t -> file:string -> (source, string) result
val resolve : t -> base:source -> reference:string -> (source, string) result
val read : t -> source -> (string, string) result
val file_name : source -> string
val relative_path : source -> string
val materialized_dir : source -> Eio.Fs.dir_ty Eio.Path.t
val root_dir : t -> Eio.Fs.dir_ty Eio.Path.t

(** Whether reads are restricted to an immutable supplied inventory, with no
    filesystem fallback. This is a mode projection, never an execution grant. *)
val is_captured : t -> bool

(** Pure confined member resolution from an admitted logical declaring-file path.
    The reference byte ceiling must lie in [0,1024]; admission precedes scanning
    or concatenation. Reject absolute paths, URL/colon/backslash/control hazards
    and root escapes. Uses the same lexical normalization as captured loading;
    no native directory, filesystem I/O or display source context is authority.
    Individual admitted path primitives scan at most2049 bytes; checkpoints
    bracket this bounded work and propagate unchanged. *)
val resolve_member
  :  Source.Portable_path.t
  -> reference:string
  -> max_reference_bytes:int
  -> checkpoint:(unit -> unit)
  -> Source.Portable_path.t Core.Or_error.t

(** Resolve a relative extension source without lexical escape, even with the
    legacy filesystem loader. Captured loaders still forbid unknown edges.
    This is not a symlink sandbox; captured artifact verification remains required. *)
val resolve_within_root : t -> base:source -> reference:string -> (source, string) result

(** Read at most [max_bytes] (0..8 MiB), then notify the capture observer.
    Oversized reads never publish a partial captured dependency. *)
val read_bounded : max_bytes:int -> t -> source -> (string, string) result
