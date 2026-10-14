# source_loader

Host-controlled source loading and provenance. Captured loaders read only their immutable inventory. The retained loader borrows a separately admitted original directory owner; its Eio root is a diagnostic label, with no ambient filesystem fallback.

See [agent-core embedding](../../agent-server/embedding.md),
[session/history behavior](../../agent-server/sessions-and-workspaces.md),
and [protocol synchronization](../../agent-server/protocol.md).

## Public contract

[Interface](../../../lib/chatmd/source_loader.mli) · [implementation](../../../lib/chatmd/source_loader.ml)

The following excerpt is the current callable contract. Eio switches own active
resources; preserve typed errors/cancellation and the host's authorization
boundary rather than bypassing the actor from a presentation adapter.

```ocaml
open! Core

(** Eio-backed source resolution for ChatMD roots, imports, and referenced
    script files. *)

type t
type source
type loader = t

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

module Generated_error : sig
  type t =
    | Invalid_reference
    | Source_unavailable
    | Limit_exceeded

  (** Recognizes only the private checked-generated-loader failure constructor.
      Arbitrary caller/checkpoint exceptions are not classified by message. *)
  val of_exn : exn -> t option
end

(** Same captured-only generated loader, with typed failures at its own
    resolution/read boundary. Only the checked generated parser uses this mode;
    existing Result-returning loader APIs retain their historical behavior. *)
val generated_filesystem_checked
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
    explicit external dependencies and are not captured.

    [on_member], when supplied, receives the exact loader-relative key from a
    successful relative-local resolution, before the existing agent observer.
    Absolute references do not have a queued member. It performs no extra read
    or resolution and conveys no native authority. Callback exceptions propagate
    unchanged. Legacy callers may omit it and retain existing return semantics. *)
val agent_reference
  :  ?on_member:(string -> unit)
  -> t
  -> base:source
  -> reference:string
  -> (string, string) result

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

(** Borrowed original-root backend. Labels/context come from [root]; every read
    uses the admitted retained [owner], descriptor-relative/no-follow. Reject
    portable member hazards before native work. No filesystem fallback. Caller
    checkpoints/observers propagate unchanged; expected native errors are loader
    failures. The host keeps this owner alive until all loader operations join. *)
val retained_filesystem
  :  root:Eio.Fs.dir_ty Eio.Path.t
  -> owner:Chatml_local_source.t
  -> checkpoint:(unit -> unit)
  -> t

module Generated_retained : sig
  module Limits : sig
    type t

    (** The same three ceilings later supplied to the generated source-bundle
        validator. Admission rejects values outside its supported hard caps;
        the loader enforces them before each new member read or retention.
        This source-loader-local type avoids a dependency on the bundle module. *)
    val create
      :  max_source_bytes:int
      -> max_bundle_bytes:int
      -> max_files:int
      -> t Or_error.t

    val max_source_bytes : t -> int
    val max_bundle_bytes : t -> int
    val max_files : t -> int
  end

  (** Borrowed checked-generated loader for one authorized document capture.
      The current editor root is supplied as exact immutable bytes and is
      never read from [owner]. Every other member resolves under the SAME
      generated portable-path rules as [generated_filesystem_checked] and is
      read descriptor-relative through the retained no-follow [owner]. The
      [root] Eio path is used only for parser display provenance and is never
      read by this loader. Absolute/escaping paths and external sources reject.
      Relative agent references are discovered by the same generated resolver,
      not by a parallel parser. *)
  type t

  val create
    :  root:Eio.Fs.dir_ty Eio.Path.t
    -> owner:Chatml_local_source.t
    -> root_file:Source.Portable_path.t
    -> root_contents:string
    -> limits:Limits.t
    -> before_import_read:(member:Source.Portable_path.t -> int)
    -> on_import_read:(member:Source.Portable_path.t -> contents:string -> unit)
    -> checkpoint:(unit -> unit)
    -> t Or_error.t

  (** Only the checked Generated parser borrows this loader, synchronously
      while the source-host owner still holds its joined root lease. It must
      not retain the loader or use it after that callback returns. The
      [before_import_read] runs before a NEW native member read and returns that
      read's remaining host byte maximum. The host owns one charged outer
      capture request; the generated parser separately charges at most1024
      individual reads, including cache hits.
      A negative value is typed [Limit_exceeded]. Root bytes count as the first
      bundle member, though they are never reread or charged as a host import.
      Before each new member read, the loader checks the file count, portable
      path/collision rules, remaining aggregate bundle bytes, per-source bound
      and caller's host bound, without first allocating a source copy. Exact
      successful bytes are cached on first read, including the root override;
      later reads of that member return those same bytes and still incur the
      generated parser's read/work charges, never a changed host observation.
      [on_import_read] runs only after an exact successful bounded native
      read. Callback/cancellation exceptions retain identity. *)
  val root_file : t -> string

  val limits : t -> Limits.t
  val loader : t -> loader

  (** Exact successfully observed root/import bytes, for checked bundle
      construction after the parser has finished. Calling this early grants
      no publication authority and cannot return bytes that were not read. *)
  val captured_sources : t -> (string * string) list
end
```
