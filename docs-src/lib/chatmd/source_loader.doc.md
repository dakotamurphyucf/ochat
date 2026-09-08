# source_loader

Host-controlled source loading and provenance. Daemon pinned sources use the captured artifact closure; no arbitrary external filesystem snapshot is implied.

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

(** Resolve a relative extension source without lexical escape, even with the
    legacy filesystem loader. Captured loaders still forbid unknown edges.
    This is not a symlink sandbox; captured artifact verification remains required. *)
val resolve_within_root : t -> base:source -> reference:string -> (source, string) result

(** Read at most [max_bytes] (0..8 MiB), then notify the capture observer.
    Oversized reads never publish a partial captured dependency. *)
val read_bounded : max_bytes:int -> t -> source -> (string, string) result
```
