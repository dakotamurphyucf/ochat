(** Runtime discovery of custom TextMate grammars. *)

open Core

module Source : sig
  type t

  (** [create ~name json] validates and snapshots one custom grammar source. *)
  val create : name:string -> Jsonaf.t -> t Or_error.t

  val name : t -> string
  val json : t -> Jsonaf.t
end

module Loaded_source : sig
  type t

  val name : t -> string
  val contents : t -> string
end

(** [load_explicit_sources ~fs ~cwd files] snapshots explicit files in order.
    The first failure is authoritative. *)
val load_explicit_sources
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> string list
  -> Source.t list Or_error.t

(** [load_discovered_sources ~fs ~cwd ~warn ()] snapshots valid discovered
    files in deterministic discovery order. Failures are warnings. *)
val load_discovered_sources
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> warn:(string -> unit)
  -> unit
  -> Source.t list

(** [read_discovered_sources ~fs ~cwd ~warn ()] reads discovered grammar
    files without parsing or validating them. *)
val read_discovered_sources
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> warn:(string -> unit)
  -> unit
  -> Loaded_source.t list

(** [parse_discovered_sources loaded] parses and validates loaded grammar
    contents. The returned warnings describe rejected sources. *)
val parse_discovered_sources : Loaded_source.t list -> Source.t list * string list

(** [load ~fs ~cwd ~registry ~explicit_files ~warn ()] loads explicit and
    automatically discovered custom grammar files in one synchronous pass.

    Files in [explicit_files] are loaded first in list order. A failure in an
    explicit file returns [Error _].

    The function then scans directories from [OCHAT_GRAMMAR_DIR], in
    colon-separated order, followed by [$XDG_CONFIG_HOME/ochat/grammars] or
    [$HOME/.config/ochat/grammars]. Each directory is scanned non-recursively
    for regular files whose names end in [.json], in lexical order.

    Failures while scanning or loading automatically discovered files are
    reported through [warn] and do not stop startup. A missing default
    directory is ignored. Relative paths are resolved against [cwd]. *)
val load
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> registry:Highlight_tm_loader.registry
  -> explicit_files:string list
  -> warn:(string -> unit)
  -> unit
  -> unit Or_error.t

(** [load_explicit ~fs ~cwd ~registry files] loads explicitly requested
    grammars in order. Failures are authoritative. *)
val load_explicit
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> registry:Highlight_tm_loader.registry
  -> string list
  -> unit Or_error.t

(** [load_discovered ~fs ~cwd ~registry ~warn ()] scans environment and
    default grammar directories. Failures are reported through [warn]. *)
val load_discovered
  :  fs:Eio.Fs.dir_ty Eio.Path.t
  -> cwd:Eio.Fs.dir_ty Eio.Path.t
  -> registry:Highlight_tm_loader.registry
  -> warn:(string -> unit)
  -> unit
  -> unit
