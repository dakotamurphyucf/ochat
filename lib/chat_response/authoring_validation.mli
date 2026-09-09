open Core

(** Readonly validation of inline script candidates. This service only selects
    existing capability bindings and invokes the bounded static compiler. It does
    not construct/evaluate a runtime, read source files, invoke tools, or create
    sessions. Generated ChatMD bundles use a separate later admission path. *)
type target =
  | One_off_script
  | Standalone_tool
  | Moderator
[@@deriving sexp, equal]

type moderator_surface =
  | Ordinary
  | Delegated
[@@deriving sexp, equal]

type host

(** The owning host supplies the installed runtime identity, available targets,
    actual moderator surface and compiler ceilings. None of these are accepted
    from submitted JSON. Identity must change with the target runtime build or
    execution contract. Guidance retrieval never changes this host or its tools. *)
val create_host
  :  runtime_identity:string
  -> targets:target list
  -> moderator_surface:moderator_surface
  -> compilation:Chatml_compilation.limits
  -> (host, string) result

(** Binds helper registration/cache identity to every supported target contract,
    the installed runtime identity and compiler policy. Changes require a fresh
    native capability binding as well as fresh validation. *)
val host_fingerprint : host -> string

type diagnostic =
  { diagnostic : Chatmd_shell_spec.Diagnostic.t
  ; topic_ids : string list
  }
[@@deriving sexp]

type report = private
  { target : target option
  ; source : Chatmd_shell_spec.Source_ref.t option
  ; validation_id : string option
  ; compiler_contract : string option
  ; capability_fingerprint : string option
  ; runtime_identity : string
  ; diagnostics : diagnostic list
  ; checked : string list
  ; deferred : string list
  }
[@@deriving sexp]

val parameters : Jsonaf.t

(** Request version 1 accepts target, source and exact selected tool names.
    Standalone candidates additionally require input_schema/output_schema.
    Unknown/duplicate fields, unavailable targets, oversized sources and invalid
    schemas fail before compilation. Every compile includes static entrypoint
    checks. Successful validation explicitly defers initializer, dynamic tool,
    state and runtime schema/permission checks.

    [capabilities] must be the caller/target's actual effective ceiling. A report
    and its identity grant no authority, even after successful validation. Run
    tools must independently re-admit current source and capabilities. Identities
    bind source, target, schemas, selected bindings, host surface, runtime build
    and compiler policy. Caller cancellation propagates through joined compiler
    cleanup. Responses include hashes/spans, never the full source or registry. *)
val validate
  :  env:Eio_unix.Stdenv.base
  -> host:host
  -> capabilities:Tool_capability.t
  -> Jsonaf.t
  -> report

val valid : report -> bool
val to_json : report -> Jsonaf.t

(** Stable topic dependencies for A01's shared corpus/coverage manifest. These
    are metadata, not a substitute for reference content or context insertion. *)
val topics : (string * string) list

val help : target -> Chatmd_shell_spec.Authoring_metadata.help
val helper_metadata : Chatmd_shell_spec.Authoring_metadata.t
