open! Core

module Diagnostic : sig
  type t =
    { code : string
    ; message : string
    ; source : string option
    }
  [@@deriving compare, equal, sexp]
end

module Capture : sig
  (** Immutable first-admission parser results and authored inventory. No loader,
      provider, filesystem capability or callback is retained. Queued Roots are
      keyed by the existing loader-relative source identity. This is the parser
      owner seam for durable custody; it does not itself persist associations. *)
  type t

  val root : t -> Prompt.Chat_markdown.Parsed.t
  val agents : t -> (string * Prompt.Chat_markdown.Parsed.t) list

  (** Raw dependencies, excluding the separately retained root authored input. *)
  val sources : t -> (string * string) list

  (** Observed retained authored bytes for the root and distinct dependencies.
      This is counted by the bounded capture walker, not recomputed from a
      caller's selected path. *)
  val source_bytes : t -> int

  val shell_manifest_sha256 : t -> string
end

(** Read a bounded authored closure and preprocess each distinct queued Root
    exactly once. Imports remain raw. Limits apply to each Root including imports;
    the existing 256-file limit also applies. Jointly retained authored bytes plus
    distinct transformed effective payloads must fit8MiB; exact-equal effective
    and authored inputs share accounting. New file reads use remaining allowance;
    a preparation may temporarily retain one bounded effective result before its
    aggregate admission. These payload limits do not bound the callback's internal
    allocations. Expected shell
    inspection failures return diagnostics. Parser, checkpoint and preprocessing
    exceptions propagate unchanged, including cancellation. *)
val capture
  :  env:Eio_unix.Stdenv.base
  -> preprocess:(string -> string)
  -> limits:Prompt.Chat_markdown.Limits.t
  -> checkpoint:(unit -> unit)
  -> Prompt_definition.t
  -> (Capture.t, Diagnostic.t list) result

(** The preparation hook receives only immutable Capture and runs once before
    semantic revision identity/publication. Its closure may own explicit host
    acquisition capabilities; it is never retained. Its returned custody must
    match the original actual capture. Without a hook, parser-only custody retains
    every effective Root and raw Import, with zero prepared programs.
    [prepare_content] likewise runs once on the SAME original Capture before
    identity/publication. Its original immutable limits continue through custody
    validation. Without it, only all-Dynamic document slots are admitted and the
    existing absent-content identity is preserved; captured/builtin slots reject.
    [build] captures, inspects, persists, and replays one prompt revision.
    Fresh replay uses the retained effective Root with no second preprocessing.
    Content validation and final artifact-rebased literal projection share the
    original work ceiling and finish before installation; the installed tree is
    verified before attaching that same parser/projection. Source directories are
    rebased to the final artifact tree, never retained authoring directories.
    Durable restart association persistence is owned by the parser-input custody
    format; legacy artifacts still use the existing restoration path.
    Capture imports, scripts, extension schemas and static relative nested-agent sources at their
    declaration directories. Deduplicate cycles; reject closures exceeding 256
    total source files (including the root) or 8 MiB of captured source bytes.
    A dependency returning different bytes during one capture fails closed.
    Explicit absolute agent references remain external dependencies.
    It never authorizes a manifest or starts executable runtime resources. *)
val build
  :  ?prepare_programs:
       (Capture.t -> (Chatml_prompt_programs.t, Diagnostic.t list) Result.t)
  -> ?prepare_content:
       (Capture.t -> (Chatmd_content_capture.t, Diagnostic.t list) Result.t)
  -> env:Eio_unix.Stdenv.base
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> created_at:Agent_protocol.Timestamp.t
  -> Prompt_definition.t
  -> (Prompt_revision.t, Diagnostic.t list) result

(** [restore] verifies and reparses an already installed pinned revision.
    A content-bearing artifact requires explicit [content_limits] host work and
    structural policy; absent policy rejects before history without fallback.
    Stored metadata never chooses policy. An authentic owned parser requesting
    captured/builtin slots rejects absent content custody; all-Dynamic stays valid.
    Legacy artifacts without owned parser metadata retain their existing path. They
    do not acquire the new captured-document guarantees retroactively. *)
val restore
  :  ?content_limits:Chatmd_prompt_content.Limits.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> Prompt_definition.t
  -> Agent_protocol.Id.Prompt_revision.t
  -> (Prompt_revision.t, Diagnostic.t list) result

(** Reparse a host-selected artifact against an existing captured tree. Verifies
    the complete tree/inventory and parser/runtime contract before parsing through
    the captured source loader. Does not install an artifact, consult live source
    files, instantiate tools or initialize scripts. Intended for retained authored
    subdocuments whose source root differs from the containing revision's root.
    Content-bearing artifacts require explicit [content_limits], with no original
    file/catalog fallback or policy inferred from stored metadata.
    The host must supply the admitted source and original definition policy; this
    is source preparation, not an execution or catalog authorization. The optional
    borrowed checkpoint defaults to Eio cancellation; its original exception and
    backtrace are never converted into parser diagnostics. *)
val reparse
  :  ?checkpoint:(unit -> unit)
  -> ?content_limits:Chatmd_prompt_content.Limits.t
  -> definition:Prompt_definition.t
  -> artifact:Agent_store.Prompt_artifact_store.Artifact.t
  -> materialized_tree:Eio.Fs.dir_ty Eio.Path.t
  -> unit
  -> (Prompt_revision.t, Diagnostic.t list) result

(** Same bounded parser/capture walker as [capture], supplied with an actual
    original-root loader. Optional [max_total_source_bytes] only narrows the
    existing 8 MiB aggregate bound: root and distinct dependency reads use its
    remaining allowance before each read, and the reported [Capture.source_bytes]
    is the actual admitted authored total. Effective transformed inputs retain
    their existing separate bounded accounting. [env] only supplies existing
    shell inspection; it never becomes a fallback source root. No
    initializer/artifact publication. *)
val capture_with_loader
  :  loader:Source_loader.t
  -> entry:Source.Portable_path.t
  -> ?max_total_source_bytes:int
  -> env:Eio_unix.Stdenv.base
  -> preprocess:(string -> string)
  -> limits:Prompt.Chat_markdown.Limits.t
  -> checkpoint:(unit -> unit)
  -> Prompt_definition.t
  -> (Capture.t, Diagnostic.t list) Result.t

(** Publish actual immutable capture after source/program preparation. Receipt
    must match original definition and root member. No original file/stat reread;
    canonical-source metadata derives solely from admitted selection receipt.
    Hook exceptions propagate unchanged; captured replay retains original wire. *)
val build_captured
  :  ?prepare_programs:
       (Capture.t -> (Chatml_prompt_programs.t, Diagnostic.t list) Result.t)
  -> ?prepare_content:
       (Capture.t -> (Chatmd_content_capture.t, Diagnostic.t list) Result.t)
  -> capture:Capture.t
  -> source_receipt:Agent_store.Prompt_source_receipt.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> created_at:Agent_protocol.Timestamp.t
  -> Prompt_definition.t
  -> (Prompt_revision.t, Diagnostic.t list) Result.t

(** Publish the exact selected portable member after original-source capture.
    Root path, Entry receipt and original Definition must agree BEFORE hooks.
    Shares [build_captured]'s complete semantic custody, final-tree replay,
    installation/verification and error policy. Original [build_captured] still
    requires its original basename. Builder consumes its own immutable Capture
    and has no Source_owner dependency; metadata cannot itself prove native reads.
    The optional checkpoint defaults to Eio cancellation and is borrowed through
    publication admission, custody/hash loops, final replay and checks before
    installation/delivery. It polls the caller's same deadline/work allowance;
    it does not preempt native primitives. Its exceptions retain identity and raw
    backtrace through the existing parser diagnostic boundary.
    Original operator receipt/canonical source is preserved independently of the
    selected Root member. Does not select executable roles or reserve a child. *)
val build_captured_entry
  :  ?checkpoint:(unit -> unit)
  -> ?prepare_programs:
       (Capture.t -> (Chatml_prompt_programs.t, Diagnostic.t list) Result.t)
  -> ?prepare_content:
       (Capture.t -> (Chatmd_content_capture.t, Diagnostic.t list) Result.t)
  -> entry:Prompt_source_entry.t
  -> capture:Capture.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> created_at:Agent_protocol.Timestamp.t
  -> unit
  -> (Prompt_revision.t, Diagnostic.t list) Result.t

(** Actual existing bounded capture profile used by Catalog and fresh child
    admission: 8 MiB input, 1024 parse calls, 100000 tokens, depth 128.
    The immutable limits grant no filesystem or source authority. *)
val capture_limits : unit -> Prompt.Chat_markdown.Limits.t
