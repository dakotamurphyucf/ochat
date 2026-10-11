open Core

(** host-local acquisition
    of one selected package. No source parsing,
    compiler call, initializer, dependency fetch, lock publication or runtime
    publication. Host composition admits roots and combines immutable catalogs.
    No process cwd/HOME/PATH/global-install search. *)

module Limits : sig
  type t

  (** Explicit host policy, not inferred from compiler policy. Counts and total
      bytes/steps are nonnegative; per-source bytes and component depth positive.
      Invalid/overflowing limits reject. No global defaults or silent cap mapping.
      [max_path_components] bounds root-relative member traversal, not recursive
      source discovery (which never occurs). *)
  val create
    :  max_directory_entries:int
    -> max_sources:int
    -> max_source_bytes:int
    -> max_total_source_bytes:int
    -> max_path_components:int
    -> max_work_steps:int
    -> t Or_error.t

  (** Literal configured ceilings, not consumed accounting. Host composition
      uses these to construct smaller remaining allowances without resetting
      aggregate limits or raising any backend ceiling. *)
  val directory_entries : t -> int

  val sources : t -> int
  val source_bytes : t -> int
  val total_source_bytes : t -> int
  val path_components : t -> int
  val work_steps : t -> int
end

module Accounting : sig
  type t

  (** All non-dot entries returned from declared directories, including irrelevant,
      excluded and overridden candidates. Repeated declared directory references
      are deduplicated before acquisition. *)
  val directory_entries : t -> int

  (** Unique final inventory including supplied exact declaration, selected entry,
      implementations and selected interfaces. Aliases/entry overlap count once. *)
  val source_count : t -> int

  val source_bytes : t -> int

  (** Conservatively reserved preparation work, not observed syscall counts or
      wall-clock time. A versioned reservation model charges directory opens/entry
      callbacks, component traversal, worst-case partial native reads, bounded
      hash/location scans and pure candidate/association ordering. Reserve known
      stage allowance via Result before entering each stage. Checkpoints are only
      cancellation/progress; never raise an ordinary admission error from one.
      Native reads may return one byte per operation: a 4096-byte chunk size does
      not imply ceil(bytes/4096) as a worst-case read-call bound. Model/wire notes
      and independent boundary fixtures accompany implementation. Arithmetic
      checks overflow. Native OS latency is not a deadline guarantee. *)
  val work_steps : t -> int
end

module Error : sig
  type limit =
    | Directory_entries
    | Source_count
    | Source_bytes
    | Total_source_bytes
    | Path_components
    | Work_steps
  [@@deriving equal, sexp_of]

  type problem =
    | Declaration of Chatml_project.Error.t
    | Filesystem of
        { failure : Chatml_local_source.Error.t
        ; member : Source.Portable_path.t option
        }
    | Catalog of Chatml_catalog.Error.t
    | Limit_exceeded of
        { limit : limit
        ; maximum : int
        }
    | Accounting_overflow
  [@@deriving sexp_of]

  (** Exact declaration-owned context, optional second context for a collision.
      Unread files have no fabricated source span. Details are finite tags and
      at most two paths/contexts; invalid-name prefix is at most 256 bytes. Native
      exception/backtrace text and directory inventories are not retained.
      Captured declaration retention belongs to the caller's capture admission. *)
  type t

  val problem : t -> problem
  val declared_at : t -> Chatml_project.Declared_at.t
  val related : t -> Chatml_project.Declared_at.t option
end

module Binding = Chatml_catalog.Binding

(** Immutable exact bytes and own-package scope intent. No filesystem path/FD,
    Eio switch/control, callback, mutable source cache or executable capability is
    retained. [sources] sorted by Source.Id; [bindings] sorted by exact name;
    [definitions] sorted by implementation identity, with aliases canonicalized.
    Successful result is not a Prepared_program or a validated dependency graph. *)
type t

(** [project] is already decoded from the exact declaration captured for this
    preparation attempt. Its captured Source.Id namespace is the effective host-
    selected namespace, including any explicitly admitted dependency override;
    authored [Project.namespace] remains the original declaration intent. Host composition
    owns reading/decoding a fresh declaration and selecting/rebinding its canonical
    identity. The declaration bytes are seeded into this inventory, not reread.

    [root] is an already open, host-admitted directory capability with empty Eio
    path, as returned by Eio.Path.open_dir; it is borrowed for this invocation.
    The caller retains its switch and must not close it during capture. Nonempty
    path roots reject: this API never resolves a source-supplied host locator. Local/absolute dependency locators are resolved by host composition,
    against their declaring project's admitted root, before another capture call.
    There is no dependency traversal in this function. [request] selects only this
    package's own environment. Exact code/interface file overrides use scope_for.

    Discovery is nonrecursive. Known explicit module names take precedence before
    conventional admission, even over multiple conventional candidates. Count all
    returned directory entries. Exclusions suppress discovery/implicit companions;
    explicit entry/module/interface selections still apply. Remaining conventional
    .chatml basenames derive by uppercasing their first ASCII byte and must satisfy
    lexer UIDENT; exact same-basename .chatmli is the optional companion. Orphan or
    ambiguous remaining candidates reject. Replaced pairs are not selected unless
    independently referenced. No punctuation rewriting or search-order precedence.

    Capture every available selected module, including modules unused by entry,
    plus the selected entry only, declaration and selected interfaces. Entry/catalog
    overlap and aliases share one Source.Id/Module_id and exact captured revision.
    Repeated observation of one ID with different bytes/association rejects;
    ordinary alias reuse consults this invocation's immutable capture table without
    rereading. File identities are not deduplicated by inode/content digest.

    Source.Portable_path validates members; one shared pure path collision helper
    covers ASCII-case and file/directory conflicts before result publication.
    No fake Prepared_program is built to borrow its full graph validator.

    Acquisition uses Chatml_local_source:
    with_root ~max_path_components owns one joined native acquisition bracket;
    Source_dir.Root maps to fold_directory ~directory:None and Relative path to
    Some path. Each declared directory receives the REMAINING aggregate entry
    allowance, not a reset per-directory cap. The fold's returned exact count
    charges all entries, including discarded candidates. Visits return ordinary
    Error.t for admission failure; no admission exception is raised by checkpoint.
    read ~max_bytes:min(per-source cap, remaining aggregate bytes) is used for
    each unique selected portable member. Native Missing is optional only for
    an implicit companion with operation Open_file; parent/root absence, explicit
    interfaces and all other native errors fail capture. Existing finite native
    operation/code payloads are retained directly with the selected member and
    declaration context; no second filesystem error taxonomy.

    No lstat/open race, native-path reopen, link following or unbounded read_dir
    fallback. Acquisition preserves its descriptor-object semantics across rename
    and best-effort before/after observations; no all-file atomic snapshot, private
    storage ownership/mode/ACL or hardlink restriction is invented here. Poll
    explicit Eio cancellation and caller checkpoint between pure stages as well
    as the acquisition library's bounded batch/read polls. Checkpoint exceptions
    propagate unchanged after joined owned-handle cleanup. No partial result or
    runtime is published; no callback is retained.

    The caller supplies remaining whole-attempt policy allowances to successive
    package calls. Complete association/scope/compiler admission remains with
    host composition. No dependency traversal or lock publication occurs here. *)
val capture
  :  Chatml_project.t
  -> root:[ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> request:Chatml_project.Selection.request
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Common validated pure artifact also consumed by the Git backend/composition. *)
val catalog : t -> Chatml_catalog.t

val project : t -> Chatml_project.t
val selection : t -> Chatml_project.Selection.t
val sources : t -> Chatml_source.t list
val entry : t -> Chatml_source.Id.t option
val definitions : t -> Chatml_prepared_program.Definition.t list
val bindings : t -> Binding.t list
val accounting : t -> Accounting.t

(** Exact captured source of an admitted implementation/interface/entry. No live
    filesystem fallback. Config-only source need not have an executable scope.
    Host composition resolves Scope_spec module references against dependency catalogs,
    validates requirements and supplies complete Prepared_program scopes/admission. *)
val find_source : t -> Chatml_source.Id.t -> Chatml_source.t option

val scope_for : t -> source:Chatml_source.Id.t -> Chatml_project.Scope_spec.t option

(** Caller supplies a live descriptor owner already rooted at the selected package.
    No Eio path/native filename is reconstructed or reopened. Capture limits must
    not exceed the owner's component ceiling; incompatible policy rejects instead
    of silently reducing the requested ceiling. Existing per-member checks may
    impose stricter limits. Caller joins capture before closing owner/switch.

    Reuse current source-owned errors, once-per-member reads, exact missing-leaf
    optional policy, overrides/discovery, work reservations and Catalog validation.
    Result contains no owner/identity/FD/callback. Root acquisition/retention costs
    remain visible in the host attempt ledger; do not double-charge a new root
    bracket that this endpoint does not create. *)
val capture_from_owner
  :  Chatml_project.t
  -> owner:Chatml_local_source.t
  -> request:Chatml_project.Selection.request
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t
