open Core
module Project = Chatml_project
module Build = Chatml_project_build
module Roots = Chatml_local_roots
module Git = Chatml_git_host
module Captured = Chatml_source

(** Host-only selected Local/Git preparation. Compiler/queries/resume and
    capture-only restore have no path to this API. No package registry/solver or
    Provided source kind; a supplied complete Prepared graph goes to source custody. *)

module Policy : sig
  (** Literal per-stage host ceilings. Host aggregation derives explicit smaller
      allowances before each stage; it never increases any supplied ceiling or
      translates authored requirements into tool/process/filesystem grants. *)
  type t

  val create
    :  roots:Roots.Limits.t
    -> local:Chatml_local_capture.Limits.t
    -> git_objects:Chatml_git_catalog.Limits.t
    -> git_native:Git.Limits.t
    -> composition:Build.Limits.t
    -> t
end

module Publication : sig
  module Expected : sig
    type t =
      | Absent
      | Exact of Captured.t
  end

  type state =
    | Available
    | Busy
    | Uncertain
    | Closed
  [@@deriving equal, sexp_of]

  (** Borrowed opened directory and exclusive caller coordination. The caller
      retains both until switch cleanup has revoked admission, cancelled and
      joined active work. Admission bounds the basename and expected bytes before
      retaining them; actual expected reads happen in preparation under its shared
      deadline. No manual close, automatic reconciliation or arbitrary writer API.
      Use the owner's switch domain; concurrent/reentrant fibers share its state.
      Writer failure or an undelivered entered write leaves Uncertain, even when
      replacement succeeded. Switch cleanup leaves Closed before resource release. *)
  type t

  val open_owned
    :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
    -> sw:Eio.Switch.t
    -> basename:string
    -> expected:Expected.t
    -> max_lock_bytes:int
    -> (t, Agent_store.Store_error.t) Result.t

  val state : t -> state
end

module Accounting : sig
  type t

  val packages : t -> int
  val dependency_visits : t -> int
  val source_count : t -> int
  val source_bytes : t -> int
  val local_directory_entries : t -> int
  val git_objects : t -> int
  val git_object_bytes : t -> int
  val git_tree_entries : t -> int
  val native_request_bytes : t -> int
  val native_output_bytes : t -> int
  val acquisition_payload_reserved : t -> int
  val work_steps : t -> int
end

(** Immutable completed Build and final invocation accounting. No clock, callbacks,
    native owners or mutable ledgers are retained. *)
type t

val build : t -> Build.t
val accounting : t -> Accounting.t

module Limits : sig
  (** One attempt's maxima. Work is the sum of documented backend/pure adapter
      units, not CPU/syscall proof. Git request bytes count the actual native
      request ledger, exposed through its final immutable receipt. Acquisition
      storage is the sum of admitted peak-payload
      reservations before work, not OS disk quota/cumulative writes. *)
  type t

  val create
    :  max_packages:int
    -> max_dependency_visits:int
    -> max_source_count:int
    -> max_source_bytes:int
    -> max_local_directory_entries:int
    -> max_git_objects:int
    -> max_git_object_bytes:int
    -> max_git_tree_entries:int
    -> max_native_request_bytes:int
    -> max_native_output_bytes:int
    -> max_acquisition_payload_reserved:int
    -> max_work_steps:int
    -> max_prepared_input_bytes:int
    -> wall_time:Time_ns.Span.t
    -> t Or_error.t
end

module Host : sig
  (** Borrowed explicit host authority, not author config. Retain anchor and Git
      store switches until each joined preparation returns. No ambient fs/cwd/
      HOME/PATH/install lookup or credential callback. Distinct exact route grants
      must be unambiguous. Git grants are trusted deployment selections; local
      dependencies use actual descriptor roots and retained identities. *)
  type t

  val create
    :  Roots.Anchor.t list
    -> git_store:Git.Store.t option
    -> git_grants:Git.Grant.t list
    -> max_grants:int
    -> max_grant_bytes:int
    -> t Or_error.t
end

module Root : sig
  (** Explicit locator intent, never authority. The selected project is package-
      relative; Git package_root affects raw tree lookup only. No native identity
      survives in a lock. Request admission bounds strings before scanning. *)
  type t =
    | Local of
        { locator : string
        ; project : Source.Portable_path.t
        }
    | Git of
        { repository : string
        ; pin : Project.Git_pin.t
        ; package_root : Source.Portable_path.t option
        ; project : Source.Portable_path.t
        }
end

module Request : sig
  type t

  (** Exact authentic contract supplied by the existing selected target adapter.
      Captured entry requires root Library own-env selection; Declared uses the
      selected declared entry. Initial/Build/Update use the same pure Selection_gate;
      Build preserves selected code while allowing environment/namespace changes.
      lock_id is an explicit logical custody identity, never a native filename. *)
  val create
    :  Root.t
    -> operation:Build.Operation.t
    -> selection:Project.Selection.request
    -> entry:Build.Entry.t
    -> contract:Chatml.Chatml_compiler_contract.t
    -> lock_id:Captured.Id.t
    -> max_request_bytes:int
    -> t Or_error.t
end

module Error : sig
  (** Finite domain failures retain honest optional declaration provenance, never
      raw exceptions, credentials, process output or live capabilities. *)
  type problem =
    | Limit_exceeded
    | Invalid_accounting
    | Accounting_overflow
    | Timed_out
    | Authority_denied
    | Conflicting_selection
    | Roots of Roots.Error.t
    | Source of Chatml_local_source.Error.t
    | Local of Chatml_local_capture.Error.t
    | Git_host of Git.Error.t
    | Git_catalog of Chatml_git_catalog.Error.t
    | Bootstrap of Document_schema.Error.t
    | Declaration of Project.Error.t
    | Composition of Build.Error.t
    | Tool_admission of Chat_response.Tool_capability.error
    | Stale_selection
    | Publication of Agent_store.Store_error.t
    | Publication_uncertain of Agent_store.Store_error.t

  type t

  val problem : t -> problem
  val declared_at : t -> Project.Declared_at.t option
end

(** Capture the selected declaration closure through explicit host authority.
    Exact aliases reuse captured sources; every incoming previous-lock route is
    checked before acquisition. Authored tool names are admitted against [tools]
    without invoking them or granting authority. No resolver, native owner,
    checkpoint or tool implementation is retained in the immutable result.

    Failure aborts the complete attempt, without hidden retries or refunds.
    Cancellation and unexpected exceptions propagate after joined cleanup.
    An independent file-backed launch captures current local files; an existing
    runtime continues using its retained immutable sources. *)
val prepare
  :  Host.t
  -> Request.t
  -> tools:Chat_response.Tool_capability.t
  -> policy:Policy.t
  -> limits:Limits.t
  -> clock:_ Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Prepare and select the completed lock under [publication]'s existing exclusive
    host coordination. The one deadline starts before the first expected-lock read.
    Tool admission, complete result allocation and the final progress/deadline
    check precede protected atomic replacement. There is no postcommit progress
    callback. Before writer entry, failure preserves the old lock. After entry,
    an unconfirmed outcome leaves [Publication.Uncertain]; no rollback or old
    selection is claimed. Expose the new queue/runtime only after success. *)
val prepare_and_publish
  :  Host.t
  -> Request.t
  -> publication:Publication.t
  -> tools:Chat_response.Tool_capability.t
  -> policy:Policy.t
  -> limits:Limits.t
  -> clock:_ Eio.Time.Mono.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t
