open Core
module Host = Chatml_git_host
module Project = Chatml_project
module Path = Source.Portable_path

(** Exact Git catalog adapter. Stock-Git process/grant/staging/cache ownership
    belongs to Chatml_git_host. This module reads a scoped immutable repository and
    verifies exact objects before producing shared immutable catalog facts. *)

module Limits : sig
  type t

  (** Explicit semantic allowance for ONE scoped Reader, aggregated by204 across a
      preparation. No global defaults or inferred compiler limits. Object bytes
      include raw object bodies;201 additionally bounds full native protocol/output.
      Tree entries count every decoded entry, including ignored names/modes; source
      count/bytes include declaration, entry, implementations and interfaces once
      per exact selected Source.Id. Distinct paths with equal blobs count separately.
      Decoded tree component names have the fixed1024-byte pre-copy bound used by
      Catalog discovery. Objects/decoded trees are charged once per distinct OID
      in this Reader; memoized reuse does not issue a fresh native query. Every
      actual query separately consumes201's aggregate native output/work allowance.
      Bounds are checked before copying/querying/retaining new data. *)
  val create
    :  max_object_bytes:int
    -> max_total_object_bytes:int
    -> max_objects:int
    -> max_tree_entries:int
    -> max_sources:int
    -> max_source_bytes:int
    -> max_total_source_bytes:int
    -> max_path_components:int
    -> max_work_steps:int
    -> document:Document_schema.Limits.t
    -> t Or_error.t
end

module Accounting : sig
  (** Immutable observed semantic work. No repository, callback, grant or mutable
      counters.204 combines this with201's separate native acquisition receipt;
      semantic units never claim to measure native instructions or physical disk. *)
  type t

  val object_count : t -> int
  val object_bytes : t -> int
  val tree_entries : t -> int
  val source_count : t -> int
  val source_bytes : t -> int
  val work_steps : t -> int
end

module Error : sig
  type limit =
    | Object_bytes
    | Total_object_bytes
    | Object_count
    | Tree_entries
    | Tree_name_bytes
    | Source_count
    | Source_bytes
    | Total_source_bytes
    | Path_components
    | Repository_member_bytes
    | Work_steps
  [@@deriving equal, sexp_of]

  type problem =
    | Host of Host.Error.t
    | Missing_object of Host.Object_id.t
    | Malformed_object_protocol
    | Object_hash_mismatch of Host.Object_id.t
    | Wrong_object_kind of Host.Object_id.t
    | Invalid_commit
    | Invalid_tree
    | Missing_member of Path.t
    | Unsupported_member_mode of Path.t
    | Limit_exceeded of limit
    | Reader_closed
    | Reader_poisoned
    | Reader_busy
    | Foreign_declaration
    | Invalid_namespace_override
    | Conflicting_capture of Chatml_source.Id.t
    | Bootstrap of Document_schema.Error.t
    | Accounting_overflow
    | Declaration of Project.Error.t
    | Catalog of Chatml_catalog.Error.t

  (** Finite typed failure plus honest optional exact declaration-owned context.
      Before declaration capture exists, no source span is invented. Never retain
      credentials/routes/native paths/stderr/raw object bodies or exception text.
      At most one selected logical member and two existing declaration contexts. *)
  type t

  val problem : t -> problem
  val declared_at : t -> Project.Declared_at.t option
  val related : t -> Project.Declared_at.t option
end

module Reader : sig
  (** One scoped201 Repository, semantic ledger and verified object/source memo.
      The callback must not retain t. Escaped readers reject Closed and their state
      is cleared on exit, releasing callbacks/repository/cache. Expected operation
      rejection poisons this Reader: caught failures cannot authorize successful
      outer cache publication. Uncaught user exceptions keep identity/backtrace. *)
  type t

  type declaration

  (** Operations run in switch-owned workers and reject concurrent/reentrant use
      Busy. Callback return with a pending operation rejects success and cancels/
      joins that operation BEFORE releasing borrowed state/repository. Waiting caller
      cancellation likewise stops/joins its worker before preserving the exception.
      Protected cleanup may exceed the operational deadline; arbitrary uncooperative
      user CPU callbacks cannot be synchronously preempted. *)

  val with_repository
    :  Host.Repository.t
    -> limits:Limits.t
    -> checkpoint:(unit -> unit)
    -> f:(t -> ('a, 'error) Result.t)
    -> ('a * Accounting.t, [ `Reader of Error.t | `Callback of 'error ]) Result.t

  (** Validate translated path/component allowance BEFORE object work. Verify
      exact pinned commit/traversed trees/selected declaration blob and project
      authored schema/namespace before creating Source.Id. An explicit override
      never bypasses namespace validity. Create/hash final source once, fully
      decode exact same bytes, no fake/bootstrap namespace. Failure before final
      declaration ownership remains honestly unlocated, never prior package-owned.
      package_root prefixes tree lookup only, not logical Source.Id paths. *)
  val read_declaration
    :  t
    -> package_root:Path.t option
    -> project:Path.t
    -> namespace_override:string option
    -> (declaration, Error.t) Result.t

  val project : declaration -> Project.t
  val package_root : declaration -> Path.t option

  (** Declaration belongs to this Reader, exact member/root/namespace. Reject
      foreign tokens before borrowing anything. Own selection/Discovery/Plan and
      Catalog APIs define names/aliases/scopes; only declared directories enumerate.
      Selected/traversed symlink/gitlink/unsupported modes reject; unrelated links
      remain inert. Optional absence means absent final tree entry, never missing
      object, bad ancestor, kind/hash or malformed tree. Executable blobs are exact.
      Reuse exact declaration and shared captured sources; distinct Source.Id values
      charge once each even for equal blobs. Source-cache conflicts reject.
      Switch error context to THIS declaration even after another package was read. *)
  val capture
    :  t
    -> declaration
    -> request:Project.Selection.request
    -> (Chatml_catalog.t, Error.t) Result.t

  (** Total-to-date, not a resetting/delta budget. Final bracket receipt supersedes
      snapshots;204 aggregates exactly one final receipt per scoped Reader. Closed/
      poisoned readers reject. No native instruction/disk quota claim. *)
  val accounting : t -> (Accounting.t, Error.t) Result.t
end

(** No transport API: callers use201.with_acquisition or with_cached_repository
    around Reader.with_repository and propagate failure. Same host monotonic deadline
    is polled during actual bounded hash/copy/tree/catalog work through Repository.
    checkpoint. Raw query cap checked-adds bounded header/trailer to the remaining
    semantic body allowance BEFORE query. One memo charges verified OIDs/decoded
    trees once and captured Source.Id bytes once; every native query independently
    consumes201's existing cumulative wire/output/work ledger. Error/exception returns
    no partial completed result; Fiber cancellation and caller exceptions retain
    identity/backtrace.204 validates repository intent against host grant before
    acquisition and composes the full closure/locks.203 restore uses captures only. *)
