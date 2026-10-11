open Core
module Pin = Chatml_project.Git_pin

(** Host-only stock-Git repository owner. Fixed command profile and
    private staging/cache lifecycle; no project decoder, catalog planner,
    compiler, execution capability registry or arbitrary command runner. *)
module Error : sig
  type code =
    | Unsupported_profile
    | Unsupported_provider
    | Invalid_input
    | Authority_denied
    | Limit_exceeded
    | Stale_request
    | Process_failed
    | Timed_out
    | Storage_failed
    | Cache_corrupt
    | Busy
    | Closed
  [@@deriving equal, sexp_of]

  type t

  val code : t -> code

  (** Finite diagnostics only. Never retain argv, environment, repository URL,
      credentials, helper stderr, paths or unexpected exceptions. *)
  val sexp_of_t : t -> Sexp.t
end

module Deployment : sig
  type t

  (** Host deployment inputs, never author configuration. Git and its curl/TLS
      build are trusted, maintained and immutable for this owner's lifetime.
      The supplied directory contains exactly admitted git, git-upload-pack and
      git-remote-https entrypoints; no http-fetch or arbitrary helper. Host keeps
      that directory and executable targets immutable. This is an explicit host
      trust contract, not authentication of an adversarial executable or a
      dynamic-library filesystem sandbox.

      Admit only the source-audited Git2.56.0 protocol-v0 command profile after bounded
      version/build/helper probes and target-platform qualification. Reject
      unsupported inventory/profile; no fallback to another installed Git.
      The absolute executable path must identify the admitted git entrypoint.
      Inputs/verification are bounded before copying/scanning. The helper directory
      is borrowed during joined inventory checks; children use its supplied trusted
      absolute native path in nonempty PATH and GIT_EXEC_PATH. An optional
      bounded absolute CA bundle is a stable trusted host file. [None] selects
      qualified platform system trust; ambient TLS environment is never inherited.
      TLS certificate verification is always enabled. *)
  val admit_git_2_56_0
    :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
    -> native_helper_directory:string
    -> native_git_executable:string
    -> native_ca_bundle:string option
    -> clock:_ Eio.Time.Mono.t
    -> probe_limits:Shell_access.Bounded_process.Limits.t
    -> max_deployment_bytes:int
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end

module Grant : sig
  type t

  (** Exact host-admitted HTTPS route. No userinfo/fragment/newline/NUL and no
      redirects. Optional authorization is a bounded single header selected by
      host policy, never provider/client credential forwarding. It is retained
      only in this live grant, never serialized/logged/returned in errors.
      Creating this value expresses host authorization, not project intent. *)
  val https
    :  repository:string
    -> authorization:string option
    -> max_input_bytes:int
    -> t Or_error.t

  (** Exact explicitly host-admitted native absolute repository path. Host
      authorizes Git's read of this repository and its Git metadata/layout and
      keeps the native path stable during acquisition. No claim of descriptor
      confinement, source-member nofollow, realpath identity or OCH200 authority.
      Commit contents/tree modes remain untrusted and are verified by the caller.
      Bounded syntax validation grants no authority by itself. *)
  val local : repository:string -> max_input_bytes:int -> t Or_error.t

  val repository : t -> string
end

module Store_limits : sig
  type t

  (** Bound existing immutable cache inventory, native path/component work,
      managed directories/entries and setup/cleanup work. Unknown or excessive
      existing entries reject admission; never recursively delete caller data.
      Cache bytes count unique regular-file payload extents. No physical block
      quota or cumulative write-I/O guarantee. *)
  val create
    :  max_cache_repositories:int
    -> max_cache_payload_bytes:int
    -> max_managed_entries:int
    -> max_path_bytes:int
    -> max_work_steps:int
    -> t Or_error.t
end

module Store : sig
  type t

  (** Retained empty-path writable private directory capability. Verify operator
    ownership/private permissions/local filesystem with established native
    storage checks, and own independent descriptors before publication. Fixed
    managed stage/cache names only, opened component-relative with nofollow;
    no HOME/global cwd/root discovery. The host keeps the native root label and
    all its ancestors stable for fixed Git path interoperability. [Eio.Path.native]
    supplies a label, not proof of binding; no guarantee covers a host-violated
    rename or replaced ancestor. The host keeps the retained root 0700 with no ACL
    throughout the owner lifetime; root privacy is revalidated on operation
    admission and before Git/copy publication stages. Group-write descendants
    from umask002 are permitted only below that private ancestor; world-write,
    special bits and aliases reject. Copied cache directories/files are created
    0700/0600. Caller retains the switch and joins users
    before closure. Lifecycle is switch-only: no manual close API. Switch release
    rejects new work, joins active operations and releases owned resources.
    No operation/lifetime mutex is held across caller callbacks; reentrant bracket
    use is rejected Busy rather than deadlocking. In-bracket Repository queries
    are supported. An exclusive nonblocking kernel store-ownership lease may
    remain held for the store lifetime; another owner of the same store is Busy.
    Existing cache inventory is bounded
    and reconciled into immutable in-memory accounting before requests.

    Deployment qualification is supplied by host startup, not an authored
    package-manager installation/provisioning flow. *)
  val open_root
    :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
    -> sw:Eio.Switch.t
    -> deployment:Deployment.t
    -> clock:_ Eio.Time.Mono.t
    -> limits:Store_limits.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end

module Limits : sig
  type t

  (** Literal native per-file caps and aggregate adapter allowances. No cap is
      silently raised. One monotonic attempt deadline/output/work ledger covers
      stage commands, callback object queries and cache publication. Poll deadline
      at every bounded owner stage and after callback success BEFORE publication;
      each native command receives remaining time/output/request allowance.
      Caller pure work must cooperate through its own bounded checkpoints; no
      synchronous preemption of arbitrary callback code is claimed. Protected
      joined cleanup may exceed the operational deadline.
      CPU/address-space/FD limits are per process; fixed admitted helpers must
      not escape their owned group. Work units are conservative adapter stages,
      not native instruction counts. *)
  val create
    :  metadata_file_bytes:int
    -> pack_file_bytes:int
    -> max_acquisition_payload_bytes:int
    -> max_request_bytes:int
    -> max_output_bytes:int
    -> max_work_steps:int
    -> wall_time:Time_ns.Span.t
    -> cpu_seconds_per_process:int
    -> memory_bytes_per_process:int
    -> open_files_per_process:int
    -> t Or_error.t
end

module Request : sig
  type t

  (** Pure after store admission: exact grant/pin/profile and checked finite
      pre-work reservation. Includes init probes/metadata, pack/index/keep/shallow
      family and cache-copy overlap. Existing admitted cache bytes are accounted
      separately by Store. No filesystem/cache probe/credential lookup/fetch here.
      Bind current store generation; stale concurrent mutations reject acquisition
      before effects. Author repository intent must agree with the exact grant
      before this host API is invoked. No implicit retry/refund. *)
  val prepare
    :  Store.t
    -> grant:Grant.t
    -> pin:Pin.t
    -> limits:Limits.t
    -> (t, Error.t) Result.t

  val payload_bytes_reserved : t -> int
end

module Object_id : sig
  type t [@@deriving equal, compare, sexp_of]

  (** Exact40/64 lowercase hex of the selected Git format, no ref/path/revision
      expression. A syntactic object ID does not assert existence or kind. *)
  val create : object_format:Pin.object_format -> string -> t Or_error.t

  val of_pin : Pin.t -> t
  val to_string : t -> string
  val object_format : t -> Pin.object_format
end

module Repository : sig
  (** Scoped immutable stored-object bytes after controlled acquisition, or
      exact cache bytes. This owner verifies storage/profile/layout bounds;
      it does NOT assert that the requested ID is a commit or tree/blob contents
      are authentic. The raw-object adapter verifies those before returning.
      Handle has no fetch port and rejects use outside its bracket. Queries run
      in joined owner fibers. Returning while a query is still active rejects the
      attempt and stops/joins that query before descriptors are released. *)
  type t

  val pin : t -> Pin.t

  (** Poll this bracket's live admission, cancellation, poison and monotonic
      deadline during pure hashing/tree/catalog work. Semantic work remains
      caller-accounted; this grants no acquisition authority and cannot preempt
      arbitrary CPU work. Escaped handles reject Closed. *)
  val checkpoint : t -> (unit, Error.t) Result.t

  (** Only fixed cat-file --batch header/raw queries with exact validated ID.
      No shell/ref syntax, --filters/--textconv, replacement or lazy fetch.
      Bounded raw protocol output includes the header/trailer; caller checks
      reported kind, length and Git hash before capturing source bytes.
      Invocation allowances consume the ONE bracket ledger; never reset limits
      per query. Process/limit/checkpoint failure poisons the attempt before
      propagation: catching such a failure in the callback cannot permit another
      query or cache publication with unknown consumed accounting.
      Checkpoint/cancellation propagate after joined process cleanup. Query
      checkpoints run in the owned query fiber; no callback is retained after its
      query or repository bracket ends. *)
  val raw_object
    :  t
    -> Object_id.t
    -> max_output_bytes:int
    -> checkpoint:(unit -> unit)
    -> (string, Error.t) Result.t
end

module Receipt : sig
  type disposition =
    | Cached
    | Fetched
  [@@deriving equal, sexp_of]

  type t

  val disposition : t -> disposition
  val payload_bytes_reserved : t -> int

  (** Cumulative bytes charged by the native request ledger, including executable,
      arguments, explicit environment and stdin. This is the admitted request
      size, not a network-transfer count or the configured maximum allowance. *)
  val request_bytes : t -> int

  val output_bytes : t -> int
  val work_steps : t -> int
end

(** Fresh exclusive bare stage per miss, exact init+local fetch-pack or HTTPS
    remote-helper profile. No checkout/install/ref publication. Callback performs
    semantic verification through the bounded read-only handle. Callback failure
    or exception publishes no cache revision; after success, publish only bounded
    immutable repository bytes using owned atomic cache lifecycle. Cache storage
    alone makes no semantic commit assertion; future cache use is reverified.
    Cache fill never publishes a selected project lock/runtime. Failure cleanup
    can leave an unreferenced bounded orphan; never reuse it as admitted cache.
    Protected stop/join precedes stage deletion; preserve exception/backtrace.
    After callback success and the atomic publication boundary, late cancellation
    or directory-sync failure can leave a verified cache revision; no rollback or
    selected project publication is implied. Publication failure invalidates the
    store inventory, rejecting further admission until release/reopen reconciliation.
    No callback/resource survives the bracket. *)
val with_acquisition
  :  Request.t
  -> checkpoint:(unit -> unit)
  -> f:(Repository.t -> ('a, 'error) Result.t)
  -> ('a * Receipt.t, [ `Host of Error.t | `Callback of 'error ]) Result.t

(** Exact stored revision only, no Grant/network parameter and no acquisition
    operation. No missing-object/invalid-cache fallback. Same bounded query
    ledger and joined lifetime as acquisition; no cache mutation. *)
val with_cached_repository
  :  Store.t
  -> repository:string
  -> pin:Pin.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> f:(Repository.t -> ('a, 'error) Result.t)
  -> ('a * Receipt.t, [ `Host of Error.t | `Callback of 'error ]) Result.t
