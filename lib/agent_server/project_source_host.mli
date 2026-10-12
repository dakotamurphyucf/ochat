open Core
module Sources = Agent_session.Prompt_script_preparation
module Git = Chatml_git_host
module Configuration = Project_source_configuration
module Policy = Project_source_policy

module Error : sig
  type t =
    | Invalid_configuration of Core.Error.t
    | Filesystem
    | Authorization_header_too_large
    | External_root of Chatml_local_roots.Error.t
    | Git of Git.Error.t
    | Closed
    | Foreign_domain

  val sexp_of_t : t -> Sexp.t
end

type t

(** Startup admission of administrative authority. No Git executable, helper or
    cache is opened for Local-only configuration. External directories are opened
    from the explicitly supplied fs capability and admitted through Anchor.create.
    Enabled Git uses exactly the admitted Git2.56.0 helper profile, stable optional
    CA file and exact configured grants. No PATH/config/credential fallback.

    The explicit Git memory cap is inserted into BOTH native acquisition and
    deployment probes. Policy contains no second memory cap. Input count/string
    bounds are checked before opening paths or retaining derived lists. Credential
    reads are bounded before allocation; errors never contain header bytes.

    Git cache is the fixed private child [project-git] below actual data_root;
    Git.Store owns its private-root, lease, inventory and joined operation checks.
    The operator keeps native labels/ancestors and deployment files stable. These
    labels do not prove pathname-to-descriptor binding or grant Local confinement
    to Grant.local. Unsupported configured Git fails startup, including offline use.

    Same creating Eio domain. An owner-private switch revokes admission, cancels
    and joins borrowers before releasing directories/helpers/cache. Caller owns
    sw through close_and_wait. Unexpected/checkpoint exceptions retain identity
    and backtrace; only expected filesystem failures become finite error tags. *)
val open_owned
  :  Configuration.t
  -> sw:Eio.Switch.t
  -> fs:Eio.Fs.dir_ty Eio.Path.t
  -> data_root:Agent_store.Data_root.t
  -> clock:_ Eio.Time.Mono.t
  -> ?policy:Policy.t
  -> checkpoint:(unit -> unit)
  -> unit
  -> (t, Error.t) Result.t

(** Borrow immutable source policy and actual existing anchors/store/grants for
    one complete source preparation. Scope privately supplies the original root,
    so the default empty Host supports local configured projects beneath it.
    Callback must join its own work. Returned prepared graphs/custody/plans have
    no live source-host capability; publication/commit/retire need no continued
    source-host borrow. Ordinary backend Busy rejects, without retry or reset.
    Operation selection comes from the authoritative baseline coordinator.

    The caller establishes its ONE catalog Startup before traversing definitions,
    using Policy.attempt_wall_time, and passes that checkpoint through every
    definition/history/capture stage. Per-definition source Startup cannot extend
    that deadline. No deadline is inferred from stored metadata. Callback and
    cancellation exceptions propagate unchanged after joined cleanup. *)
val with_host
  :  t
  -> f:(Sources.Policy.t -> Sources.Host.t -> 'a)
  -> ('a, Error.t) Result.t

(** Finite original catalog-attempt ceiling, independent of runtime restoration's
    5s policy. This immutable query grants no source authority. *)
val policy : t -> Policy.t

(** Nonblocking idempotent revocation/cancel, then explicit joined close before
    persistence/source resources release. Never self-join from a borrowed callback.
    All queries/mutations require the creating domain. *)
val close : t -> unit

val close_and_wait : t -> unit
