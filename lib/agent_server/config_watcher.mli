(** Configuration reload ownership with explicit durable publication outcomes. *)
open! Core

(** Operations are confined to the creating Eio domain. A foreign-domain call
    raises [Invalid_argument] before state access or callbacks. *)
type t

module State : sig
  type t =
    | Available
    | Busy
    | Uncertain
    | Closed
  [@@deriving equal, sexp_of]
end

module Certainty : sig
  type t =
    | Rejected
    | Uncertain
  [@@deriving equal, sexp_of]
end

module Failure : sig
  type t =
    | Rejected of Config.Diagnostic.t list
    | Uncertain of Config.Diagnostic.t list
  [@@deriving sexp_of]

  val diagnostics : t -> Config.Diagnostic.t list
end

(** Hooks belong to this watcher and may not be shared by another reload owner.
    Preparation and durable publication run outside its mutex and may perform IO.
    [publish] returns [Ok ()] only after durable acknowledgment. An uncertain
    outcome prohibits further reload or fresh catalog-dependent admission.

    [commit] is synchronous, nonyielding, preallocated and infallible for admitted
    prepared values. It performs only in-memory installation; it must not invoke
    arbitrary user callbacks or IO. The watcher runs it protected, outside the
    mutex, then swaps [current] and its observed modification time without any
    intervening yield. An unexpected commit exception is propagated and makes
    publication uncertain.

    [discard_prepared] cancels/joins preparation and releases unpublished sealed
    resources. Its result states whether publication is certainly rejected or
    uncertain; it must not roll back a durable commit. On an exception, cleanup
    runs protected; cleanup failure conservatively makes the owner uncertain and
    never replaces the original exception/backtrace.

    [audit] and [retire] run after pointer installation, outside the mutex. Their
    errors propagate without reverting the installed configuration. [retire]
    runs under cancellation protection even if audit raises, and must release
    only retired prepared resources.
    All hooks must join their own borrowed work before returning. *)
type hooks =
  { prepare : Config_diff.t -> Config.t -> (unit, Failure.t) Result.t
  ; publish : Config_diff.t -> Config.t -> (unit, Failure.t) Result.t
  ; commit : Config_diff.t -> Config.t -> unit
  ; discard_prepared : unit -> Certainty.t
  ; audit : Config_diff.t -> unit
  ; retire : unit -> unit
  }

val create
  :  sw:Eio.Switch.t
  -> env:Eio_unix.Stdenv.base
  -> path:string
  -> initial:Config.t
  -> hooks:hooks
  -> t

(** Last installed immutable configuration, including after uncertain reload.
    Retaining this value grants no admission to a new catalog-dependent session. *)
val current : t -> Config.t

val status : t -> State.t * Config.Diagnostic.t list option

(** A short admission check for callers about to begin fresh catalog-dependent
    work. It is not a borrow that excludes a subsequent reload; actual catalog
    acquisition must also use its sealed/generation-qualified host owner. *)
val check_admission : t -> (unit, Failure.t) Result.t

(** Claims [Busy] before loading/validation. Concurrent/reentrant reload rejects
    promptly. No callbacks or IO execute under the state mutex. Known rejection
    retains the previous configuration/mtime; uncertain publication blocks retry
    until explicit host reconciliation or restart (this API supplies no repair).

    Caller cancellation and hook exceptions retain their identity/backtrace.
    Cancellation after durable acknowledgment but before protected installation
    is uncertain; once installed, audit/retirement failure does not roll back.
    A final caller cancellation check precedes result delivery, including when
    the worker already completed; this does not repeat cleanup or installation.
    There is no caller checkpoint or IO between protected commit and pointer swap.
    [close] revokes admission and cancels active work; [close_and_wait] joins
    it before borrowed hook resources can be released. Hooks must not call
    [close_and_wait] on this watcher (that would join themselves). The watcher
    and its callbacks are confined to the creating Eio domain. *)
val reload : t -> (Config_diff.t, Failure.t) Result.t

(** Polls only while admission is available. Busy is skipped; uncertain and closed
    stop automatic reload. Unexpected hook exceptions propagate to the run switch. *)
val run : sw:Eio.Switch.t -> clock:_ Eio.Time.clock -> every:float -> t -> unit

(** Revokes new admissions and interrupts polling; idempotent and nonblocking.
    Cancels an admitted reload, including before callback entry. Does not release
    hook resources or pretend an in-flight publication rolled back. *)
val close : t -> unit

(** Revoke admission and cancel/join the admitted reload before releasing its
    borrowed resources. Protected joining preserves caller cancellation cleanup.
    Call outside this watcher's hooks and before closing persistence owners. *)
val close_and_wait : t -> unit
