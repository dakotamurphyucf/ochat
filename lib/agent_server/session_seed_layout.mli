open! Core

(** Immutable prelayout candidate for the actual admitted stopped seed. It borrows
    the same live Store/environment; neither a source grant nor a provisional
    native workspace is fabricated. Factory holds catalog/retained reservations. *)
type t

(** Actual workspace route planning and complete first-Created size admission,
    before directories/ownership markers. Original checkpoints/cancellation and
    unexpected exceptions propagate. The timestamp is the actual Factory clock. *)
val prepare
  :  Session_seed_admission.t
  -> env:Eio_unix.Stdenv.base
  -> store:Agent_store.Session_store.t
  -> principal:Agent_protocol.Principal.t
  -> request:Agent_protocol.Session_seed.Request.t
  -> command_audit:Document_schema.Document.t
  -> timestamp:Agent_protocol.Timestamp.t
  -> limits:Document_schema.Limits.t
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) Result.t

(** Factory has rechecked its retained witness and derived protected physical
    roots from the SAME admitted catalog. Mark the genuine layout attempt before
    calling: this operation enters Store creation. The caller's switch owns the
    resulting Handle and existing workspace lease. Exact one-shot Store evidence
    transfers the native workspace at protected rename; prepublication failures
    clean only owned temporary resources. Original exception/backtrace wins while
    the same lease retains secondary cleanup diagnostics for release/retry.
    No journal, actor, initializer, provider or receipt is created here. *)
val install
  :  t
  -> sw:Eio.Switch.t
  -> physical_roots:string list
  -> checkpoint:(unit -> unit)
  -> ( Agent_store.Session_store.Handle.t * Agent_session.Session_state.t
       , Agent_store.Store_error.t )
       Result.t
