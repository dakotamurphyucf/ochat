open! Core

(** Explicit execution selection for an on-demand host. Owns no parallel ledger:
    the actual admitted registry entry is the selected owner. *)
type t

val create
  :  sw:Eio.Switch.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> job_result_max_count:int
  -> job_result_max_bytes:int
  -> store:Agent_store.Session_store.t
  -> registry:Session_registry.t
  -> factory:Session_factory.t
  -> t

(** Reauthorize retained principal against the actual state, validate the exact
    host/session/generation/canonical/lifecycle anchor and Automatic admission,
    then consume a current checked Current witness under the issuing per-ID
    reservation before execution recovery. Indexed selection retains one Handle
    continuously through immutable validation and actual factory recovery. Any
    provisional cleanup failure stays owned by the registry. Restore/Resume gate
    commits and cached receipts never select execution. Cancellation propagates;
    promotion and irreversible owner cleanup are protected. *)
val select
  :  t
  -> principal:Agent_protocol.Principal.t
  -> expected:Agent_protocol.Session_lifecycle.Expected.t
  -> (Session_registry.entry, Agent_protocol.Error.t) Result.t
