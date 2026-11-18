(** Fixed pure Session recovery CPU operations. One adapter is created for the
    actual Factory and reused by every retention preflight, bounding that
    Factory to one admitted CPU domain at a time. It owns no storage or execution
    authority and accepts no codec callback. *)
open! Core

type t

val create
  :  domain_mgr:Eio.Domain_manager.ty Eio.Resource.t
  -> limits:Document_schema.Limits.t
  -> t

(** Calls on the creating Eio domain acquire the adapter's single permit before
    spawning a domain. Waiting for admission is cancellable. The permit stays
    held until Domain_manager.run has joined, including cancellation or raised
    exceptions. No Factory, environment, Handle, Journal, Persistence, retention
    owner, semaphore or domain manager enters the worker closure: only the
    fixed operation's immutable inputs and immutable limits do.

    Each operation uses the same canonical path and error ordering as its direct
    counterpart. Raised exceptions and their raw worker backtraces propagate
    unchanged through the actual Domain_manager; no partial result escapes.
    Operation-local Capture/lazy encoders are created and consumed by the worker.
    No archive IO, proof/cache mutation or logging is performed by this module. *)
val restore
  :  t
  -> Agent_store.Snapshot.t
  -> (Session_state_document.t option, Agent_store.Store_error.t) result

val apply
  :  t
  -> Session_state_document.t option
  -> Agent_store.Transaction.t
  -> (Session_state_document.t option, Agent_store.Store_error.t) result

(** Complete encode-left, encode-right, then exact string equality. Preserves
    unknown field ordering, presence and numeric spelling; never uses a digest
    or a reduced native projection. Left failure remains before right failure. *)
val equivalent
  :  t
  -> Session_state_document.t option
  -> Session_state_document.t option
  -> (bool, Agent_store.Store_error.t) result

(** Pure Session_state.validate with the existing None and Corrupt conversion.
    The Factory must next run EACH existing fresh archive validator on its own
    domain in the original order, before returning the validation result. *)
val validate_native
  :  t
  -> Session_state_document.t option
  -> (unit, Agent_store.Store_error.t) result
