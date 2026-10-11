open Core
module Error = Git_host_profile.Error
module Pin = Chatml_project.Git_pin

(** Borrowed repository directory with a joined query scope and one attempt
    ledger. No cache/store lifetime, staging/copy/publication or semantic capture.
    Caller keeps [directory] alive through [with_directory]'s terminal return. *)
type t

val with_directory
  :  Git_host_store.t
  -> directory:Git_host_storage.t
  -> pin:Pin.t
  -> attempt:Git_host_attempt.t
  -> limits:Git_host_attempt.Limits.t
  -> f:(t -> 'a)
  -> 'a

val pin : t -> Pin.t

(** Poll live admission, cancellation and this same attempt's monotonic deadline.
    Pure semantic callers combine this with their own unit accounting/checkpoints;
    no arbitrary CPU preemption or additional resource authority. *)
val checkpoint : t -> (unit, Error.t) Result.t

(** Private fixed-profile init/fetch/raw command runner. Request, environment,
    output, deadline and work share [t]'s ledger; failure poisons before returning
    or propagating the original exception/backtrace after joined cleanup. *)
val run
  :  t
  -> Git_host_profile.command
  -> file_size_bytes:int
  -> max_output_bytes:int
  -> checkpoint:(unit -> unit)
  -> (string, Error.t) Result.t

(** Fixed no-follow layout/config admission before any offline Git invocation. *)
val validate
  :  t
  -> checkpoint:(unit -> unit)
  -> (Git_host_storage.Inventory.t, Error.t) Result.t

val raw_object
  :  t
  -> Git_host_profile.Object_id.t
  -> max_output_bytes:int
  -> checkpoint:(unit -> unit)
  -> (string, Error.t) Result.t

(** Call semantic verifier once, close its query admission afterwards, and
    reject a returned success while a query is pending or the ledger is poisoned.
    Enclosing [with_directory] stops/joins all query fibers before returning. *)
val verify
  :  t
  -> f:(t -> ('a, 'error) Result.t)
  -> ('a, [ `Host of Error.t | `Callback of 'error ]) Result.t
