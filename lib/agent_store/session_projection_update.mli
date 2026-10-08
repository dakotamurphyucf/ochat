(** Serialized ownership of recoverable metadata/index publication. The marker
    also preserves eager hydration of scheduling hints. A fresh successful update
    may clear only its own marker; a prior recovery requirement survives. *)
open! Core

type t

(** [sync_directory] supplies the retained owner's directory-sync capability;
    default is the existing durable adapter. Failure/cancellation after unlink
    restores the exact original admitted marker bytes and blocks live clearing. *)
val create
  :  ?sync_directory:
       (env:Eio_unix.Stdenv.base -> path:string -> (unit, Store_error.t) Result.t)
  -> env:Eio_unix.Stdenv.base
  -> marker_path:string
  -> unit
  -> t

val pending
  :  env:Eio_unix.Stdenv.base
  -> marker_path:string
  -> (bool, Store_error.t) Result.t

val require
  :  env:Eio_unix.Stdenv.base
  -> marker_path:string
  -> (unit, Store_error.t) Result.t

(** Callback prevalidates both documents before invoking [require_intent].
    It must finish the projection before returning success. Partial/uncertain
    failure keeps the marker; recovery completion in this live owner then fails
    until restart reconciliation. No committed authority is rolled back. *)
val publish
  :  t
  -> f:
       (require_intent:(unit -> (unit, Store_error.t) Result.t)
        -> ('a, Store_error.t) Result.t)
  -> ('a, Store_error.t) Result.t

(** Called after startup's complete eager hydration. Serialized with publication;
    refuses to erase any newer failed live publication's requirement. Active
    canonical tokens defer clearing with success; their matching publication owns
    later completion. Inherited requirements remain until a complete startup. *)
val complete_recovery : t -> (unit, Store_error.t) Result.t

module Pending : sig
  (** Live capability owned by one projection receiver and one session. The
      latest serial target advances only under that receiver's mutex. *)
  type t

  val matches : t -> Session_index.Entry.t -> bool
end

(** Run carrier/index prevalidation under this owner without authoritative
    effects. Callback may acquire index locks and yield; this owner remains held
    throughout. Before durably requiring
    recovery. Reuse only this owner's live same-session capability; lower revisions
    or changed full hints at equal revision fail. Uncertain begin blocks clearing.
    Caller may perform authoritative archive/journal I/O only after success. *)
val prepare_canonical
  :  t
  -> previous:Pending.t option
  -> prepare:(unit -> (Session_index.Entry.t, Store_error.t) Result.t)
  -> (Pending.t, Store_error.t) Result.t

(** Called only after matching metadata AND full index publication succeeds.
    Stale/cross-owner completion cannot retire a newer target. Last completion
    clears only a newly owned marker; inherited requirements remain for startup. *)
val finish_canonical
  :  t
  -> Pending.t
  -> entry:Session_index.Entry.t
  -> (unit, Store_error.t) Result.t
