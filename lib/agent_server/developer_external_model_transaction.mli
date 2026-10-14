(** One exact Store Prepared/Handoff and shared-capacity reservation for an
    external Model_call. This private module settles storage/capacity only;
    physical actor/provider/parent admission remains with Developer_root_owner. *)
open! Core

type t

val create
  :  reservation:Job_capacity.reservation
  -> prepared:Agent_store.Developer_run_store.Prepared.t
  -> t

val commit
  :  t
  -> Agent_store.Developer_run_store.handle
  -> admit:(unit -> bool)
  -> ( Agent_store.Developer_run_store.Settlement.t
       , Agent_store.Developer_run_store.Error.t )
       result

val reconcile
  :  t
  -> Agent_store.Developer_run_store.handle
  -> ( Agent_store.Developer_run_store.Settlement.t
       , Agent_store.Developer_run_store.Error.t )
       result

(** Only after the exact writer proved this transaction absent or prewrite
    conflicting and the physical call will not retry. An attempted unknown
    write must retain [t] and its reservation. *)
val abort_proven_absent : t -> unit
