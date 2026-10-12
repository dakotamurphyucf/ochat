open Core
module Inventory = Program_inventory

(** Named custody document with retained extension ownership and exact storage
    payloads. Domain construction is bounded and side-effect free. *)
type t

val inventory : t -> Inventory.t
val to_storage : t -> Agent_store.Prompt_program_bundle.t

val create
  :  Inventory.t
  -> authored:Chatmd_source_bundle.t
  -> limits:Inventory.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Inventory.Error.t) Result.t

val of_storage
  :  Agent_store.Prompt_program_bundle.t
  -> authored:Chatmd_source_bundle.t
  -> limits:Inventory.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Inventory.Error.t) Result.t

(** Replace the known projection through the named document extension carrier.
    Unsafe unknown-field reassignment/deletion conflicts; old values stay intact. *)
val replace
  :  t
  -> Inventory.t
  -> authored:Chatmd_source_bundle.t
  -> limits:Inventory.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Inventory.Error.t) Result.t

(** Preserve all previous admitted programs, logical revisions and parser inputs,
    then transfer compatible unknown fields into the incoming selection. *)
val adopt
  :  t
  -> previous:t
  -> authored:Chatmd_source_bundle.t
  -> limits:Inventory.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Inventory.Error.t) Result.t
