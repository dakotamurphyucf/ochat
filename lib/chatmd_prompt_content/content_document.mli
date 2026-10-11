open Core
module B = Content_budget
module Inventory = Content_inventory
module Storage = Agent_store.Prompt_program_bundle

(** Named ochat.prompt-content v1 codec retaining complete extension ownership. *)
type t

val inventory : t -> Inventory.t
val to_storage : t -> Storage.t
val create : Inventory.t -> budget:B.t -> (t, B.Error.t) Result.t

val of_storage
  :  Storage.t
  -> inputs:Chatmd_parser_input.t list
  -> budget:B.t
  -> (t, B.Error.t) Result.t

val adopt : t -> previous:t -> budget:B.t -> (t, B.Error.t) Result.t
