open Core
module Inventory = Program_inventory
module Binding = Chatml_prompt_binding
module Contract = Chatml.Chatml_compiler_contract

module Program : sig
  type t

  val binding : t -> Binding.t
  val declaration : t -> Prompt.Chat_markdown.Declaration.t
  val input : t -> Inventory.Input.t
  val prepared : t -> Chatml_prepared_program.t
  val compiler_contract : t -> Chatml_source.Digest.t
end

(** Only the current durable active selection has been semantically restored. *)
type t

val programs : t -> Program.t list
val find : t -> Binding.t -> compiler_contract:Chatml_source.Digest.t -> Program.t option

val restore
  :  Inventory.t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> contracts:(Binding.t * Contract.t) list
  -> limits:Inventory.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Inventory.Error.t) Result.t
