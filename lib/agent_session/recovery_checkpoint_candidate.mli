(** Local preparation of the single pre-authorized stock checkpoint tool for
    the worker. The candidate carries only coordinates and never note contents. *)
open! Core

module P = Agent_protocol
module O = Recovery_checkpoint_opportunity
module Catalog = Recovery_notes.Catalog

type t

val create
  :  scope:P.Recovery_window.Scope.t
  -> id:P.Id.Operation.t
  -> tools:Chat_response.Soft_checkpoint.tools
  -> Catalog.t
  -> t option

val recipe : t -> O.recipe
val project : t -> Inference.Request.t -> Inference.Request.t option
val accepts : t -> name:string -> arguments:string -> bool
val matches_create : O.recipe -> Catalog.mutation -> bool

val selected_tools
  :  Chat_response.Tool_capability.t
  -> Chat_response.Soft_checkpoint.tools

val checkpoint_selected : Chat_response.Tool_capability.t -> bool

val pressure
  :  Inference_runtime.Context_capacity.Policy.t
  -> Inference_runtime.Prepared.t
  -> Inference_runtime.Context_capacity.Assessment.pressure option
