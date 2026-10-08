open! Core

(** Explicit meta-prompt context. No model/key environment lookup: omitted
    overrides inherit the supplied selected execution. Filesystem capabilities
    and injected offline heuristics remain separate from inference selection. *)
type prompt_type =
  | General
  | Tool

type action =
  | Generate
  | Update

type t =
  { proposer_model : string option
  ; rng : Random.State.t
  ; env : Eio_unix.Stdenv.base option
  ; inference : Inference_client.Execution.t option
  ; guidelines : string option
  ; model_to_optimize : string option
  ; action : action
  ; prompt_type : prompt_type
  }

(** Fresh deterministic RNG, no selected execution/model override, General and
    Generate. Model strategies require selected inference; explicit heuristic
    strategies may run offline. *)
val default : unit -> t

val with_proposer_model : t -> model:string option -> t
val with_guidelines : t -> guidelines:string option -> t
