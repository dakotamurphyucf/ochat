open Core
module Model = Openai.Responses.Request

(** Immutable context record passed explicitly through the
    meta-prompting pipeline.

    The record bundles all shared, mutable-in-principle values that the
    various optimisation steps might need — random state, model
    configuration, etc.  Threading it explicitly avoids hidden global
    state and makes unit-testing easier.  Adding a new field is a
    backwards-compatible change because callers can rely on OCaml's
    record update syntax ({[ { ctx with new_field } ]}).

    {1 Record fields}

    • [proposer_model] – optional override for the language model that
      proposes new prompts.
    • [rng] – deterministic pseudo-random generator used whenever the
      pipeline needs randomness.
    • [env] – optional {!Eio_unix.Stdenv.base} giving access to
      network, clock, filesystem, … when talking to external services.
    • [guidelines] – additional textual guidance injected into the
      system message.
    • [model_to_optimize] – model whose behaviour we are trying to
      approximate or improve.
    • [action] – whether we are generating a brand-new prompt or
      updating an existing one.
    • [prompt_type] – distinguishes between general-purpose assistant
      prompts and tool-specific prompts.
*)

type prompt_type =
  | General (** Free-form assistant conversation. *)
  | Tool (** Structured prompt intended to call a tool. *)

type action =
  | Generate (** Create a new prompt from scratch. *)
  | Update (** Refine or mutate an existing prompt. *)

(** Immutable context record.  See the module documentation for an
    explanation of each field. *)
type t =
  { proposer_model : Model.model option
  ; rng : Random.State.t
  ; env : Eio_unix.Stdenv.base option
  ; guidelines : string option
  ; model_to_optimize : Model.model option
  ; action : action
  ; prompt_type : prompt_type
  }

(** [default ()] returns a fresh context with

    - [rng] seeded deterministically for reproducible test runs;
    - [model_to_optimize] defaulting to {!Model.O3};
    - [action] = {!Generate};
    - [prompt_type] = {!General}.

    Use this when migrating existing code that did not yet thread an
    explicit context value. *)
val default : unit -> t

(** [with_proposer_model t ~model] returns a shallow copy of [t] in
    which [proposer_model] has been replaced by [model].  All other
    fields are kept intact. *)
val with_proposer_model : t -> model:Model.model option -> t

(** [with_guidelines t ~guidelines] returns [t] with the [guidelines]
    field replaced by [guidelines]. *)
val with_guidelines : t -> guidelines:string option -> t
