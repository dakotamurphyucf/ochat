(** One durable authored checkpoint opportunity per recovery window. An offer is
    recorded before provider dispatch. A retained offer never grants permission
    to dispatch again after cancellation or restart. *)
open! Core

module P = Agent_protocol
module Catalog = Recovery_notes.Catalog
module Expected = Recovery_rollover_checkpoint.Expected
module Attempt_id = Inference.Observation.Observation_id

type unavailable =
  | No_recipe
  | Unproven_fit
  | Unproven_source
  | Ineligible_response
  | Checkpoint_refused
[@@deriving equal, sexp_of]

type recipe =
  | Register
  | Create_note of
      { catalog : Catalog.Catalog_revision.t
      ; path : Catalog.Note_path.t
      }
[@@deriving equal, sexp_of]

type decision =
  | Offer of recipe
  | Withhold of unavailable
[@@deriving equal, sexp_of]

module Source : sig
  type t [@@deriving equal, sexp_of]

  val capture : P.History.entry list -> (t, P.Error.t) result

  val check
    :  t
    -> P.History.entry list
    -> own_call:P.History.Id.t option
    -> (unit, P.Error.t) result
end

module Claim : sig
  type t = private
    { id : P.Id.Operation.t
    ; expected : Expected.t
    ; owner : P.Id.Operation.t
    ; principal : P.Id.Principal.t
    ; configuration_revision : int64
    ; preparation : Attempt_id.t
    ; policy : Chatml_source.Digest.t
    ; decision : decision
    ; source : Source.t option
    }
  [@@deriving equal, sexp_of]

  val create
    :  id:P.Id.Operation.t
    -> Expected.t
    -> owner:P.Id.Operation.t
    -> principal:P.Id.Principal.t
    -> configuration_revision:int64
    -> preparation:string
    -> policy:Chatml_source.Digest.t
    -> decision:decision
    -> source:Source.t option
    -> (t, P.Error.t) result
end

type outcome =
  | Checkpointed of
      { invocation : P.Id.Invocation.t
      ; attempt_ordinal : int64
      }
  | Unavailable of unavailable
  | Interrupted
[@@deriving equal, sexp_of]

type change =
  | Admit of Claim.t
  | Accept of
      { id : P.Id.Operation.t
      ; expected : Expected.t
      ; attempt : Attempt_id.t
      }
  | Settle of
      { id : P.Id.Operation.t
      ; expected : Expected.t
      ; outcome : outcome
      }

type t [@@deriving equal, sexp_of]

val claim : t -> Claim.t
val attempt : t -> Attempt_id.t option
val outcome : t -> outcome option

val apply
  :  t option
  -> active:P.Recovery_window.Scope.t
  -> revision:int64
  -> change
  -> (t, P.Error.t) result

val change_to_json : change -> Jsonaf.t
val change_of_json : Jsonaf.t -> (change, P.Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
