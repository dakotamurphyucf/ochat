(** Durable evidence produced by the selected foreground checkpoint call.
    It carries no live authority and contains no note bodies or caller key. *)
open! Core

module P = Agent_protocol
module Catalog = Recovery_notes.Catalog

val name : string
val implementation_revision : string
val note_name : string
val note_implementation_revision : string

module Request : sig
  type action =
    | Register of Catalog.selection list
    | Readback

  type t = private
    { window : P.Id.Context_window.t
    ; key : Chatml_source.Digest.t
    ; action : action
    }

  val of_json : Jsonaf.t -> (t, P.Error.t) result
  val parameters : Jsonaf.t
end

module Receipt : sig
  type t =
    | Accepted
    | Stale
    | Absent
    | Uncertain
  [@@deriving equal]

  val to_json : t -> Jsonaf.t
end

type soft_note =
  { opportunity : P.Id.Operation.t
  ; created : Catalog.selection
  }
[@@deriving equal, sexp_of]

type t = private
  { principal : P.Id.Principal.t
  ; key : Chatml_source.Digest.t
  ; operation : P.Id.Operation.t
  ; invocation : P.Id.Invocation.t
  ; call : P.History.Id.t
  ; attempt_ordinal : int64
  ; source_revision : int64
  ; configuration_revision : int64
  ; prompt_revision : P.Id.Prompt_revision.t
  ; moderator : Chatml_source.Digest.t option
  ; catalog_revision : Catalog.Catalog_revision.t
  ; pending_revision : P.Pending_input.Revision.t
  ; instruction_revision : int64 option
  ; instruction_digest : Chatml_source.Digest.t option
  ; covered_rows : int
  ; soft_note : soft_note option
  }
[@@deriving equal, sexp_of]

val create
  :  principal:P.Id.Principal.t
  -> key:Chatml_source.Digest.t
  -> operation:P.Id.Operation.t
  -> invocation:P.Id.Invocation.t
  -> call:P.History.Id.t
  -> attempt_ordinal:int64
  -> source_revision:int64
  -> configuration_revision:int64
  -> prompt_revision:P.Id.Prompt_revision.t
  -> moderator:Chatml_source.Digest.t option
  -> catalog_revision:Catalog.Catalog_revision.t
  -> pending_revision:P.Pending_input.Revision.t
  -> instruction_revision:int64 option
  -> instruction_digest:Chatml_source.Digest.t option
  -> covered_rows:int
  -> (t, P.Error.t) result

val with_soft_note : t -> soft_note -> t
val same_key : t -> principal:P.Id.Principal.t -> key:Chatml_source.Digest.t -> bool

(** Matches the exact original, selected native model route and call occurrence.
    The actor or reducer separately checks the execution and its owning attempt. *)
val selected_invocation : P.Invocation.t -> bool
val selected_note_invocation : P.Invocation.t -> bool

val matches_invocation : t -> P.Invocation.t -> bool
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
