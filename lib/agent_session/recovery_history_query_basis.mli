(** Ordered structural evidence. The ledger admission caps bound this pass
    independently of the page's candidate and plaintext ceilings. *)
open! Core

module P = Agent_protocol

type family = P.Recovery_history_request.family =
  | Canonical
  | Effective

type scope =
  | Current_generation
  | Window of P.Recovery_window.Scope.t

module Highwater : sig
  type t =
    { windows : int
    ; last_window_members : int
    ; last_preparation_revision : int64 option
    }
end

type candidate =
  { selection : Recovery_item_text.selection
  ; ordinal : int
  ; attempt : Recovery_effective_attempt.t option
  ; is_selected : bool
  }

type t

type error =
  | Source of Recovery_source_reader.error
  | Refresh_required

val create
  :  Recovery_source_work.t
  -> Session_state.t
  -> owner:P.Session_ref.t
  -> scope:scope
  -> family:family
  -> ordinals:(int * int) option
  -> highwater:Highwater.t option
  -> (t, error) Result.t

val highwater : t -> Highwater.t
val digest : t -> Chatml_source.Digest.t
val length : t -> int
val has_unretained_sources : t -> bool
val at : t -> int -> candidate
val is_earliest : t -> candidate -> position:int -> bool
val attempt_evidence : Recovery_effective_attempt.t -> Jsonaf.t
