(** Selected, authenticated native context effects on one actual session. *)
open! Core

module P = Agent_protocol

type effect_kind =
  | Remaining
  | New_window
[@@deriving equal]

val name : effect_kind -> string

type access

val capture
  :  Native_tool_invocation.Admission.t
  -> binding:Chat_response.Tool_capability.binding
  -> effect_kind:effect_kind
  -> (access, P.Error.t) result

val check_now
  :  access
  -> Session_state.t
  -> effect_kind:effect_kind
  -> (P.Id.Principal.t * P.Invocation.t, P.Error.t) result

type observation =
  { window : P.Recovery_window.Scope.t option
  ; revision : int64
  ; capacity : Session_context_capacity.status
  }

val to_json : observation -> Jsonaf.t

type t =
  { remaining : access -> (observation, P.Error.t) result
  ; new_window :
      access -> P.Id.Context_window.t -> (Recovery_rollover.Receipt.t, P.Error.t) result
  }
