open! Core
open Authoring_context_service_core

(** Thin dispatch over the eleven installed source/context method producers.
    Other authoring families are installed by their separate owner. *)
val handle_core
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> W.Request.t
  -> (W.Response.t, Error.t) result
