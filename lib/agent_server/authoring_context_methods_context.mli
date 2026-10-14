open! Core
open Authoring_context_service_core

(** Context and document mutation handlers. Each successful new generation is
    prepared outside the short gate and admitted by its final pure CAS. *)
val handle_context_open
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Context.Open_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result

val handle_context_refresh
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Context.Refresh_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result

val handle_context_close
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Context.Close_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result

val handle_document_version
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Document.Version_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result

val handle_document_replace
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Document.Replace_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result

val handle_document_close
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Document.Close_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result
