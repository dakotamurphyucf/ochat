open! Core
open Authoring_context_service_core

(** Fresh host observations and immutable captured-source reads. Root and
    context affinity is repeated at final nonyielding publication. *)
val handle_roots
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result

val handle_host_list
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Source_read.Host_list_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result

val handle_host_read
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Source_read.Host_read_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result

val handle_source_list
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Source_read.List_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result

val handle_source_read
  :  t
  -> actor:Operator_authorization.t
  -> connection:Connection_context.t
  -> P.Authoring.Source_read.Read_request.t
  -> checkpoint:(unit -> unit)
  -> (W.Response.t, Error.t) result
