open! Core
module P = Agent_protocol
module Ledger = Chatml_authoring_context

(** Pure wire projection of a current admitted ledger value. The caller owns
    source authorization, the ledger publication gate, and the request's
    bounded lifetime. *)
val source_metadata
  :  Ledger.t
  -> Ledger.Identity.t
  -> Chatml_source.t
  -> (P.Authoring.Source.t, unit) result

val view_of_context
  :  host:P.Id.Server.t
  -> max_sources:int
  -> max_source_bytes:int
  -> max_context_source_bytes:int
  -> Ledger.t
  -> id:P.Id.Authoring_context.t
  -> identity:Ledger.Identity.t
  -> origin:P.Authoring.Context.origin
  -> checkpoint:(unit -> unit)
  -> (P.Authoring.Context.t, unit) result

val response_fits
  :  max_response_bytes:int
  -> max_protocol_bytes:int
  -> P.Authoring.Wire.Response.t
  -> bool
