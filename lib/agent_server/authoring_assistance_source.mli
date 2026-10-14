open! Core
module Current = Authoring_context_service_core.Current
module Ledger = Chatml_authoring_context
module Wire_source = Agent_protocol.Authoring.Source
module Error = Authoring_context_service_core.Error

type basis =
  | Project of Ledger.Inputs.t
  | Document of Ledger.Document.Inputs.t

(** Resolve only an EXACT current captured revision from the borrowed Core
    context, including its current editor revision. No native read, host root,
    parser or compiler step occurs here. Context generation and actor authority
    are checked again by the owning service before response publication. *)
val admit
  :  Current.t
  -> source:Wire_source.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_source.t * basis, Error.t) result
