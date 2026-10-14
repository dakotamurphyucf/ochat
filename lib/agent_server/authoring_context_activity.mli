open! Core
module P = Agent_protocol
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Upload = Authoring_upload_host

type phase =
  | Active
  | Revoking
  | Closed

type context_owner =
  | Host_project of Sources.Context.t
  | Host_document of Sources.Document_context.t
  | Uploaded_project of Upload.Context_capture.t
  | Supplied_document of Operator_authorization.t

type context_entry =
  { id : P.Id.Authoring_context.t
  ; identity : Ledger.Identity.t
  ; original_actor : Operator_authorization.t
  ; origin : P.Authoring.Context.origin
  ; owner : context_owner
  ; mutable view : P.Authoring.Context.t
  }

type connection_entry =
  { connection : Connection_context.t
  ; lifecycle_gate : Eio.Mutex.t
  ; mutable idle : unit Eio.Promise.t option
  ; mutable idle_waker : unit Eio.Promise.u option
  ; mutable negotiation : Connection_context.Authoring_negotiation.t option
  ; mutable phase : phase
  ; mutable active : Eio.Cancel.t list
  ; mutable roots : (P.Id.Authoring_root.t * Sources.Root.t) list
  ; mutable contexts : context_entry list
  ; mutable retired_owners : context_owner list
  }

val create : Connection_context.t -> connection_entry
val find_context : connection_entry -> P.Id.Authoring_context.t -> context_entry option
val valid_negotiation : connection_entry -> bool
val with_lifecycle : connection_entry -> (unit -> 'a) -> 'a
val in_active : connection_entry -> bool
val with_active : connection_entry -> (unit -> 'a) -> 'a
val close_owner : context_owner -> unit
