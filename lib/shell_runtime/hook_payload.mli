open! Core

(** JSON encodings of the same typed values used by ChatML shell extensions. *)

val context
  :  ?policy:Shell_access.Policy.decision
  -> event:string
  -> Shell_access.Context.t
  -> Jsonaf.t

val approval_request : Shell_access.Approval.request -> Jsonaf.t
val command_result : Shell_access.Interceptor.command_result -> Jsonaf.t

val audit_event
  :  secret_filter:Shell_access.Secret_filter.t
  -> Shell_access.Audit.envelope
  -> Jsonaf.t
