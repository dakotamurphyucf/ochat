open Core

(** Readonly services shared with the inherited helper transport. Construction
    captures the owning runtime's actual validation target and compiler environment;
    request data cannot replace either. Documentation is loaded lazily from the
    installed corpus. No native helper tool needs to be registered. *)
type t

val create : env:Eio_unix.Stdenv.base -> host:Chat_response.Authoring_validation.host -> t

(** Both operations recheck the live borrow before work and before disclosure.
    Reference cursors bind session/generation, selected capabilities, target and
    corpus; replacing this service invalidates its cursors. Validation executes
    no submitted code, tools or model requests. *)
val reference
  :  t
  -> Native_tool_invocation.borrowed
  -> Jsonaf.t
  -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result

val validate
  :  t
  -> Native_tool_invocation.borrowed
  -> Jsonaf.t
  -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result
