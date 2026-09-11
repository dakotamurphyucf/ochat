open Core

(** Transport-independent request adapter over the persisted-session services.
    Operation selection is separate from the child's delegated tool selection. *)
type operation =
  | Create
  | Send
  | Read
  | Status
  | Wait
  | Stop
[@@deriving equal, sexp]

val operation_to_string : operation -> string
val operation_of_json : Jsonaf.t -> (operation, Agent_protocol.Error.t) result

type t

(** Trusted host construction only. [allowed] must come from the admitted tool or
    authenticated helper grant, never from the request payload. The borrow supplies
    the actual session/generation/tool ceiling and expires with its owner. Services
    must belong to that caller and perform their usual current relationship checks.
    This adapter neither authenticates transport peers nor admits an invocation;
    the host must establish both before lending it to a caller. *)
val create
  :  borrowed:Native_tool_invocation.borrowed
  -> allowed:operation list
  -> creation:Generated_session_request.service option
  -> sessions:Managed_session_service.t option
  -> t

(** Shared strict argument decoding and dispatch. No native tool registration or
    ambient CLI credential is consulted. Denied operations and expired borrows
    fail before any service call, including when the target ID is unknown. *)
val run
  :  t
  -> operation
  -> Jsonaf.t
  -> (Jsonaf.t, Agent_protocol.Invocation.tool_error) result

(** Version-1 envelope: [{"version":1,"operation":"read","arguments":{...}}].
    Rejects missing, duplicate and unknown fields and unsupported versions.
    Arguments use exactly the same decoder as the corresponding native tool.
    The envelope contains no caller identity, capability selection or credentials. *)
val dispatch : t -> Jsonaf.t -> Agent_protocol.Invocation.outcome

(** Decode the versioned envelope without services, authority or effects.
    Operation-specific arguments are still validated by [run]. This is not
    authorization and does not produce an executable capability. *)
val decode_request
  :  Jsonaf.t
  -> (operation * Jsonaf.t, Agent_protocol.Invocation.tool_error) result
