open! Core
module P = Agent_protocol
module DTO = P.Provider_operator

(** Trusted optional host port. Concrete provider/storage ownership stays below
    composition roots; no session authority or credential bytes in this surface.
    Factory runs once under host switch with actual initialized server identity.
    Failure must join/release newly acquired resources before returning Error. *)
type t

val create
  :  dispatch:
       (actor:Operator_authorization.t
        -> P.Command.t
        -> (P.Method_result.t, DTO.Error.t) Result.t)
  -> receipt:
       (actor:Operator_authorization.t
        -> P.Command.t
        -> (P.Command_receipt.t, DTO.Error.t) Result.t)
  -> close:(unit -> unit)
  -> t

type factory = sw:Eio.Switch.t -> server_id:P.Id.Server.t -> (t, P.Error.t) Result.t

val dispatch
  :  t option
  -> actor:Operator_authorization.t
  -> P.Command.t
  -> (P.Method_result.t, P.Error.t) Result.t

val receipt
  :  t option
  -> actor:Operator_authorization.t
  -> P.Command.t
  -> (P.Command_receipt.t, P.Error.t) Result.t

(** Permanent close first denies new dispatch/receipt admission, then serializes
    actual callback completion through a private close coordinator. Callback IO
    runs under cancellation protection; concurrent close callers join that same
    completion, and a failed callback retains Closing for an explicit retry.
    Original exceptions/backtraces propagate, never a fabricated successful close.
    The trusted callback must retain unfinished resources on failure, tolerate
    retries, and must not recursively close this same port. *)
val close : t -> unit

val protocol_error : DTO.Error.t -> P.Error.t
val is_provider_command : P.Command.t -> bool
