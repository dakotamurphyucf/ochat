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
  -> current_model_bridge:
       (unit -> (Inference_host.Credential_bridge.t, DTO.Error.t) Result.t)
  -> close:(unit -> unit)
  -> t

module Model_selection : sig
  type t

  (** The exact authenticated actor token used for the bridge's principal-scoped
      inference authorization. A copied principal ID is not equivalent. *)
  val actor : t -> Operator_authorization.t

  (** Complete nonsecret host-selected target. Neither script arguments nor a
      conversation service principal can choose this binding. *)
  val target : t -> Inference.Request.Target.t

  (** Borrow the original Context issued by this selected bridge after an
      actual currentness check. The value is configuration/data, not an
      enduring credential or effect lease. The caller must retain this
      selection and recheck physical authority at every effect boundary. *)
  val context : t -> (Inference_runtime.Context.t, P.Error.t) result

  (** Nonyielding. Requires this selection's physical port still Open, its
      original actor current, the trusted current bridge getter still returning
      the same physical bridge, and same-target Context.derive still accepting
      the binding. Direct runtime close therefore invalidates retained
      selections even if a port handle outlives it. This checks current nonsecret
      provider eligibility; execution obtains its secret lease separately. *)
  val check_current : t -> (unit, P.Error.t) result
end

(** The target is supplied by trusted host configuration, never by script
    payload. The port asks its runtime for the actual current Opened bridge,
    resolves for [(Operator_authorization.principal actor).id] via that bridge
    (operation Inference), and rechecks port, actor, bridge identity and binding
    after the yielding resolution before issuing an opaque selection. A port
    created before initial provider setup can use the bridge installed later.
    Missing setup, denied actor and close reject with typed errors; there is no
    service-principal fallback or generic authorization Boolean callback. *)
val select_model
  :  t
  -> actor:Operator_authorization.t
  -> target:Inference.Request.Target.t
  -> (Model_selection.t, P.Error.t) result

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
