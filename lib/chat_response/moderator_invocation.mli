open Core
module I = Agent_protocol.Invocation
module R = Chatml_host_runtime
module L = Chatml.Chatml_lang

type t

(** Host-only dispatch scope. The invocation must already be dispatched under
    current owner policy. Its implementation revision is the prepared handler's
    fingerprint; its capability fingerprint is the selected registry fingerprint.
    This checks identity/input, not current authorization or actor ownership.
    [validate_work] must recheck that Pending refers to admitted work owned by
    this invocation's session/generation with a valid completion path. It must
    not publish or mutate state. *)
val create
  :  prepared:Extension_compiler.t
  -> invocation:I.t
  -> limits:Chatmd_shell_spec.Chatmd_script_spec.limits
  -> validate_work:(I.work -> (unit, string) result)
  -> (t, string) result

val event : t -> L.value
val invocation : t -> I.t

(** Add the v1 transactional resolution operation and JSON-only emit/timer
    adapters to the normal host registry. No tool implementation runs here. *)
val operations : R.op_def list -> R.op_def list

(** Normalize v1 buffered JSON emits for the legacy local-effect decoder. Rejects
    resolution outside a dispatched invocation. *)
val ordinary_effects : L.eff list -> (L.eff list, string) result

(** Run Tool_invoked on an already borrowed, serialized moderator runtime.
    Exactly one resolution is validated before the runtime commits state/effects.
    [prepare_commit] validates other effects and any host transaction, returning
    an infallible installer. No provider output or persistence is performed by
    this function itself. Failures leave buffered effects uncommitted. External
    work is never automatically retried or undone. Serializable moderator state
    is defensively copied and restored on failure; mutable globals are excluded.
    The caller owns serialization, cancellation and conversion to terminal
    failures. Pure evaluation interruption is not provided by task limits alone. *)
val run
  :  t
  -> runtime:R.session
  -> context:L.value
  -> prepare_commit:
       (resolved:I.t -> local_effects:L.eff list -> (unit -> unit, string) result)
  -> (I.t, string) result
