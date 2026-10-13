open Core

(** Trusted selected native executor issued from one physical developer root.
    The package binds actual actor/capture, stable invocation issuer, current
    selected registry and exact scope-bound Store writer. Caller coordinates
    cannot construct it, and an expiring child borrow cannot be used as Root. *)
type t

val create : root:Developer_root_owner.t -> (t, Agent_protocol.Error.t) result

(** Function, Custom and owned managed calls use one existing native policy
    kernel. The host creates one bounded registered terminal scope per call;
    Dispatching is durable before effect and a terminal plus exact selected
    children is submitted through the root's one queue. Uncertain dispatch or
    terminal saves retain physical custody without handler replay. *)
val run_root
  :  t
  -> managed:Agent_session.Native_tool_invocation.selected_managed_dispatch option
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Host_invocation.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result

(** Direct child of the current selected native callback. Obtains the live
    physical parent proof from its expiring borrow, verifies this exact issuer,
    and narrows against that borrow's current capability ceiling. The child
    must join within the parent's Eio callback switch. Bare Job_child remains
    unavailable until claimed-worker integration. *)
val run_child
  :  t
  -> managed:Agent_session.Native_tool_invocation.selected_managed_dispatch option
  -> reference:Chat_response.Tool_capability.reference
  -> invocation:Agent_protocol.Host_invocation.t
  -> authorize:
       (Agent_protocol.Host_invocation.t
        -> Chat_response.Tool_capability.binding
        -> (unit, Agent_protocol.Error.t) result)
  -> prepare_output:
       (Openai.Responses.Tool_output.Output.t
        -> (Jsonaf.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Host_invocation.t, Agent_protocol.Error.t) result
