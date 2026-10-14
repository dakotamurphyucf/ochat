open Core

(** Trusted selected native executor issued from one physical developer root.
    The package binds actual actor/capture, stable invocation issuer, current
    selected registry and exact scope-bound Store writer. Caller coordinates
    cannot construct it, and an expiring child borrow cannot be used as Root. *)
type t

val create : root:Developer_root_owner.t -> (t, Agent_protocol.Error.t) result

(** Compose one physically issued developer Script Job service with this same
    selected executor; the ordinary no-Job caller keeps the original path. *)
val with_job_service
  :  t
  -> Developer_script_job_service.t
  -> (t, Agent_protocol.Error.t) result

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
    inherits any physical claimed-job ancestor, and narrows against that
    borrow's current capability ceiling. The child joins within the parent's
    Eio callback switch. *)
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

(** Direct child of the actual claimed worker callback owned by this root.
    The root derives an abstract job-parent proof from its real Worker issuer;
    the same selected registry, policy, dispatch and terminal path is used as
    [run_root]. Invocation context must name the exact claimed job and no
    parent invocation. This grants no authority from a saved Running row. *)
val run_job_child
  :  t
  -> active:Agent_session.Developer_job_scope.active
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
