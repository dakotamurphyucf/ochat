open Core

(** Reconstructible, non-executable requests for owned tool/script jobs. These
    values contain JSON, source, policy and stable configuration pins, never live
    runners, capability IDs, closures or invocation borrows. They are not grants:
    only a host-owned persisted job may supply them to worker admission. Session,
    generation, parent, deadline and attempt ownership belong to that job. *)
type t

(** Capture exactly one already selected tool, checking its live reference and
    input schema. Managed implementations retain their full revision pin, including
    the dependencies captured by their host registration. Does not execute. *)
val tool
  :  capabilities:Tool_capability.t
  -> reference:Tool_capability.reference
  -> input:Jsonaf.t
  -> policy:One_off_request.policy
  -> (t, Agent_protocol.Error.t) result

(** Capture the exact source and selected authority of a statically prepared
    one-off script. No compilation or initializer execution occurs here. *)
val script
  :  prepared:One_off_script.t
  -> input:Jsonaf.t
  -> policy:One_off_request.policy
  -> (t, Agent_protocol.Error.t) result

val to_json : t -> Jsonaf.t

(** Effective stored budget, retained independently of later host defaults. *)
val policy : t -> One_off_request.policy

(** Strict versioned decoding and resource validation. Stored limits must still
    fit the current host ceiling; changing a default cannot enlarge old work.
    Decoding does not resolve tools, compile source or confer execution authority. *)
val of_json
  :  policy:One_off_request.policy
  -> Jsonaf.t
  -> (t, Agent_protocol.Error.t) result

(** Stable content identity, including input, source/contract, authority and all
    resource limits. A digest is an audit pin, not proof of authorization. *)
val fingerprint : t -> string

(** Stable configuration pins for a host-selected ceiling, independent of live
    capability IDs. Rebinding selects exactly those names and checks each actual
    permission fingerprint. Extra current registrations never widen the result;
    missing or changed bindings fail. These functions do not execute tools. *)
val capability_pins
  :  Tool_capability.t
  -> ((string * string) list, Agent_protocol.Error.t) result

val rebind_capabilities
  :  pins:(string * string) list
  -> capabilities:Tool_capability.t
  -> (Tool_capability.t, Agent_protocol.Error.t) result

(** Pure authority-pin check, without compiling or executing the saved request.
    A job ID alone does not grant access to a different script's selected tools. *)
val validate_capabilities
  :  t
  -> capabilities:Tool_capability.t
  -> (unit, Agent_protocol.Error.t) result

type execution = private
  | Tool of
      { capabilities : Tool_capability.t
      ; reference : Tool_capability.reference
      ; input : Jsonaf.t
      ; policy : One_off_request.policy
      }
  | Script of
      { prepared : One_off_script.t
      ; input : Jsonaf.t
      ; policy : One_off_request.policy
      }

(** Trusted worker reconstruction after job ownership/capacity admission.
    Explicitly re-admits the pinned configuration against the current registry;
    same-name replacements with different owner, resources, interface, metadata
    or implementation fail. Additional current tools are never inherited.
    Script recompilation checks the pinned compiler contract and revalidates live
    bindings after the domain wait. Fresh executions must still use the actor's
    job-owned invocation service and current per-call policy/moderation/disclosure.
    This function neither starts a worker nor revives a caller's expired scope. *)
val prepare
  :  env:Eio_unix.Stdenv.base
  -> current_capabilities:(unit -> Tool_capability.t)
  -> policy:One_off_request.policy
  -> t
  -> (execution, Agent_protocol.Error.t) result
