open Core

(** Reconstructible, non-executable requests for owned tool/script jobs. These
    values contain JSON, source, policy and stable configuration pins, never live
    runners, capability IDs, closures or invocation borrows. They are not grants:
    only a host-owned persisted job may supply them to worker admission. Session,
    generation, parent, deadline and attempt ownership belong to that job. *)
type t

module Agent_artifact : sig
  (** Immutable reference to selected child code, not a source grant, native
      resource, compiled state or reservation. Validated domain types bound the
      revision ID, digest and exact portable root member. The trusted host must
      install and verify that artifact, its authentic role and its association
      with the original native binding before attaching this reference. *)
  type t

  val create
    :  revision_id:Agent_protocol.Id.Prompt_revision.t
    -> manifest_sha256:Chatml_source.Digest.t
    -> root_relative_path:Source.Portable_path.t
    -> t

  val revision_id : t -> Agent_protocol.Id.Prompt_revision.t
  val manifest_sha256 : t -> Chatml_source.Digest.t
  val root_relative_path : t -> Source.Portable_path.t
end

(** Trusted attachment to an already admitted Tool request. Reject Script requests
    and replacement of a selection. Original target, input, budget and permission
    pins remain unchanged. Structural admission does not prove code/binding or
    filesystem authority. No new source-mode/default policy is selected here. *)
val with_agent_artifact : t -> Agent_artifact.t -> (t, Agent_protocol.Error.t) result

val agent_artifact : t -> Agent_artifact.t option

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

(** A present code reference uses the strict v2 carrier and participates in the
    fingerprint. Absence retains exact existing v1 encoding and fingerprint. *)
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

(** Rebind this exact saved request's selected names and permission fingerprints
    against an independently current registry. The result is a ceiling for the
    same request, never the host's full registry. This does not compile or
    execute the request, and it confers no Job claim or source authority. *)
val selected_capabilities
  :  t
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
