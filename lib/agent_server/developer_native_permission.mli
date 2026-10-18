open! Core
module P = Agent_protocol

(** Private Host choice. Explicit callbacks preserve trusted embedding behavior;
    Selected_prompt_profile uses the configured policy of the genuinely admitted
    Live Prompt. Neither mode grants source or execution authority. *)
module Mode : sig
  type authorizer =
    P.Host_invocation.t
    -> Chat_response.Tool_capability.binding
    -> (unit, P.Error.t) result

  type t =
    | Explicit_callback of authorizer
    | Selected_prompt_profile
end

(** One immutable profile/revision pinned under the same catalog admission as
    Live Prompt and Workspace selection. A profile ID is comparison data, not
    a source grant. A missing profile rejects selected-mode preparation. *)
type t

val pin
  :  catalogs:Session_factory.Developer_catalog.t
  -> profile_id:string
  -> (t, P.Error.t) result

(** Compare the current catalog entry's ID/revision to the pinned profile.
    Replacement, removal, uncertain publication or foreign-domain use denies;
    this does not renew a Source/Workspace/Root lease. *)
val check_current
  :  t
  -> catalogs:Session_factory.Developer_catalog.t
  -> (unit, P.Error.t) result

(** Called only by the physically paired Live/Root authorizer. [runtime] is the
    actual Root, prepared selected-child or restored claimed runtime admitted
    by that caller. A native binding must resolve to the same physical
    implementation; a managed binding must resolve to its exact selected
    target and permission identity. A Shell exemption applies only to a native
    binding in that runtime's [shell_tool_names]; a root name alone cannot
    classify a child's tool.
    Shell still requires the independent exact Manifest_grant and Once broker.

    Other tools use the existing profile decision, stable permission identity
    and redacted display. No generic human responder exists, so Ask uses its
    configured fallback. Request_review calls the pinned owner-neutral reviewer
    once; Allow applies only to this invocation, Deny/error rejects, and Eio
    cancellation propagates. [check_current] is invoked before and after any
    yielding evaluator/reviewer, including an allow result. The existing native
    kernel then re-resolves the selected binding and runs its final Root/child/
    Claim effect gate; this function grants no reusable approval or Session row. *)
val authorize
  :  t
  -> runtime:Chat_response.Agent_runtime.t
  -> check_current:(unit -> (unit, P.Error.t) result)
  -> Mode.authorizer
