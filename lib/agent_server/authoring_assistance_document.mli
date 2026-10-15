open! Core
module Owner = Authoring_context_service_core
module Wire = Agent_protocol.Authoring_assistance.Assistance
module Limits = Authoring_assistance_admission.Limits
module Selected_tools = Authoring_assistance_admission.Selected_tool_policy

(** A prepared answer to one current Document assistance request. Its response
    was built only from the exact authored source, optional retained Parsed
    capture, and currently admitted selections. This value owns comparison
    evidence, not an import loader or source/read grant. *)
type t

val result : t -> Wire.Result.t

(** Called in [Authoring_context_owner.with_current]'s final nonyielding check,
    after all capture, scan, path-list and response work. It rechecks only
    selections actually used in [result]: the authentic selected script/Build,
    decoded declaration project-metadata route, the raw selected-script policy
    witness even when a mismatching Build is downgraded to metadata-only,
    selected tool registry, and
    any host-root path grant. All arguments were
    frozen while [Owner.Current] was live; this never touches the expired token,
    reparses, hashes an arbitrary contract, reads a source, waits or yields.
    The enclosing owner independently rechecks actor/context/source/generation
    and its one retained Parsed head before pure publication. *)
val check_now : t -> (unit, Owner.Error.t) result

(** Produce local ChatMD tag/attribute/value/template help from exact authored
    bytes even when no Parsed capture exists. A genuine current capture may add
    source-owned references for root or imported inputs; imported requests use
    only actual input/source pairs and repeated occurrences remain distinct.
    The shared selected-script policy can add a current Build/contract only for
    an actual declaration. A mapped ChatML body result requires the genuine
    Script extraction map and both exact authored/compiled point and range
    proof; unsupported/ambiguous/external provenance stays unavailable.
    Project environment facts may come from the one actual
    [Current.project_capture] for this exact Parsed/declaration, even if no
    selected Build checked successfully. The shared project-metadata policy
    reauthorizes its route and actors before use and after yielding work; the
    decoded receipt itself grants no read or selection authority. When a
    selected Build exists, its policy receives the same metadata and must
    validate its actual route; content comparison is bounded and pure.
    Host-root path names require separate bounded source-root listing;
    no dependency contents are read. Each stage uses fixed nonresetting limits,
    with final wire bytes checked by the enclosing service. [Current] is borrowed
    solely while this call runs. Callback failures and cancellation propagate. *)
val prepare
  :  Owner.Current.t
  -> actor:Operator_authorization.t
  -> request:Wire.Request.t
  -> script_selection:Authoring_script_selection.Policy.t option
  -> project_metadata:Authoring_project_metadata.Policy.t option
  -> selected_tools:Selected_tools.t option
  -> project_build_limits:Chatml_project_build.Limits.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Owner.Error.t) result
