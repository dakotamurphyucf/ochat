open Core
module Spec = Chatmd_shell_spec.Extension_spec
module Metadata = Chatmd_shell_spec.Authoring_metadata

type t

(** Attach authored help only to actual expanded registrations. Helper roles and
    existing host metadata cannot be replaced by authored declarations. Exact
    callable names are used, without namespace inference or tool construction.
    Source provenance participates in identity. This performs no tool effects or
    context injection; the owning host must resolve authoring policy before
    exposing the manifest. Do not apply to an inherited registry: inherit its
    existing bindings and metadata through selection instead. Host-supplied
    [delegation_restrictions] are retained in capability identity independently
    of authored help; their reasons must be safe public diagnostics. *)
val create
  :  ?host_metadata:(string * Metadata.t) list
  -> ?result_contracts:(string * Tool_capability.result_contract) list
  -> ?delegation_restrictions:(string * string) list
  -> declarations:Spec.authoring_help list
  -> owner:string
  -> resource_fingerprint:string
  -> (string * Ochat_function.t) list
  -> (t, Tool_capability.error) result

val capabilities : t -> Tool_capability.t
val sources : t -> (string * Chatmd_shell_spec.Source_ref.t) list

(** Register metadata and resolve policy as one non-executing admission step.
    [registrations] is the already host-approved ceiling, including any authentic
    helpers; [selected_names] is the initial requested selection. The returned
    plan describes the effective manifest and required context work. A host must
    fulfill that plan before exposing tools, not merely advertise its intent. *)
val resolve
  :  ?host_metadata:(string * Metadata.t) list
  -> ?result_contracts:(string * Tool_capability.result_contract) list
  -> ?delegation_restrictions:(string * string) list
  -> ?context:Spec.authoring_context
  -> ?catalog:Authoring_policy.catalog
  -> declarations:Spec.authoring_help list
  -> owner:string
  -> resource_fingerprint:string
  -> registrations:(string * Ochat_function.t) list
  -> selected_names:string list
  -> unit
  -> (t * Authoring_policy.t, Tool_capability.error) result
