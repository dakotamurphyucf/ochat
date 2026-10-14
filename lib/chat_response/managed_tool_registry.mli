open Core

(** Captured managed definitions and real base capabilities, compiled without
    initialization, resource construction or effects. Native and managed targets
    remain distinct; no no-op native runners are installed. *)
type t

(** Validates source/schema digests, handler kinds, exact uses dependencies and
    definition cycles before binding and compilation. The managed permission
    identity includes all captured scripts/tools/help and base permission identities;
    this conservatively invalidates grants when the definition's authority changes.
    Native bindings retain their original live identities and metadata; help
    declarations cannot retag existing capabilities. [owner] is supplied by
    the owning host; reference JSON cannot register or authorize targets.

    Uses existing definition limits and Eio-managed compilation. This preparation
    does not expose tools, initialize a moderator or install invocation dispatch.
    The host must use the matching prepared definitions under actor ownership and
    current permission/disclosure policy, including revalidation after waits.
    [delegated_moderator] selects the restricted moderator compiler contract and
    includes it in the managed authority identity; defaults to false.
    [tool_contract] selects the actual static tool compiler contract; default
    Standard preserves ordinary Tool_v1. This choice is included in authority
    identity and retained by [rebind_code], never inferred from source text. *)
val prepare
  :  ?startup:Startup_compilation.t
  -> ?limits:Chatml_compilation.limits
  -> ?delegated_moderator:bool
  -> ?tool_contract:Chatml_compilation.Tool_contract.t
  -> ?programs:Chatml_prompt_programs.Restored.t
  -> env:Eio_unix.Stdenv.base
  -> owner:string
  -> capabilities:Tool_capability.t
  -> Prompt.Chat_markdown.top_level_elements list
  -> (t, Chatmd_shell_spec.Diagnostic.t list) result

(** Execution revision selected by this immutable registry for an exact binding.
    Original registries preserve the existing declaration revision; a rebound
    registry returns the selected handler fingerprint. This does not change the
    binding, permission identity or authority fingerprint. *)
val execution_revision
  :  t
  -> Tool_capability.binding
  -> (string, Tool_capability.error) Result.t

val capabilities : t -> Tool_capability.t
val definition : t -> Extension_compiler.definition
val authority_fingerprint : t -> string

(** Compile replacement authored code over this exact registry's existing native
    and managed capability identities. The caller has admitted the selected
    artifact and authentic role. Validate unchanged managed tool declarations,
    exact schemas, targets and uses, plus lifecycle script identities/kinds/limits
    before compiling. Bodies and selected project graphs may change; diagnostics
    may rebase. No initializer, native acquisition or capability registration.

    Existing capability/permission identities and authority_fingerprint remain
    unchanged. The returned definition/handlers carry their actual new executable
    fingerprints. Rebinding retains this registry's original tool compiler
    contract; it cannot widen or silently retarget the compiled surface.
    The owning host must authorize and dispatch the selected code
    under its original resource/actor checks. Borrowed startup/checkpoint cover
    the whole comparison and compilation phase and never escape. Original
    exceptions and cancellation propagate. *)
val rebind_code
  :  t
  -> startup:Startup_compilation.t
  -> limits:Chatml_compilation.limits
  -> delegated_moderator:bool
  -> programs:Chatml_prompt_programs.Restored.t option
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> Prompt.Chat_markdown.top_level_elements list
  -> (t, Chatmd_shell_spec.Diagnostic.t list) Result.t

(** Resolve the exact live binding, never a same-name target. *)
val resolve
  :  t
  -> Tool_capability.binding
  -> (Extension_compiler.t, Tool_capability.error) result

(** Revalidate this definition's captured registry; extra current bindings cannot
    widen it, and removed/re-registered bindings invalidate it. *)
val revalidate : t -> current:Tool_capability.t -> (unit, Tool_capability.error) result

(** Checked standalone delegation view. The public selection stays distinct from
    its transitive private execution dependencies. Every binding is resolved by
    exact identity in the original registry; no tools, resources, scripts or
    moderator state are reconstructed or initialized. Stateful dependencies reject
    until an original-owner dispatch service is supplied. The host still owns
    live authority checks, permission policy, lifetime and actual dispatch. *)
type delegation

val delegate_standalone
  :  t
  -> selected:Tool_capability.t
  -> (delegation, Tool_capability.error) result

val delegation_selection : delegation -> Tool_capability.t

(** Execution registry contains only the selected bindings and their captured
    dependency closure. It must never replace the child's advertised/borrowed
    public selection. Existing admission and revalidation apply to this registry. *)
val delegation_registry : delegation -> t

(** Only publicly selected standalone handlers, with no parent lifecycle scripts.
    Transitive private handlers remain available through the execution registry. *)
val delegation_definition : delegation -> Extension_compiler.definition

(** Private dependency ceiling of an actor-verified running handler. Supports
    persisted model standalone and nested managed invocation identities. The host
    must supply the actual dispatched actor record and verify its owner/session/
    generation; arbitrary caller records do not prove ownership. This function
    checks implementation identity and grants no execution or lifecycle authority. *)
val delegated_invocation_dependencies
  :  delegation
  -> Agent_protocol.Invocation.t
  -> (Tool_capability.t, Tool_capability.error) result

(** Verified link between a dispatched call's selected managed capability and
    that capability's captured implementation. Its private dependencies belong
    to this implementation, not to the calling script's tool selection. *)
type execution

(** Same admission checks as [execution], with an owner-neutral invocation.
    The conversation API and its consumers retain their original exact type. *)
type owned_execution

(** Checks live captured authority, exact caller selection and dispatched source
    identity. This performs no authorization or execution; the owning dispatcher
    must repeat admission after any permission wait. *)
val admit
  :  t
  -> current:Tool_capability.t
  -> selected:Tool_capability.t
  -> reference:Tool_capability.reference
  -> invocation:Agent_protocol.Invocation.t
  -> (execution, Tool_capability.error) result

val prepared : execution -> Extension_compiler.t
val binding : execution -> Tool_capability.binding
val invocation : execution -> Agent_protocol.Invocation.t

(** Exact developer/owned admission. No provider-history or Session ID is
    synthesized. The implementation shares the binding, revision, selection,
    source and live-registry validation used by [admit]. *)
val admit_owned
  :  t
  -> current:Tool_capability.t
  -> selected:Tool_capability.t
  -> reference:Tool_capability.reference
  -> invocation:Agent_protocol.Host_invocation.t
  -> (owned_execution, Tool_capability.error) result

val owned_prepared : owned_execution -> Extension_compiler.t
val owned_binding : owned_execution -> Tool_capability.binding
val owned_invocation : owned_execution -> Agent_protocol.Host_invocation.t

val delegated_owned_invocation_dependencies
  :  delegation
  -> Agent_protocol.Host_invocation.t
  -> (Tool_capability.t, Tool_capability.error) result
