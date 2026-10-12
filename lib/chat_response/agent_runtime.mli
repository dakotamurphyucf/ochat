open! Core

(** Compiles one parsed ChatMD document into its complete live tool runtime. *)

type diagnostic =
  { code : string
  ; message : string
  ; source : Chatmd_shell_spec.Source_ref.t option
  }
[@@deriving sexp, compare, equal]

(** [diagnostic_to_string diagnostic] renders a stable startup error. *)
val diagnostic_to_string : diagnostic -> string

(** [default_home env] returns the host home directory when [HOME] is set,
    otherwise the Eio current working directory. *)
val default_home : Eio_unix.Stdenv.base -> Eio.Fs.dir_ty Eio.Path.t

type t =
  { functions : Ochat_function.t list
    (** Lazily bind the exact constructed implementations, including dynamically
      expanded MCP names. Forcing requires the host's secure ID generator.
      Selection never rebuilds tool declarations; invocation policy remains
      the owning service's responsibility. *)
  ; native_registrations : Native_tool_pool.Registration.t list
    (** Exact original source associations. Native inheritance filters actual
        function identity; it never discovers source authority by name or JSON. *)
  ; capabilities : (Tool_capability.t, Tool_capability.error) result Lazy.t
    (** Extension preparation includes source-bound managed capabilities here;
        [functions] continues to contain only actual native implementations. *)
  ; classifications : (string * Tool_execution_event.agent_page_kind) list
  ; shell_tool_names : String.Set.t
  ; shell_registry : Shell_runtime.Registry.t option
  ; shell_manifest : Chatmd_shell_spec.Manifest.t option
  ; shell_admin_policy : Shell_runtime.Admin_policy.t option
  ; shell_security_status : Shell_runtime.Manifest_security.status option
  ; moderator_shell_runtime : string option
  ; legacy_moderator : Moderator_manager.Registry.artifact option
    (** Compiled before native activation against this host's actual surface and
        complete tool pool. Later manager construction consumes this artifact
        without recompiling or initializing here. Native inheritance clears it. *)
  }

type shell_inspection =
  { manifest : Chatmd_shell_spec.Manifest.t
  ; live_runtimes : Chatmd_shell_spec.Shell_spec.t list
  ; administrative_policy : Shell_runtime.Admin_policy.t
  ; security_status : Shell_runtime.Manifest_security.status
  }

type resource_inspection =
  { manifest : Chatmd_shell_spec.Manifest.t
  ; live_runtimes : Chatmd_shell_spec.Shell_spec.t list
  ; administrative_policy : Shell_runtime.Admin_policy.t
  }

(** Complete requested execution identity remains inspectable even if the current
    signature/trust check rejects. This is neither a manifest grant nor runtime
    activation; an operator can use the actual canonical identity/signature
    payload before installing its signature. *)
type prepared_shell_inspection =
  { manifest : Chatmd_shell_spec.Manifest.t
  ; live_runtimes : Chatmd_shell_spec.Shell_spec.t list
  ; administrative_policy : Shell_runtime.Admin_policy.t
  ; security_check :
      ( Shell_runtime.Manifest_security.status
        , Shell_runtime.Manifest_security.error list )
        Result.t
  }

(** Pure declaration preparation shared by source planning and runtime inspection.
    Returns the actual manifest compiler's transient material so callers can
    select effective scripts and invocation contracts. Does not consult host
    policy, acquire source authority, authorize tools or run initializers.
    Material may contain private declaration data: never persist it in program
    custody or expose it as public authoring metadata. *)
val prepare_shell_manifest
  :  platform:Chatmd_shell_spec.Shell_spec.platform
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> ( Chatmd_shell_spec.Manifest.t * Chatmd_shell_spec.Manifest_compiler.material
       , diagnostic list )
       result

(** [inspect_shell ~env ~platform ~prompt_elements] evaluates requested shell
    declarations against host administrative and trust policy without
    authorizing a manifest or instantiating any executable runtime. This legacy
    text inspection rejects project-selected hooks, whose complete security
    identity requires [inspect_prepared_shell] and authentic captured programs. *)
val inspect_shell
  :  env:Eio_unix.Stdenv.base
  -> platform:Chatmd_shell_spec.Shell_spec.platform
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> (shell_inspection, diagnostic list) result

(** Resource-only declaration/admin inspection for capture before prepared
    programs exist. No signature status is asserted for uncompiled dependencies.
    No initializer, native setup, provider or source acquisition. *)
val inspect_resources
  :  env:Eio_unix.Stdenv.base
  -> platform:Chatmd_shell_spec.Shell_spec.platform
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> (resource_inspection, diagnostic list) Result.t

(** Prepare through the same authentic Shell_preparation owner as activation,
    including every custom matcher alternative, without native resource setup,
    tool authorization or initialization. Supplied captured programs are the only
    configured source inventory; missing configured custody rejects without text
    fallback. Projection/serialization share the borrowed startup deadline and
    exact metadata/input allowances. Security outcome cannot hide the complete
    requested identity. No original source lookup, provider or process effects. *)
val inspect_prepared_shell
  :  ?startup:Startup_compilation.t
  -> ?programs:Chatml_prompt_programs.Restored.t
  -> env:Eio_unix.Stdenv.base
  -> limits:Chatml_compilation.limits
  -> checkpoint:(unit -> unit)
  -> platform:Chatmd_shell_spec.Shell_spec.platform
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> unit
  -> (prepared_shell_inspection, diagnostic list) Result.t

(** [platform ()] returns the manifest platform for the current host. *)
val platform : unit -> Chatmd_shell_spec.Shell_spec.platform

(** [moderator_process_handler t] returns a ChatML [Process.run] handler only
    when the manifest explicitly associates the moderator with a shell
    runtime. *)
val moderator_process_handler
  :  t
  -> (Chatml_host_runtime.session
      -> command:string
      -> args:Chatml.Chatml_lang.value
      -> (string, string) result)
       option

(** [host ~env ~workspace ~tool_dir ~prompt_dir ~session_dir ~cache_dir ~home
    ~session_id ~prompt_elements] creates the explicit host
    capabilities used by shell runtime instantiation. Source-relative roots
    are derived only from declaration provenance retained by the ChatMD
    parser. *)
val host
  :  env:Eio_unix.Stdenv.base
  -> workspace:Eio.Fs.dir_ty Eio.Path.t
  -> tool_dir:Eio.Fs.dir_ty Eio.Path.t
  -> prompt_dir:Eio.Fs.dir_ty Eio.Path.t
  -> session_dir:Eio.Fs.dir_ty Eio.Path.t
  -> cache_dir:Eio.Fs.dir_ty Eio.Path.t
  -> home:Eio.Fs.dir_ty Eio.Path.t
  -> session_id:string
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> (Shell_runtime.Host.t, diagnostic list) result

(** Inspect configured read-file roots using existing declaration provenance and
    the same resolver as live construction. Does not initialize scripts, authorize
    manifests, discover MCP tools or perform model/process effects. Other runtime
    construction errors may still arise after this limited preflight succeeds. *)
val validate_tool_paths
  :  Shell_runtime.Host.t
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> (unit, diagnostic list) result

(** [create ~sw ~ctx ~host ~platform ~prompt_elements ~manifest_authorizer
    ~approval_provider ~run_agent] compiles and authorizes every shell
    declaration before exposing any declared tool. Legacy command tools are
    desugared into manifest-bound fixed shell tools.

    Documents without shell or legacy command declarations do not invoke the
    manifest authorizer and return no shell registry. All resources remain
    owned by [sw]. [native_service_revision] optionally binds trusted host-service
    policy to resource/capability identity. Hosts restoring those resources must
    supply the same policy identity; omission preserves legacy fingerprints.
    [programs] supplies already restored captured scripts, including configured
    legacy moderators. Preparation shares one compilation deadline and distinct
    input allowances; executable activation runs after preparation. [startup]
    borrows the host's existing preparation context, including time already spent
    planning/restoring scripts. It requires the same original [compilation_limits]
    and remains owned by the enclosing [Startup_compilation.with_context]. *)
val create
  :  ?native_service_revision:string
  -> ?startup:Startup_compilation.t
  -> ?agent_source_runner:Tool.Agent_source_runner.t
  -> ?programs:Chatml_prompt_programs.Restored.t
  -> ?moderator_surface:Chatml.Chatml_builtin_surface.surface
  -> ?compilation_limits:Chatml_compilation.limits
  -> ?checkpoint:(unit -> unit)
  -> sw:Eio.Switch.t
  -> ctx:Eio_unix.Stdenv.base Ctx.t
  -> host:Shell_runtime.Host.t
  -> platform:Chatmd_shell_spec.Shell_spec.platform
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> ?extension_snapshots:Session.Shell_state.Extension_snapshot.t list
  -> ?persist_extension_snapshots:
       (Session.Shell_state.Extension_snapshot.t list -> (unit, string) result)
  -> run_agent:
       (?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
        -> ?session_id:string
        -> ?observer:Agent_response_loop.observer
        -> source:string
        -> ctx:Eio_unix.Stdenv.base Ctx.t
        -> string
        -> Prompt.Chat_markdown.content_item list
        -> string)
  -> unit
  -> (t, diagnostic list) result

(** Native resources plus the complete captured, non-evaluated extension
    definition. Extension declarations are not backed by a dummy native runner
    and are not added to [native.functions]. The owning session host must install
    the prepared dispatcher and moderator event services before advertising them. *)
type extension_resources =
  { native : t
  ; definition : Extension_compiler.definition
  ; managed : Managed_tool_registry.t
  }

(** Select exact live native bindings from an already constructed parent runtime.
    Retains their runners, resource context, shell redaction and classifications;
    constructs no resources and exposes no unselected tools. [managed] retains
    checked standalone handlers and their private dependency closure; the host
    must install their inherited dispatcher. Stateful dependencies reject. Private
    shell classification is retained for execution, without advertising those
    tools or adding native runners for managed declarations. Disables the
    child's direct moderator Process route. The host must still establish actual
    delegability, ongoing parent policy mediation and parent resource lifetime. *)
val inherit_native
  :  ?managed:Managed_tool_registry.delegation
  -> parent:t
  -> capabilities:Tool_capability.t
  -> unit
  -> (t, diagnostic list) result

(** Trusted host implementations selected only by an explicit Builtin or
    persistence-enabled agent declaration of the same name. Authored agent
    registrations must bind that exact captured declaration and its admitted
    private resources; ordinary one-off Agent declarations retain the legacy path.
    Unselected registrations are never exposed. Revision and
    result contract participate in capability identity; duplicate supplied names
    are rejected. This does not grant invocation or authoring authority. *)
type native_registration = Native_tool_pool.Registration.t =
  { implementation : Ochat_function.t
  ; implementation_revision : string
  ; result_contract : Tool_capability.result_contract
  ; authoring_metadata : Chatmd_shell_spec.Authoring_metadata.t option
    (** Trusted metadata on this implementation, retained in its capability
        identity and selection. The host still must fulfill the corresponding
        authoring policy before general model-visible exposure. *)
  ; source_requirement : Native_tool_pool.Source_requirement.t
  }

(** Bound count with a prefix traversal before lookup, polling every visited
    registration under the caller's shared startup/job checkpoint. Reject foreign
    native functions and Source without an installed-artifact preparer. Ordinary
    and Managed bindings need no source selection. Caller must already verify
    the selected binding/request pins. Exceptions and cancellation keep identity. *)
val prepare_agent_source
  :  t
  -> binding:Tool_capability.binding
  -> owner:Agent_protocol.Job.launch_owner
  -> request:Background_request.t
  -> max_registrations:int
  -> checkpoint:(unit -> unit)
  -> (Background_request.Agent_artifact.t option, Agent_protocol.Error.t) Result.t

(** Prepare a definition for an extensibility-aware host. Validates the captured
    declarations, constructs native resources through the same shell authorization
    and resource bindings as [create], then compiles all versioned scripts in an
    Eio domain against those exact staged native capabilities. Only successful
    preparation permits native shell-hook initialization and snapshot restore.
    Versioned script initializers and handlers remain unevaluated; the session
    owner installs their execution services later. Failed script preparation
    preserves source diagnostics and leaves staged shell execution revoked. Cross-kind collisions include dynamically expanded native
    tool names. Inherited/generated and authoring declarations still require their
    respective host services and are rejected. Resources remain owned by [sw];
    the caller must release that scope after failed preparation or runtime teardown.
    [native_service_revision] has the same resource-identity meaning as in [create].
    [delegated_moderator] defaults to false; true compiles moderators and their
    managed handlers against the tool-mediated delegated contract. The host still
    must enforce inherited execution authority when constructing the live runtime.
    [moderator_surface] defaults to the headless legacy surface; UI-capable hosts
    must supply their actual UI surface. [compilation_limits] and [checkpoint]
    govern one preparation/preflight deadline and distinct text/graph input
    allowances across native, managed and legacy roles; activation/execution
    retains its separate control. [startup] borrows the existing host preparation
    context with the same original limits, as in [create], without resetting its
    deadline or input accounting. Original callback failures propagate.
    Legacy declarations compile in the same preflight and their selected artifact
    is retained on [native].

    When [programs] is supplied, native hooks and versioned scripts compile from
    that exact restored custody. Selected matcher entrypoints retain their own
    contracts, and all native hook project requirements are admitted against the
    complete staged pool before any hook initializer. Missing graphs reject
    without falling back to script bodies or current files.
    This internal preparation entrypoint does not enable public feature flags. *)
val prepare_extensions
  :  ?native_service_revision:string
  -> ?startup:Startup_compilation.t
  -> ?agent_source_runner:Tool.Agent_source_runner.t
  -> ?moderator_surface:Chatml.Chatml_builtin_surface.surface
  -> ?compilation_limits:Chatml_compilation.limits
  -> ?checkpoint:(unit -> unit)
  -> ?native_registrations:native_registration list
  -> ?delegated_moderator:bool
  -> ?programs:Chatml_prompt_programs.Restored.t
  -> sw:Eio.Switch.t
  -> ctx:Eio_unix.Stdenv.base Ctx.t
  -> host:Shell_runtime.Host.t
  -> platform:Chatmd_shell_spec.Shell_spec.platform
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> approval_store:Shell_access.Approval.store
  -> ?extension_snapshots:Session.Shell_state.Extension_snapshot.t list
  -> ?persist_extension_snapshots:
       (Session.Shell_state.Extension_snapshot.t list -> (unit, string) result)
  -> run_agent:
       (?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
        -> ?session_id:string
        -> ?observer:Agent_response_loop.observer
        -> source:string
        -> ctx:Eio_unix.Stdenv.base Ctx.t
        -> string
        -> Prompt.Chat_markdown.content_item list
        -> string)
  -> unit
  -> (extension_resources, diagnostic list) result
