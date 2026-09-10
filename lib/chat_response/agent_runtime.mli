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
  }

type shell_inspection =
  { manifest : Chatmd_shell_spec.Manifest.t
  ; live_runtimes : Chatmd_shell_spec.Shell_spec.t list
  ; administrative_policy : Shell_runtime.Admin_policy.t
  ; security_status : Shell_runtime.Manifest_security.status
  }

(** [inspect_shell ~env ~platform ~prompt_elements] evaluates requested shell
    declarations against host administrative and trust policy without
    authorizing a manifest or instantiating any executable runtime. *)
val inspect_shell
  :  env:Eio_unix.Stdenv.base
  -> platform:Chatmd_shell_spec.Shell_spec.platform
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> (shell_inspection, diagnostic list) result

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
    ~session_id ~resource_runner ~prompt_elements] creates the explicit host
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
  -> resource_runner:string option
  -> prompt_elements:Prompt.Chat_markdown.top_level_elements list
  -> (Shell_runtime.Host.t, diagnostic list) result

(** [create ~sw ~ctx ~host ~platform ~prompt_elements ~manifest_authorizer
    ~approval_provider ~run_agent] compiles and authorizes every shell
    declaration before exposing any declared tool. Legacy command tools are
    desugared into manifest-bound fixed shell tools.

    Documents without shell or legacy command declarations do not invoke the
    manifest authorizer and return no shell registry. All resources remain
    owned by [sw]. *)
val create
  :  sw:Eio.Switch.t
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
    constructs no resources and exposes no unselected tools. Managed bindings
    reject until an owner-aware inherited dispatcher is supplied. Disables the
    child's direct moderator Process route. The host must still establish actual
    delegability, ongoing parent policy mediation and parent resource lifetime. *)
val inherit_native
  :  parent:t
  -> capabilities:Tool_capability.t
  -> (t, diagnostic list) result

(** Trusted host implementations selected only by an explicit Builtin declaration
    of the same name. Unselected registrations are never exposed. Revision and
    result contract participate in capability identity; duplicate supplied names
    are rejected. This does not grant invocation or authoring authority. *)
type native_registration =
  { implementation : Ochat_function.t
  ; implementation_revision : string
  ; result_contract : Tool_capability.result_contract
  ; authoring_metadata : Chatmd_shell_spec.Authoring_metadata.t option
    (** Trusted metadata on this implementation, retained in its capability
        identity and selection. The host still must fulfill the corresponding
        authoring policy before general model-visible exposure. *)
  }

(** Prepare a definition for an extensibility-aware host. Validates the captured
    declarations, constructs native resources through the same shell authorization
    and resource bindings as [create], then compiles all versioned scripts in an
    Eio domain against those exact live capabilities. No script initializer or
    handler runs here. Cross-kind collisions include dynamically expanded native
    tool names. Inherited/generated and authoring declarations still require their
    respective host services and are rejected. Resources remain owned by [sw];
    the caller must release that scope after failed preparation or runtime teardown.
    This internal preparation entrypoint does not enable public feature flags. *)
val prepare_extensions
  :  ?native_registrations:native_registration list
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
