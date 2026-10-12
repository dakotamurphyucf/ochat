open! Core

(** Immutable live shell runtime and tool registry. *)

(** Prepared identity for one script-file tool. [path] is the argv path
    relative to the runtime working directory when possible, while
    [verification_path] identifies the same file from the host filesystem. *)
type script =
  { tool_name : string
  ; path : string
  ; verification_path : string
  ; sha256 : string
  ; size : int
  ; executable_path : string
  ; executable_sha256 : string
  ; arguments_before_model : string list
  ; max_source_bytes : int
  }
[@@deriving sexp, compare, equal]

type inspection =
  { manifest_sha256 : string
  ; runtime_ids : string list
  ; tool_names : string list
  ; scripts : script list
  }
[@@deriving sexp, compare, equal]

type t
type prepared

type error =
  { code : string
  ; message : string
  ; runtime_id : string option
  }
[@@deriving sexp, compare, equal]

(** [prepare ~manifest ~material] compiles all authorized ChatML extension
    sources without instantiating runtimes or performing host I/O. *)
val prepare
  :  manifest:Chatmd_shell_spec.Manifest.t
  -> material:Chatmd_shell_spec.Manifest_compiler.material
  -> (prepared, error list) result

(** Administrative variant of {!prepare}. *)
val prepare_with_policy
  :  admin_policy:Admin_policy.t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> material:Chatmd_shell_spec.Manifest_compiler.material
  -> (prepared, error list) result

(** [compile_extension] is invoked only for actual authorized scripts supplied
    by the manifest compiler. Its output must match the entire declaration and
    default entrypoint; mismatches reject before initialization. The default
    legacy compiler used by {!prepare_with_policy} rejects project-selected
    scripts rather than compiling their entry body alone. *)
val prepare_with_compiler
  :  compile_extension:
       (Chatmd_shell_spec.Chatmd_script_spec.t
        -> (Chatml_extension.compiled, Chatmd_shell_spec.Diagnostic.t list) Result.t)
  -> admin_policy:Admin_policy.t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> material:Chatmd_shell_spec.Manifest_compiler.material
  -> (prepared, error list) result

(** Bounded compiler variant used by the shared startup owner. Original material
    pairing and administrative admission precede compilation. Build complete
    execution identity from the actual compiled set before returning; no runtime
    or native resource is initialized. Output canonical metadata is bounded by
    [max_manifest_bytes], and every projection/encoding/hash walk uses [checkpoint].
    Existing legacy wrappers preserve empty-projection v1 canonical bytes. *)
val prepare_with_compiler_bounded
  :  Chatmd_shell_spec.Manifest_compiler.material
  -> compile_extension:
       (Chatmd_shell_spec.Chatmd_script_spec.t
        -> (Chatml_extension.compiled, Chatmd_shell_spec.Diagnostic.t list) Result.t)
  -> admin_policy:Admin_policy.t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> max_manifest_bytes:int
  -> checkpoint:(unit -> unit)
  -> (prepared, error list) Result.t

(** Complete immutable executable authorization identity. Security verification,
    grants, registry publication and audit bind this same value. *)
val prepared_manifest : prepared -> Chatmd_shell_spec.Manifest.t

module Staged : sig
  type t

  (** Genuine admitted metadata and configurations. Execution is gated until
      complete activation; retained values reject after failure or owner closure.
      This view is not a live [Registry.t]. *)
  val manifest : t -> Chatmd_shell_spec.Manifest.t

  val runtime : t -> string -> Runtime.t option
  val script : t -> string -> script option
  val tools : t -> Chatmd_shell_spec.Shell_tool_spec.t list
  val check : t -> (unit, string) Result.t
end

(** Build the real native resources and invoke [preflight] exactly once before
    any hook initializer. Success permits activation, existing snapshot restore
    and publication; failure/cancellation/exception terminally revokes escaped
    staged functions/configurations. Original exceptions/backtraces propagate.
    The caller's switch owns native handles and revokes even ready access when
    closed. No unrelated switch is closed and completed initializer external
    effects are not rolled back. Existing [instantiate_prepared] delegates with
    an empty preflight, preserving its immediate behavior. *)
val instantiate_prepared_after
  :  sw:Eio.Switch.t
  -> host:Host.t
  -> grant:Manifest_authorizer.grant
  -> approval_provider:Approval_broker.provider
  -> ?approval_store:Shell_access.Approval.store
  -> ?model_completion:Runtime.model_completion
  -> ?extension_snapshots:Session.Shell_state.Extension_snapshot.t list
  -> ?persist_extension_snapshots:
       (Session.Shell_state.Extension_snapshot.t list -> (unit, string) result)
  -> preflight:(Staged.t -> ('a, error list) Result.t)
  -> prepared
  -> (t * 'a, error list) Result.t

(** [instantiate_prepared prepared] verifies [grant] and publishes runtimes
    only after the prepared extension set is valid. Saved extension state matches
    the exact manifest and executable source revision: prepared hook dependencies
    participate, while legacy hooks retain their body digest. Nonmatching saved
    state is ignored and the hook starts with its freshly initialized state.
    Multiple matching snapshots for one persistent binding reject restoration. *)
val instantiate_prepared
  :  sw:Eio.Switch.t
  -> host:Host.t
  -> grant:Manifest_authorizer.grant
  -> approval_provider:Approval_broker.provider
  -> ?approval_store:Shell_access.Approval.store
  -> ?model_completion:Runtime.model_completion
  -> ?extension_snapshots:Session.Shell_state.Extension_snapshot.t list
  -> ?persist_extension_snapshots:
       (Session.Shell_state.Extension_snapshot.t list -> (unit, string) result)
  -> prepared
  -> (t, error list) result

(** [instantiate] verifies the grant and compiler-owned live material, then
    instantiates every runtime. Failure leaves no partial registry. *)
val instantiate
  :  sw:Eio.Switch.t
  -> host:Host.t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> grant:Manifest_authorizer.grant
  -> material:Chatmd_shell_spec.Manifest_compiler.material
  -> approval_provider:Approval_broker.provider
  -> (t, error list) result

val instantiate_with_model_completion
  :  sw:Eio.Switch.t
  -> host:Host.t
  -> manifest:Chatmd_shell_spec.Manifest.t
  -> grant:Manifest_authorizer.grant
  -> material:Chatmd_shell_spec.Manifest_compiler.material
  -> approval_provider:Approval_broker.provider
  -> model_completion:Runtime.model_completion
  -> (t, error list) result

val manifest : t -> Chatmd_shell_spec.Manifest.t
val inspection : t -> inspection
val runtime : t -> string -> Runtime.t option
val tool : t -> string -> Chatmd_shell_spec.Shell_tool_spec.t option

(** [redact_tool_input t ~tool_name value] redacts a shell tool payload with
    the exact runtime bound to that tool. Unknown tools remain unchanged. *)
val redact_tool_input : t -> tool_name:string -> string -> string

(** [script t name] returns the script and executable identities captured
    before tool publication. *)
val script : t -> string -> script option

val runtimes : t -> Runtime.t list
val tools : t -> Chatmd_shell_spec.Shell_tool_spec.t list

module Code_view : sig
  type registry = t

  (** Fresh authorized specialist code over the receiver's retained native
      resources. The switch owns fresh hook instances; the original registry,
      hook state and native registrations remain unchanged. No source loader,
      grant callback or new native resource is retained. *)
  type t

  (** Logical comparison/construction work observed by [rebind_code]. *)
  val work_steps : t -> int

  module Execution : sig
    (** Borrowed admission for one actual execution. It expires when the owning
        [with_execution] callback returns, including on exceptions. *)
    type t

    (** Selected registry for direct Process handlers. Returned runtime configs
        retain this admission's check as well as both resource-owner checks;
        retaining the registry cannot extend the admission. *)
    val registry : t -> (registry, string) Result.t

    (** Trusted native-dispatch adapter composed through [Call_context]. Exact
        opaque resource ownership selects fresh code; unrelated configs retain
        their original code. Functional caller overlays preserve that ownership.
        The supplied config's actual caller/approval/channel/native fields are
        preserved, and fresh owner/admission checks are composed into it. *)
    val prepare_executor
      :  t
      -> Shell_access.Executor.config
      -> (Shell_access.Executor.config, string) Result.t
  end

  (** Enter at execution time under the genuine host-owned native resource loan.
      Deferred jobs/continuations enter a new admission under their recorded
      revision's owner and reacquire actual native invocation services; they do
      not reuse the creating fiber's bindings or an expired borrowed invocation.
      No ambient current-view is installed. The host owns worker cancellation and
      joins all admitted work before releasing original resources.

      [checkpoint] is borrowed for this invocation and polls the bounded runtime
      projection before retention. It must debit the host's existing execution
      allowance where that host accounts projection work; no fresh allowance is
      created here. Original exceptions/cancellation/backtraces propagate. *)
  val with_execution
    :  t
    -> checkpoint:(unit -> unit)
    -> (Execution.t -> 'a)
    -> ('a, string) Result.t
end

(** Rebuild only specialist code over this registry's retained native material.
    [grant] must authorize the actual complete prepared execution manifest.
    Validate effective typed native declarations and retained material before
    initializing hooks; raw manifest/body SHA equality is not this comparison.
    Code/graph changes and diagnostic rebasing are allowed. Native paths, resource
    declarations, tools/script-file bindings, non-ChatML hook bindings, wiring,
    lifecycle and controls remain fixed. No path lookup, environment reload,
    sink reopening, connection acquisition or native-function reminting occurs.

    Original administrative/live resource checks and root-authorized approval
    services remain. Newly authorized specialist callbacks replace the old
    specialist callbacks. Raw audit sink/sequence/failure/content/secret controls
    are retained. Fresh snapshots use the selected complete manifest and actual
    graph revisions; parent state is untouched.

    [sw] owns fresh instances. The caller completes role/capability/compiler
    preflight before calling, keeps the original native loan alive, and joins
    admitted work before release. Failure revokes fresh escaped access; external
    initializer effects are not rolled back. [max_work_steps] is the remaining
    logical comparison/construction allowance, checked before work/retention.
    Errors abort the whole attempt, with no implicit retry or allowance reset.
    Borrowed callback/cancellation exceptions preserve identity and raw trace. *)
val rebind_code
  :  t
  -> prepared:prepared
  -> grant:Manifest_authorizer.grant
  -> sw:Eio.Switch.t
  -> ?extension_snapshots:Session.Shell_state.Extension_snapshot.t list
  -> ?persist_extension_snapshots:
       (Session.Shell_state.Extension_snapshot.t list -> (unit, string) Result.t)
  -> max_work_steps:int
  -> checkpoint:(unit -> unit)
  -> unit
  -> (Code_view.t, error list) Result.t
