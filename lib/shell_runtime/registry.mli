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

(** [instantiate_prepared prepared] verifies [grant] and publishes runtimes
    only after the prepared extension set is valid. *)
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

(** [script t name] returns the script and executable identities captured
    before tool publication. *)
val script : t -> string -> script option

val runtimes : t -> Runtime.t list
val tools : t -> Chatmd_shell_spec.Shell_tool_spec.t list
