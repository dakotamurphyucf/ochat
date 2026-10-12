open Core

(** Selected specialist code over the original host-owned private resources.
    This owner sits below Runtime_builder. It opens no resource, source loan,
    connection or sink, and never registers a replacement native capability. *)
module Original : sig
  (** The genuine Factory-owned persistent request decoder and original private
      authority. This is called by the exact retained registration before queue
      staging, when there is no native invocation or Delegation key yet. Resume
      and replay use their recorded artifact; new capture installs and verifies
      the selected code before returning. Request JSON grants no source authority. *)
  type prepare_persistent_source =
    binding:Chat_response.Tool_capability.binding
    -> defining_revision:Prompt_revision.t
    -> reference:Prompt.Chat_markdown.Agent_reference.t
    -> owner:Agent_protocol.Job.launch_owner
    -> request:Chat_response.Background_request.t
    -> checkpoint:(unit -> unit)
    -> ( Chat_response.Background_request.Agent_artifact.t option
         , Agent_protocol.Error.t )
         Result.t

  (** A borrowed view of the already admitted specialist. The owning Factory
      supplies these exact values together and keeps their resource switch alive.
      The helper validates source/artifact/parser, managed/capability and typed
      declaration coherence. The native record itself has no revision identity;
      its association with these original resources remains the Factory's trusted
      coassembly, rather than a claim inferred from names or filesystem paths.
      No new lifecycle or permission owner is created by this view. *)
  type t =
    { source : Authored_agent_source.t
    ; revision : Prompt_revision.t
    ; native : Chat_response.Agent_runtime.t
    ; definition : Chat_response.Extension_compiler.definition option
    ; managed : Chat_response.Managed_tool_registry.t option
    ; agent_source : Captured_agent_runner.Source_binding.t option
    ; prepare_persistent_source : prepare_persistent_source option
    }
end

type t

(** Validate the selected installed artifact/tree, genuine delegated script role,
    unchanged native declarations and managed interfaces/dependency selections.
    Compile the selected moderator/managed handlers against the SAME original
    capability bindings. Complete all compiler and tool preflight before invoking
    any fresh shell-hook initializer.

    Shell code requires current security verification and a NEW authorization of
    its complete prepared execution manifest. Registry.rebind_code retains the
    original native resource footprint, worker/service occurrences, raw audit sink
    and actual approval/caller checks. Only newly authorized specialist code is
    replaced; the original specialist and its code remain immutable.

    Validation, compilation, tool admission, security verification and grant
    acquisition share the caller's original startup deadline. Activation follows
    that complete preflight outside the compiler timer, under the existing hook
    execution limits/cancellation scope. The complete helper also shares one
    finite custody-work allowance, including activation's owner construction.
    Neither startup nor checkpoint escapes. The switch owns fresh hook instances. The
    caller joins all fresh execution before releasing the original resource loan.
    Expected preparation failures return diagnostics; original callback exceptions
    and cancellation retain their identity/backtrace. No source read, artifact
    install, Delegation reservation or capability minting occurs here. *)
val prepare
  :  Original.t
  -> selected:Prompt_revision.t
  -> script_preparation:Runtime_script_preparation.t
  -> startup:Chat_response.Startup_compilation.t
  -> env:Eio_unix.Stdenv.base
  -> sw:Eio.Switch.t
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> extension_snapshots:Session.Shell_state.Extension_snapshot.t list
  -> persist_extension_snapshots:
       (Session.Shell_state.Extension_snapshot.t list -> (unit, string) Result.t)
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) Result.t

val revision : t -> Prompt_revision.t
val native : t -> Chat_response.Agent_runtime.t
val definition : t -> Chat_response.Extension_compiler.definition option
val managed : t -> Chat_response.Managed_tool_registry.t option
val shell_code_view : t -> Shell_runtime.Registry.Code_view.t option

(** The exact original native implementations remain unchanged. Their updated
    registration-owned preparers use this selected revision through the genuine
    borrowed Source_binding and Factory callback. Missing required source routing
    rejects; it never becomes Ordinary or reopens an artifact path as authority.
    The runner supplies selected content conversion, while the immutable view
    redirects the retained native closures at their authenticated dispatch point.
    The host enters shell_code_view at execution and installs this source view in
    that same current native service; neither getter creates an invocation. *)
val agent_source_code_view : t -> Agent_source_code_view.t

val agent_source_runner : t -> Chat_response.Tool.Agent_source_runner.t option

(** Exact original receiver and selected revision admission. This checks physical
    original runtime identity and exact complete selected artifact identity; a
    same-name/native descriptor or another admitted source is insufficient. *)
val check
  :  t
  -> original:Original.t
  -> selected:Prompt_revision.t
  -> (unit, Agent_protocol.Error.t) Result.t
