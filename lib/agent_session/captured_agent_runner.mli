open Core

(** Explicit retained-code child runner, confined to its creating Eio domain.
    This does not select the default for newly authored file-backed children.
    No original source grant or initialized child state is retained. *)
type t

(** [policy_revision] is the host's versioned identity for the supplied callback
    implementation, including shell authorizer/approval behavior and host changes.
    Actual script/content numeric limits are incorporated automatically. A digest grants no permissions. The
    factory's implementation/mode tag and each defining artifact's complete
    manifest digest participate separately in the returned port identity.

    The supplied shell policy callbacks and response-directory capability are
    borrowed through each bound runtime's lifetime; these are execution policy
    and output coordinates, not original source acquisition authority. *)
val create
  :  script_preparation:Runtime_script_preparation.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> policy_revision:Chatml_source.Digest.t
  -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
  -> approval_provider:Shell_runtime.Approval_broker.provider
  -> response_dir:Eio.Fs.dir_ty Eio.Path.t
  -> t

(** Host implementation tag plus actual script/content numeric policy. *)
val policy_identity : t -> Chatml_source.Digest.t

(** Bind exact owned parser/program/content custody to one runtime switch.
    Reject legacy or inconsistent owned revisions. Performs bounded metadata
    checks; no child compilation, initializer or original source acquisition.
    Binding is lazy: recursive source references do not initialize/bind children.

    Every invocation starts its own startup owner BEFORE bounded exact descriptor
    lookup, then prepares the captured child's authentic Headless role using the
    same startup and original compiler limits. The child prompt gets a new runner;
    caller-authored invocation content keeps this original caller's separate port.
    Content descriptors use the defining revision's exact document projection.
    No Startup is retained in a port or initialized state retained across calls.

    Runtime_activity owns cancellation and joined cleanup for both caller and
    runtime lifetimes. Closing [sw] revokes new calls and cancels/joins active ones.
    Child runtime ownership ends when its invocation returns. Concurrent calls
    share immutable custody only. Eio cancellation polls cover metadata lookup,
    capture/restoration and preflight; activation has its own execution controls.

    Missing/nonlocal/foreign references reject without Fetch, legacy content cache
    or parent program fallback. Expected preparation errors use the existing
    lower runner failure form; unexpected exceptions and cancellation preserve
    identity/backtrace. Lower runtime checks govern supported script contracts;
    this factory does not imply that Driver implements managed versioned dispatch.
    Do not transfer this factory or its callbacks across Eio domains. *)
val bind
  :  t
  -> revision:Prompt_revision.t
  -> sw:Eio.Switch.t
  -> (Chat_response.Tool.Agent_source_runner.t, Agent_protocol.Error.t) Result.t

module Source : sig
  (** Acquisition policy, distinct from runtime restoration/activation policy.
      The callback uses the SAME Startup and actual Scope with current history.
      policy_revision versions preprocessing/preparation callbacks and host
      implementation. Actual parser/compiler/work limits are incorporated
      automatically; the digest grants no authority and binds
      installed-factory native implementation identity separately from captured. *)
  type t

  val create
    :  policy_revision:Chatml_source.Digest.t
    -> compilation:Chatml_compilation.limits
    -> parser_limits:Prompt.Chat_markdown.Limits.t
    -> max_work_steps:int
    -> preprocess:(string -> string)
    -> prepare_programs:
         (Chat_response.Startup_compilation.t
          -> invocation:Runtime_script_preparation.Invocation.t
          -> Prompt_source_owner.Scope.t
          -> Prompt_source_owner.Capture.t
          -> ( Chatml_prompt_programs.t
               , Prompt_revision_builder.Diagnostic.t list )
               Result.t)
    -> t Or_error.t

  (** Host implementation tag plus actual admitted numeric acquisition policy. *)
  val policy_identity : t -> Chatml_source.Digest.t
end

(** Installed headless runtime binding. Exact saved current-services artifact wins
    before File/Captured selection. New File uses the actual original source lease;
    new Captured uses retained code. Both install/verify before returning a queued
    reference, then execute under authentic Headless policy. The runtime switch
    joins preparation/execution before releasing Store/lease. No native resource
    or source capability is serialized. Child ports belong to the selected child;
    invocation content retains its caller's port. The explicit prune embedding
    must serialize selection/install/admission; Daemon prunes only pre-recovery.
    Persistent precreated specialists require their separate Authored/code-view
    owner and cannot use this ordinary headless implementation. *)
val bind_installed
  :  t
  -> source:Source.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> source_lease:Prompt_source_lease.t
  -> env:Eio_unix.Stdenv.base
  -> revision:Prompt_revision.t
  -> sw:Eio.Switch.t
  -> (Chat_response.Tool.Agent_source_runner.t, Agent_protocol.Error.t) Result.t

module Source_binding : sig
  (** Immutable original-source template; borrows Store and source lease, without
      opening or claiming either. Bound ports belong to their actual resource
      switch. Join them before releasing the borrowed host owners. *)
  type t

  val create
    :  source:Source.t
    -> runtime_policy_revision:Chatml_source.Digest.t
    -> content_limits:Chatmd_prompt_content.Limits.t option
    -> artifact_store:Agent_store.Prompt_artifact_store.t
    -> source_lease:Prompt_source_lease.t
    -> t

  (** Bind the exact restored revision before native registration, using its
      constructor's actual shell/content policies and response directory. No
      source read or initializer. Missing/closed source loans still permit saved
      or Captured selections; a new File selection rejects without fallback. *)
  val bind
    :  t
    -> script_preparation:Runtime_script_preparation.t
    -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
    -> approval_provider:Shell_runtime.Approval_broker.provider
    -> response_dir:Eio.Fs.dir_ty Eio.Path.t
    -> env:Eio_unix.Stdenv.base
    -> revision:Prompt_revision.t
    -> sw:Eio.Switch.t
    -> (Chat_response.Tool.Agent_source_runner.t, Agent_protocol.Error.t) Result.t
end
