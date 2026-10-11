open Core

(** Explicit retained-code child runner, confined to its creating Eio domain.
    This does not select the default for newly authored file-backed children.
    No original source grant or initialized child state is retained. *)
type t

(** [policy_revision] is the host's versioned identity for the complete supplied
    execution policy, including shell authorizer/approval behavior, script/content
    limits and host implementation changes. A digest grants no permissions. The
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
