open! Core

(** Existing chat-completion executable composition. Validation precedes host
    capture/storage admission. Definition and exported transcript are separate. *)
module Options : sig
  type t

  val create
    :  definition_file:string
    -> output_file:string
    -> storage:Agent_server.Local_storage.t
    -> session_id:Agent_protocol.Id.Session.t option
    -> run_id:Agent_protocol.Id.Run.t option
    -> rebuild:bool
    -> import_transcript:string option
    -> submission:string option
    -> mode:Agent_protocol.Run.Mode.t
    -> (t, Agent_protocol.Error.t) Result.t
end

(** One actual stopped authored create/seed or anchored retained selection.
    New code is captured through existing Embedded/Catalog with Headless; retained
    Spec/role/target always win. No bootstrap attachment. The selected versioned
    moderator admits Authored_start on Managed_completion_cli's sole subscribed
    handle in the explicit mode; consumed startup is a typed failure, never a
    history/state guess followed by fallback. An explicit retained run uses its
    authorized original Admission receipt and must match the requested mode.
    Explicit nonempty -message on a retained versioned moderator uses genuine
    User_submission. Ordinary/legacy explicit -message rejects before effects;
    their initial input comes from the authored/imported transcript.
    Ordinary/legacy foreground supports Single_turn only; Workflow rejects
    before run admission/provider work. Independently admitted session creation
    can initialize its hooks before this classification. No synthetic message or
    coercion.

    Caller supplies explicit existing project-source grants (omission is local-only),
    actual host provider composition and genuine inert
    transcript-conversion context. Import reuses Frontend_transcript exactly once
    and submits the original stopped seed through existing receipt ownership.
    Unknown seed outcome reports the original key; never retries with another.
    Execution failure/cancellation reads each newly retained original command
    receipt once before releasing the host, reports original method/key/session/
    storage and a genuine Accepted_run ID when available, and preserves the
    primary failure. This best-effort whole report has a one-second clock bound
    inside protected cleanup, including receipt reads and diagnostic output.
    Storage coordinates are quoted. No params/attachment credentials are printed; no automatic
    admission or re-keyed retry follows.

    Final output uses the complete authorized canonical export and shared bounded
    ChatMD renderer, then Eio stdout or atomic file installation. No preview text
    enters canonical output and no definition/import path is overwritten. Like
    old Driver's default executable path, file/ChatMD output is finalized after
    actual completion; this does not promise provider-delta streaming. Preserve
    canonical output before reporting actual Failed/Cancelled/Limited/Interrupted
    terminal as Error, so the executable never exits successfully for those cases.
    Actual selected session ID and original admitted run ID, when present, are
    reported on stderr beside bounded live draft/tool observations. Stdout
    contains only the complete canonical transcript.
    Complete render output is bounded to 64 MiB and 256 MiB render-work units with
    the existing default document limits, matching native TUI export policy.
    The actual owning Switch closes reader/handle before connection/host/provider.
    Original callbacks, cancellation and backtraces remain original. *)
val run
  :  Options.t
  -> env:Eio_unix.Stdenv.base
  -> daemon_options:Agent_server.Daemon.options
  -> project_sources:Agent_server.Project_source_configuration.t option
  -> transcript_context:Eio_unix.Stdenv.base Chat_response.Ctx.t
  -> (Managed_completion_cli.Completed.t, Agent_protocol.Error.t) Result.t

(** Actual bin/main dispatch. Adds explicit mode, retained session/run, transcript
    import and local store selectors to existing prompt/output/transport flags.
    Missing definition is rejected before host admission; an output file is never
    implicitly reinterpreted as executable source or resumed history. *)
val command : Command.t
