open Core

type t
type child = t

val revision : t -> Prompt_revision.t
val reference : t -> Chat_response.Background_request.Agent_artifact.t

(** Canonical immutable reference of a verified installed revision. This is
    comparison data only: the caller must have authenticated the revision's
    artifact and current selected owner. It performs no source acquisition. *)
val reference_of_revision
  :  Prompt_revision.t
  -> checkpoint:(unit -> unit)
  -> (Chat_response.Background_request.Agent_artifact.t, Agent_protocol.Error.t) result

module Model_selection : sig
  (** A model member selected and replayed through the existing artifact
      publisher, with actual bounded ChatMD authored root/dependency bytes and
      charged outer selection checkpoints. Program/project sources retain their
      distinct existing bounded acquisition policy. Source_binding wraps this
      in its physical issuer before it can mint a Model target. *)
  type t

  val source : t -> child
  val input_bytes : t -> int
  val work_steps : t -> int
end

(** New File selection, under the defining runtime's original source lease.
    Same remaining cooperative work allowance covers exact reference admission,
    Scope capture, program/content preparation, final replay and installation.
    Entry retains only admitted bounded identity; original parent/reference do
    not escape. The host callback prepares only immutable custody using actual
    explicitly supplied source grants and original dependency history. It must
    use capture_child's authentic invocation role; no guessed Initial operation.
    Content uses the SAME actual Scope. Final Builder capture_entry publication
    installs and verifies before returning. No initializer/native resource,
    reservation, job stage, retry or fallback here. Original exceptions retain
    identity/backtrace after Scope joining. *)
val file
  :  parent:Prompt_revision.t
  -> reference:Prompt.Chat_markdown.Agent_reference.t
  -> source_lease:Prompt_source_lease.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> compilation:Chatml_compilation.limits
  -> tool_contract:Chatml_compilation.Tool_contract.t
  -> parser_limits:Prompt.Chat_markdown.Limits.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> max_work_steps:int
  -> preprocess:(string -> string)
  -> prepare_programs:
       (Chat_response.Startup_compilation.t
        -> tool_contract:Chatml_compilation.Tool_contract.t
        -> Prompt_source_owner.Scope.t
        -> Prompt_source_owner.Capture.t
        -> (Chatml_prompt_programs.t, Prompt_revision_builder.Diagnostic.t list) Result.t)
  -> env:Eio_unix.Stdenv.base
  -> created_at:Agent_protocol.Timestamp.t
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) Result.t

(** New Captured selection: authentic retained child role under runtime semantic
    policy, then install or reuse its complete verified immutable artifact.
    Repeated selection of the same captured source returns the original installed
    publication and replays that tree; a conflicting or damaged installation
    rejects. No original
    lease, current file read, Fetch or source grant is used. The selected child
    has its own materialized tree/replayed Parsed; original parent untouched.
    Actual complete role selection and restoration precede installation. *)
val captured
  :  parent:Prompt_revision.t
  -> reference:Prompt.Chat_markdown.Agent_reference.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> policy:Runtime_script_preparation.t
  -> tool_contract:Chatml_compilation.Tool_contract.t
  -> invocation:Runtime_script_preparation.Invocation.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) Result.t

(** New File Model selection through a checked original-root member entry.
    Unlike [file], this has no parser Agent_reference. The upper selected
    Content_source supplies the exact parent/member and rechecks the active
    child/source before and after this yielding preparation. The same bounded
    Scope capture, program/content preparation and verified installation are
    reused; no separate source walker or retry after a denied read. The
    Builder Capture reports actual authored root/dependency bytes; the outer
    bounded checkpoint counts selection, custody and publication work once.
    Existing program/project source and compiler limits remain independent. *)
val file_selected_member
  :  parent:Prompt_revision.t
  -> entry:Prompt_source_entry.t
  -> source_lease:Prompt_source_lease.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> compilation:Chatml_compilation.limits
  -> tool_contract:Chatml_compilation.Tool_contract.t
  -> parser_limits:Prompt.Chat_markdown.Limits.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> max_work_steps:int
  -> max_input_bytes:int
  -> preprocess:(string -> string)
  -> prepare_programs:
       (Chat_response.Startup_compilation.t
        -> tool_contract:Chatml_compilation.Tool_contract.t
        -> Prompt_source_owner.Scope.t
        -> Prompt_source_owner.Capture.t
        -> (Chatml_prompt_programs.t, Prompt_revision_builder.Diagnostic.t list) result)
  -> env:Eio_unix.Stdenv.base
  -> created_at:Agent_protocol.Timestamp.t
  -> checkpoint:(unit -> unit)
  -> on_acquire:(unit -> unit)
  -> (Model_selection.t, Agent_protocol.Error.t) result

(** Immutable captured Model selection from the exact selected parent and
    member. No original-source lease or ambient Fetch. Missing custody rejects
    instead of changing to File after capture, and repeated selection preserves
    the verified retained publication identity. The returned byte count comes
    from the retained authored inventory; the outer charged checkpoint counts
    replay/publication work once. *)
val captured_selected_member
  :  parent:Prompt_revision.t
  -> member:Source.Portable_path.t
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> policy:Runtime_script_preparation.t
  -> tool_contract:Chatml_compilation.Tool_contract.t
  -> invocation:Runtime_script_preparation.Invocation.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> max_input_bytes:int
  -> max_work_steps:int
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> on_acquire:(unit -> unit)
  -> (Model_selection.t, Agent_protocol.Error.t) result

(** Recorded reference: strict complete artifact load, exact digest/member,
    parser/program/content replay under explicit policy; no source selection.
    Caller has FIRST authenticated the reference against actual saved binding,
    root invocation and current native services. The reference grants nothing.
    This path is mandatory on queued retry/recovery before any File branch. *)
val restore
  :  artifact_store:Agent_store.Prompt_artifact_store.t
  -> definition:Prompt_definition.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> Chat_response.Background_request.Agent_artifact.t
  -> checkpoint:(unit -> unit)
  -> (t, Agent_protocol.Error.t) Result.t
