open Core

(** Host policy for restoring authored scripts before native/runtime activation.
    Contains no source authority, compiler state or live runtime resources. *)
type t

module Invocation : sig
  (** Actual runtime role. Headless uses the ordinary headless moderator surface;
      delegated UI uses its restricted v1 contract. No impossible combination of
      headless surface and delegated UI is exposed. This label grants no tools. *)
  type t =
    | Ui
    | Headless
    | Delegated_ui
  [@@deriving equal, sexp_of]
end

(** All component policies are already validated. Compilation limits and the
    positive custody work ceiling are validated here. [max_custody_work_steps]
    bounds each complete retained-custody selection or restoration operation;
    planning and selected-project receipt reconstruction retain their own limits.
    Restoration uses the host's semantic project allowance, never artifact-only
    admission limits. The startup deadline spans all these preparation phases. *)
val create
  :  compilation:Chatml_compilation.limits
  -> planning:Chat_response.Script_contract_plan.Limits.t
  -> restoration:Chatml_prompt_programs.Limits.t
  -> max_custody_work_steps:int
  -> t Or_error.t

val compilation : t -> Chatml_compilation.limits
val restoration : t -> Chatml_prompt_programs.Limits.t
val max_custody_work_steps : t -> int

(** Plan the actual parser's scripts before authoring helpers are added. Uses the
    existing pure shell manifest compiler and the explicit invocation contract.
    No filesystem access, native setup or initializer. The borrowed startup owner
    covers the complete phase; original callback failures propagate unchanged. *)
val plan
  :  t
  -> startup:Chat_response.Startup_compilation.t
  -> parsed:Prompt.Chat_markdown.Parsed.t
  -> invocation:Invocation.t
  -> (Chat_response.Script_contract_plan.t, Agent_protocol.Error.t) Result.t

(** Start or borrow the host startup owner before planning/restoration, then lend
    restored scripts and the same original compilation limits to [f]. [f] must
    forward [startup] into Agent_runtime; it may activate scripts after its own
    complete preflight. Activation is not wrapped in the compilation timer.

    [None] policy supports only legacy revisions without parser/program custody
    or stored program/content bundles, using existing default compilation limits.
    Owned revisions require explicit policy even when they contain no projects.
    A supplied policy also governs legacy compilation when custody is absent.
    No missing graph/selection falls back to compiling only the declaration body.
    Source files are never reopened; the exact revision parser and stored custody
    are used. [f] must not retain the borrowed context in resources or artifacts;
    its original owner enforces lifetime. A newly created context rejects use after
    this call returns. Callback failures and cancellation retain their original
    backtraces. A borrowed context must have the same original compilation limits. *)
val with_revision
  :  t option
  -> ?startup:Chat_response.Startup_compilation.t
  -> Prompt_revision.t
  -> env:Eio_unix.Stdenv.base
  -> invocation:Invocation.t
  -> checkpoint:(unit -> unit)
  -> f:
       (Chat_response.Startup_compilation.t
        -> compilation_limits:Chatml_compilation.limits
        -> programs:Chatml_prompt_programs.Restored.t option
        -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Prepare a retained authored child and select its exact delegated role before
    its source fingerprint can enter resource admission or a durable reservation.
    The captured inventory is preserved; ambiguous, partial or missing recorded
    roles reject without compiling a replacement entry body. Genuine legacy
    sources retain their existing path. Content replay uses [content_limits].

    The callback borrows the same startup context that bounded child capture,
    role planning, selection and adoption. It must forward that context into
    resource construction and must not retain it. The callback may activate after
    complete preflight; activation is outside the preparation timer. No native
    source acquisition, tool initialization or artifact installation occurs before
    the callback. Original callback exceptions and cancellation propagate. *)
val with_authored_child
  :  t option
  -> parent:Prompt_revision.t
  -> tool_name:string
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> f:
       (Chat_response.Startup_compilation.t
        -> source:Authored_agent_source.t
        -> revision:Prompt_revision.t
        -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t

(** Prepare an ordinary child from one genuine reference in the defining parent's
    owned Parsed. Capture, select/adopt its explicit invocation role, and restore
    CHILD programs under the same borrowed startup owner. An explicit validated
    policy is mandatory; no legacy absence, missing-role, or parent-inventory
    fallback is possible. Captured source policy is fixed: no original read,
    source-mode default, tool grant, initializer or installation here.

    [f] receives the child's own revision/Parsed/projection and its authentic
    restored programs. Bind child dispatch to that revision, while caller-owned
    invocation content retains the caller's separate dispatcher. Forward startup
    and original limits through lower preflight; activation remains outside the
    preparation timer. Original callbacks/cancellation/backtraces propagate. *)
val with_captured_child
  :  t
  -> ?startup:Chat_response.Startup_compilation.t
  -> Prompt_revision.t
  -> reference:Prompt.Chat_markdown.Agent_reference.t
  -> invocation:Invocation.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> env:Eio_unix.Stdenv.base
  -> checkpoint:(unit -> unit)
  -> f:
       (Chat_response.Startup_compilation.t
        -> revision:Prompt_revision.t
        -> compilation_limits:Chatml_compilation.limits
        -> programs:Chatml_prompt_programs.Restored.t option
        -> ('a, Agent_protocol.Error.t) Result.t)
  -> ('a, Agent_protocol.Error.t) Result.t
