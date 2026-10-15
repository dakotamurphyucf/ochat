open! Core

(** One physical developer Model_call runner. The sole Job scheduler offers a
    Root-issued candidate; this module enters that Root's existing Worker and
    Model_inference scope. It owns no provider resolver, durable writer, result
    ledger, independent capacity pool, async Model.spawn table or polling loop. *)
type t

module Sources : sig
  type t

  (** Concrete Eio source and approval services from the trusted developer host
      assembly. The directories must belong to the authenticated captured
      workspace and outlive joined work; this grouping does not authorize
      reading a source or sending a model request. Unlike the conversation
      Runtime_builder's exec_context, it contains no service-principal Session
      or callback that can replace the physical Root identity. The runner uses
      these services with Chat_response.Fetch and Driver under its live Claim,
      rechecking that Claim and current actor/provider after yielding source or
      approval work and again at application submission. The
      authorizer and approval provider are required, never silent defaults. *)
  val create
    :  env:Eio_unix.Stdenv.base
    -> prompt_dir:Eio.Fs.dir_ty Eio.Path.t
    -> tool_dir:Eio.Fs.dir_ty Eio.Path.t
    -> response_dir:Eio.Fs.dir_ty Eio.Path.t
    -> cache:Chat_response.Cache.t
    -> manifest_authorizer:Shell_runtime.Manifest_authorizer.t
    -> approval_provider:Shell_runtime.Approval_broker.provider
    -> t
end

module Availability : sig
  type t =
    | Runnable
    | Malformed_supported
    | Unsupported_kind
  [@@deriving equal, sexp_of]
end

(** Bind to [root]'s actual selected Model provider/actor and the SAME
    Root.Job_result owner used by Tool/Script work. A missing selected provider
    or foreign result owner rejects. The runner retains the Root Store handle's
    actual immutable document limits for saved envelope admission; it cannot
    use a caller-supplied larger budget. No credential bytes are obtained at
    construction. [sources] are trusted host resources, not selection or
    execution authority. All physical checks are repeated at Worker entry and
    application-effect admission. *)
val create
  :  root:Developer_root_owner.run
  -> results:Developer_root_owner.Job_result.t
  -> sources:Sources.t
  -> (t, Agent_protocol.Error.t) result

(** Comparison only: this package was issued for exactly this live Root object.
    An equal run ID or copied candidate cannot attach it elsewhere. *)
val issued_by_root : t -> Developer_root_owner.run -> bool

(** Lend the same strict provider Context and recipe used by [run_candidate],
    now under OCH398's physically entered saved-Agent source/native proof. The
    receiver must be issued by the same live Root that validates [selected].
    This calls that Root's existing Model_inference.with_claimed_agent; it does
    not claim a Model_call job, create a provider or install an alternate
    tracking engine. Trusted [f] must use the Context only in this lexical
    callback; its type alone cannot prevent returning it as ['a]. An escaped
    Context cannot admit a new effect after the physical scope expires because
    Root/source checks and the composed provider submission guard run at
    re-entry and effect admission. Original callback exceptions and
    cancellation preserve identity/backtrace. *)
val with_saved_agent_context
  :  t
  -> selected:Agent_session.Captured_agent_runner.Claimed_developer.Selected.t
  -> f:(Eio_unix.Stdenv.base Chat_response.Ctx.t -> 'a)
  -> ('a, Agent_protocol.Error.t) result

(** Scheduling hint only. A saved Model_call envelope is decoded under the
    Root's actual whole-document limits; the later entered Claim is decoded
    again and governs execution. A malformed genuine Model_call is still
    offered to [run_candidate] so the physical Worker records Failed without a
    handler or provider effect. Other Job kinds stay unclaimed by this package.
    A temporarily unavailable/revoked selected provider is handled by the
    physical attempt, not silently left in a permanent unsupported queue. *)
val availability
  :  t
  -> Developer_root_owner.Job_candidate.t
  -> (Availability.t, Agent_protocol.Error.t) result

(** Require an authentic Root candidate, then enter exactly one
    Job_candidate.with_claimed; saturated/not-due/stale returns [Ok None]
    without a callback. Derive the saved Model_call and its full Target from
    the entered Claim, re-admit its v1 agent_prompt_v1 request, and require the
    exact Root-selected provider/actor before effects. The original
    Model_inference.with_claimed supplies the selected Context, durable
    identity/attempt/observation/completion callbacks, admitted-call proof,
    before-dispatch target check, nested child admission and composed
    application-submit guard. A strict immutable recipe invokes the one shared
    Model_executor body; nested Driver/Fork/auxiliary contexts inherit it and
    nested Model.spawn persists through this same Root rather than a local
    Model_executor table. Source/approval waits recheck the live scope, while
    HTTP/WS checks current actor/Claim/provider after auth/transport waits.

    The callback's genuine terminal result becomes an Inline Completion under
    the Worker's exact attempt. [results] performs the same frozen
    Inline/Artifact selection as Tool/Script work through the Worker's one
    post-join terminal CAS. An uncertain intent/Blob/Store write retains its
    exact selection, shared pending allowance, Claim and capacity; neither
    recipe, provider effect nor nested spawn is replayed. Unexpected exception
    or cancellation preserves its original identity/backtrace through protected
    Worker cleanup. [on_capacity_decision] is a nonyielding one-shot observer,
    never a Claim or effect grant. *)
val run_candidate
  :  t
  -> ?on_capacity_decision:(Developer_job_worker.Capacity_decision.t -> unit)
  -> Developer_root_owner.Job_candidate.t
  -> (Agent_protocol.Developer_job.t option, Agent_protocol.Error.t) result
