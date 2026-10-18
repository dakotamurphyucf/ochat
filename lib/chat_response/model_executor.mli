open! Core
module CM = Prompt.Chat_markdown
module Lang = Chatml.Chatml_lang

type exec_context =
  { ctx : Eio_unix.Stdenv.base Ctx.t
  ; run_agent :
      ?history_compaction:bool
      -> ?prompt_dir:Eio.Fs.dir_ty Eio.Path.t
      -> ?session_id:string
      -> ctx:Eio_unix.Stdenv.base Ctx.t
      -> string
      -> CM.content_item list
      -> string
  ; fetch_prompt :
      ctx:Eio_unix.Stdenv.base Ctx.t
      -> prompt:string
      -> is_local:bool
      -> (string * Eio.Fs.dir_ty Eio.Path.t option, string) result
  }

type job_state =
  [ `Pending
  | `Succeeded of Jsonaf.t
  | `Failed of string
  ]

type t

module Session_settlement : sig
  type t =
    { spawned_jobs : int
    ; delivered_events : int
    }
  [@@deriving sexp_of]
end

module Selected_source : sig
  type t

  (** Trusted host composition port, not a type-level provenance proof.
      [with_selected] enters the authenticated member for this exact decoded
      request and lends its retained Config and a consumed-once [run] closure.
      The closure receives the Context after the shared override/target/derive
      path, plus that path's exact synthesized input, history-compaction and
      nested session values. It cannot fetch or reparse an ambient path. The
      host must repeat physical Root/source/provider checks after yields;
      cancellation and unexpected exceptions retain their identity. Expected
      typed source denial is mapped to this existing recipe's string error
      domain only at the adapter boundary. *)
  val create
    :  with_selected:
         (Model_spawn_request.t
          -> f:
               (config:CM.config
                -> run:
                     (ctx:Eio_unix.Stdenv.base Ctx.t
                      -> input:CM.content_item list
                      -> history_compaction:bool
                      -> session_id:string
                      -> (string, string) result)
                -> (Moderation.Capabilities.model_call_result, string) result)
          -> (Moderation.Capabilities.model_call_result, string) result)
    -> t
end

(** [create ~sw ~exec_context ?max_spawned_jobs ()] creates the model-executor
    capability used by moderator-driven async model work.

    The spawned-job budget intentionally remains owned by this constructor
    rather than by {!Chat_response.Runtime_semantics.policy}; see the Phase 2
    contract in [docs-src/chatml-budget-policy.md].

    For the end-to-end async completion, reinjection, and wakeup lifecycle,
    see [docs-src/chatml-async-completion-lifecycle.md]. *)
val create
  :  sw:Eio.Switch.t
  -> exec_context:exec_context
  -> ?max_spawned_jobs:int
  -> unit
  -> t

(** Opt-in selected child recipe owner. The trusted Child_prepared assembly
    supplies a joined local-job switch nested inside its active callback;
    selected calls still recheck the
    authenticated source and physical Root on every attempt. The same strict
    body performs target overrides, capture, Context.derive, input/default
    synthesis and result encoding. This mode has no Fetch or ordinary
    run_agent fallback. Ordinary [create] rejects explicit Captured source mode
    before any Fetch because it has no selected captured-source custody. *)
val create_selected
  :  sw:Eio.Switch.t
  -> ctx:Eio_unix.Stdenv.base Ctx.t
  -> source:Selected_source.t
  -> ?max_spawned_jobs:int
  -> unit
  -> t

val agent_prompt_v1_name : string

(** Run the canonical agent_prompt_v1 body on a decoded request. This is the
    existing prompt fetch, configuration override, selected-target capture,
    same-target Context.derive, nested agent run and result encoding. Expected
    recipe failures remain [Error]; unexpected exceptions and cancellation
    retain their original identity and backtrace. No provider or actor is
    resolved here: [exec_context.ctx] and the optional inference override must
    already come from the owning host. Existing Ctx dispatch/submission guards
    are inherited into nested execution; [before_inference_attempt] composes
    before its observer. Decoding is deliberately outside this function, so
    callers retain their existing malformed-payload error boundary. *)
val call_agent_prompt_v1_strict
  :  exec_context
  -> request:Model_spawn_request.t
  -> ?inference_context:Inference_runtime.Context.t
  -> ?before_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> capture_recipe_target:(Inference.Request.Target.t -> (unit, string) result)
  -> unit
  -> (Moderation.Capabilities.model_call_result, string) result

(** An optional host guard is composed before the existing attempt observer and
    inherited by derived/nested recipe inference. Ctx.Inference_admission_rejected
    becomes the safe Model_error "model inference admission was revoked";
    cancellation is preserved. The guard does not grant target or tool authority.
    For a selected executor, expected recipe/source denial becomes Model_error,
    while unexpected exceptions and cancellation propagate to its joined
    child owner with their original identity/backtrace. The selected recipe
    is rebound on the shared body's derived Context before nested execution. *)
val recipe_agent_prompt_v1
  :  t
  -> session_id:string
  -> ?inference_context:Inference_runtime.Context.t
  -> ?before_inference_attempt:(Inference_runtime.Attempt.t -> unit)
  -> ?capture_recipe_target:(Inference.Request.Target.t -> (unit, string) result)
  -> unit
  -> Moderation.Capabilities.model_recipe

(** Register the moderator manager responsible for [session_id] so spawned
    model jobs can be reinjected as moderator internal events and can wake the
    host session when delivery succeeds.

    The wider lifecycle is documented in
    [docs-src/chatml-async-completion-lifecycle.md]. *)
val register_session
  :  ?on_wakeup:(unit -> unit)
  -> t
  -> session_id:string
  -> manager:Moderator_manager.t
  -> unit

(** Remove any registered wakeup callback for [session_id].

    Completed jobs may still be enqueued on the moderator manager after this,
    but the executor stops issuing host wakeups for that session. *)
val unregister_session_wakeup : t -> session_id:string -> unit

(** Await completion of a spawned job (test utility).

    This waits for executor completion, not for the host to drain the
    reinjected internal event. *)
val await_job : t -> job_id:string -> (unit, string) result

(** Observe and join jobs currently spawned for [session_id] under the executor's
    live switch, then retry delivery of every terminal result to its registered
    moderator manager. The returned counts are cumulative for this session at
    return: [delivered_events] counts results enqueued, not events handled.
    A failed enqueue or missing manager returns [Error] and retains the
    undelivered result for a later attempt, never apparent quiescence. Event
    handlers can spawn more jobs after this call; the selected host repeats
    settlement after each bounded drain and closes other spawn producers before
    its final observation. The constructor's finite [max_spawned_jobs] bounds
    the cumulative chain.

    This is the strict, opt-in hosted completion boundary. Recipe-domain
    Model_error/Model_refused results are delivered as failed job events.
    An escaped cancellation or unexpected worker exception is observed promptly
    even when another job is blocked, and retains its original identity and
    backtrace. The enclosing owner then cancels and joins the live switch; this
    call does not await blocked siblings before raising. A cancelled waiter
    likewise propagates cancellation. The operation does not drain moderator
    events, and callers must not hold the manager or source execution gate. *)
val settle_session : t -> session_id:string -> (Session_settlement.t, string) result

(** Nonyielding failure audit after this executor's exact local-job switch has
    joined following an intentional selected-child stop. Require every job for
    [session_id] to have a terminal phase and resolved worker promise. Ignore
    only [Eio.Cancel.Cancelled reason] whose [reason] is physically
    [allowed_cancelled_reason]; scan every job and give any other escaped
    worker exception priority over allowed cancellation, re-raising it with
    its retained raw backtrace. A pending/result-ready/inconsistent job
    returns [Error] rather than claiming a successful stop. This neither
    delivers moderator events nor replays a recipe. The trusted host owns the
    private stop reason and calls on the executor's creating Eio domain. *)
val audit_joined_session
  :  t
  -> session_id:string
  -> allowed_cancelled_reason:exn
  -> (unit, string) result

module Session_progress : sig
  type cursor

  type stop =
    | Progressed
    | All_completed
end

(** Capture one opaque, process-local progress point for this executor/session
    before observing the moderator queue. Counts are cumulative; this is not
    permission to deliver or drain. Call on the executor's owning Eio domain. *)
val session_progress_cursor : t -> session_id:string -> Session_progress.cursor

(** After the caller's queue observation, wait on the executor's existing
    condition for a new spawn or enqueued completion relative to [cursor], or
    for all current jobs to complete. Escaped exceptions/cancellation take
    priority and propagate with original identity/backtrace. A completed but
    undelivered job is retried before waiting, returning a typed [Error] on
    failed enqueue even if a sibling is still Pending. Successful enqueue
    broadcasts progress before a potentially yielding wakeup callback. This
    avoids waiting behind a blocked job whose release depends on the newly
    queued event. [Progressed] requires the caller to re-observe/drain the
    manager; [All_completed] requires strict [settle_session] and a final queue
    observation before claiming quiescence. The cursor must belong physically
    to [t]. Other spawn producers have been closed by the live owner before
    the final quiescence observation. *)
val await_session_progress
  :  t
  -> Session_progress.cursor
  -> (Session_progress.stop, string) result

(** Cumulative local job count for a session at this nonyielding observation.
    It includes still-running jobs and conveys no delivery or source authority.
    Call from the executor's owning Eio domain; a foreign-domain call is
    rejected before reading the mutable job table. *)
val session_job_count : t -> session_id:string -> int

(** [job_state t ~job_id] reports executor-local job completion state.

    This is narrower than the full lifecycle described in
    [docs-src/chatml-async-completion-lifecycle.md], because delivery to the
    moderator queue and host wakeup happen after ordinary job completion. *)
val job_state : t -> job_id:string -> job_state option
