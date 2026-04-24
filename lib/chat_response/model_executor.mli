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

val agent_prompt_v1_name : string

val recipe_agent_prompt_v1
  :  t
  -> session_id:string
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

(** [job_state t ~job_id] reports executor-local job completion state.

    This is narrower than the full lifecycle described in
    [docs-src/chatml-async-completion-lifecycle.md], because delivery to the
    moderator queue and host wakeup happen after ordinary job completion. *)
val job_state : t -> job_id:string -> job_state option
