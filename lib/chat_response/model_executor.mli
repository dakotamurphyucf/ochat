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

(** Register the moderator manager responsible for [session_id] so spawned model
    jobs can be reinjected as internal events. *)
val register_session : t -> session_id:string -> manager:Moderator_manager.t -> unit

(** Await completion of a spawned job (test utility). *)
val await_job : t -> job_id:string -> (unit, string) result

val job_state : t -> job_id:string -> job_state option
