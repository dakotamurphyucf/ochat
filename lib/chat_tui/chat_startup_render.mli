(** Aggregate rendering of the initial transcript. *)

type snapshot =
  { transcript_generation : int
  ; render_generation : int
  ; width : int
  ; message_count : int
  ; theme_generation : int
  ; grammar_generation : int
  }

type completion =
  { snapshot : snapshot
  ; jobs : Chat_message_render_job.t list
  ; results : Chat_message_render_job.result list
  }

type outcome =
  | Completed of completion
  | Failed of exn
  | Cancelled

(** [snapshot ~model ~theme_generation ~grammar_generation] captures the
    immutable startup identity when the active history width is established. *)
val snapshot
  :  model:Model.t
  -> theme_generation:int
  -> grammar_generation:int
  -> snapshot option

val snapshot_is_current : snapshot -> model:Model.t -> bool

(** [render ...] renders all [jobs] in two isolated background domains and
    returns one aggregate outcome. No partial result is published. *)
val render
  :  domain_mgr:Eio.Domain_manager.ty Eio.Resource.t
  -> config:Chat_render_worker_runtime.Config.t
  -> code_cache_capacity:int
  -> is_cancelled:(unit -> bool)
  -> snapshot:snapshot
  -> jobs:Chat_message_render_job.t list
  -> outcome
