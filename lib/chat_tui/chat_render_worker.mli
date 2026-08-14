(** Bounded priority service for detached Chat rendering.

    Submission and result acceptance are UI-domain operations. Worker domains
    receive immutable jobs/configuration, own their highlighting runtime, and
    only publish immutable results. *)

type t

type submit_result =
  | Queued
  | Already_pending
  | Rejected

type direction =
  | Preferred
  | Neutral
  | Opposite

type ranked_job =
  { job : Chat_message_render_job.t
  ; distance : int
  ; direction : direction
  }

type retry_result =
  | Retried
  | Exhausted
  | Stale

(** [create ...] starts [worker_count] domain workers under [sw].

    [queue_capacity] bounds accepted queued work; in-flight work is bounded by
    [worker_count]. Both counts must be positive. [now] supplies monotonic
    timestamps for render-latency instrumentation. *)
val create
  :  sw:Eio.Switch.t
  -> domain_mgr:Eio.Domain_manager.ty Eio.Resource.t
  -> config:Chat_render_worker_runtime.Config.t
  -> worker_count:int
  -> queue_capacity:int
  -> code_cache_capacity:int
  -> on_result:(Chat_message_render_job.result -> unit)
  -> on_error:(Chat_message_render_job.t -> exn -> unit)
  -> now:(unit -> Mtime.t)
  -> unit
  -> t

(** [submit t job] coalesces work for the same message/selected variant and
    queues the newest request according to its priority. An equivalent pending
    request keeps its existing priority. Dispatch is FIFO
    within each priority and prefers visible, then prefetch, then background
    work. When at least two workers exist, one lane is reserved for all visible
    work, including generation-scoped resize rows. At capacity, prefetch may
    evict background or older prefetch work;
    visible work may additionally evict older visible work. Background work
    never evicts other work. This function never blocks. *)
val submit : t -> Chat_message_render_job.t -> submit_result

(** [submit_batch t ~resize_generation ~batch_id jobs] submits one bounded
    progressive-width batch. Scheduling identity includes the resize
    generation, target width, stable row identity and revision, and batch. *)
val submit_batch
  :  t
  -> resize_generation:int
  -> batch_id:int
  -> ranked_job list
  -> submit_result list

(** [reprioritize_batch t ...] updates queued tier, distance, and directional
    ordering without invalidating equivalent completed or in-flight work. *)
val reprioritize_batch
  :  t
  -> resize_generation:int
  -> batch_id:int
  -> ranked_job list
  -> unit

(** [cancel_generation t ~resize_generation] removes queued work and
    cooperatively cancels in-flight work belonging to the resize generation. *)
val cancel_generation : t -> resize_generation:int -> unit

(** [available_slots t] returns how many additional jobs can be admitted
    without eviction or rejection. *)
val available_slots : t -> int

(** [submit_visible_viewport t jobs] invalidates queued, staged, or in-flight
    visible work outside [jobs], then submits the newest viewport. *)
val submit_visible_viewport : t -> Chat_message_render_job.t list -> unit

(** [accepts_result t result] consumes and accepts [result] only when it
    matches the latest request for its message/selected variant. *)
val accepts_result : t -> Chat_message_render_job.result -> bool

(** [reject t job] consumes the latest matching request after worker failure.
    It returns whether [job] was still current. *)
val reject : t -> Chat_message_render_job.t -> bool

(** [retry_failed t job] retries a current failed job once through the
    detached worker pool. It returns [Exhausted] after the retry also fails
    and [Stale] when cancellation or replacement already superseded [job]. *)
val retry_failed : t -> Chat_message_render_job.t -> retry_result

(** [force_synchronous t job] makes the matching message variant render
    synchronously on its next visible redraw. *)
val force_synchronous : t -> Chat_message_render_job.t -> unit

(** [should_render_synchronously t job] consumes a matching failure marker. *)
val should_render_synchronously : t -> Chat_message_render_job.t -> bool

(** [submit_prefetch_viewport t jobs] replaces queued prefetch work for the
    previous viewport with [jobs]. Visible and in-flight work is preserved. *)
val submit_prefetch_viewport : t -> Chat_message_render_job.t list -> unit

(** [update_config t config] publishes a new immutable runtime configuration.
    Workers rebuild their private resources before rendering jobs from the new
    generation. Queued old-generation work is discarded. *)
val update_config : t -> Chat_render_worker_runtime.Config.t -> unit

(** [close t] rejects new work, discards queued/pending requests, and prevents
    worker callbacks after the UI reducer stops. It also asks every worker
    domain to terminate after any active renderer returns. The operation is
    idempotent. *)
val close : t -> unit

val theme_generation : t -> int
val grammar_generation : t -> int

(** [record_synchronous_fallback t] records a visible render performed on the
    UI domain because [t] rejected its detached job. *)
val record_synchronous_fallback : t -> unit

(** [record_failure t] records a detached render failure delivered to the UI
    domain. *)
val record_failure : t -> unit

(** [metrics_json t] snapshots cumulative submission, coalescing, dropping,
    completion, stale-result, fallback, queue-depth, and render-latency
    metrics. *)
val metrics_json : t -> Jsonaf.t

module For_testing : sig
  type stats =
    { queued : int
    ; pending : int
    ; queue_capacity : int
    ; worker_count : int
    }

  val stats : t -> stats

  (** [queued_jobs t] returns queued work in dispatch order without changing
      [t]. *)
  val queued_jobs : t -> Chat_message_render_job.t list

  (** [create_detached ...] creates queue state without fibers for
      deterministic admission and stale-result tests. *)
  val create_detached
    :  config:Chat_render_worker_runtime.Config.t
    -> queue_capacity:int
    -> worker_count:int
    -> t

  (** [create ...] constructs a worker service with an injected domain-safe
      renderer for deterministic queue and lifecycle tests. *)
  val create
    :  sw:Eio.Switch.t
    -> domain_mgr:Eio.Domain_manager.ty Eio.Resource.t
    -> config:Chat_render_worker_runtime.Config.t
    -> worker_count:int
    -> queue_capacity:int
    -> render:
         (unit
          -> (unit -> bool)
          -> Chat_render_worker_runtime.Config.t
          -> Chat_message_render_job.t
          -> Chat_message_render_job.result)
    -> on_result:(Chat_message_render_job.result -> unit)
    -> on_error:(Chat_message_render_job.t -> exn -> unit)
    -> now:(unit -> Mtime.t)
    -> unit
    -> t

  (** [runtime_renderer ~code_cache_capacity] is the production worker-local
      rendering adapter. *)
  val runtime_renderer
    :  code_cache_capacity:int
    -> (unit -> bool)
    -> Chat_render_worker_runtime.Config.t
    -> Chat_message_render_job.t
    -> Chat_message_render_job.result

  val metrics_json : t -> Jsonaf.t
end
