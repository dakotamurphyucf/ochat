(** Worker-owned rich-message rendering resources.

    Each runtime owns a fresh TextMate registry, an engine attached to that
    registry, a private code-image cache, and a private width-independent
    highlight cache. A runtime has one execution owner and must not be shared
    between domains. *)

module Config : sig
  type t

  (** [create ~custom_grammars ~theme_generation ~grammar_generation]
      snapshots the immutable configuration used to construct equivalent
      worker-local runtimes. *)
  val create
    :  custom_grammars:Highlight_grammar_discovery.Source.t list
    -> theme_generation:int
    -> grammar_generation:int
    -> t

  val theme_generation : t -> int
  val grammar_generation : t -> int
end

type t

(** [create ~config ~code_cache_capacity ()] builds fresh worker-local
    registry, decoded grammars, engine, and code cache.

    @raise Invalid_argument if [code_cache_capacity] is negative *)
val create : config:Config.t -> code_cache_capacity:int -> unit -> t Core.Or_error.t

(** [rebuild t ~config] constructs a complete replacement registry, engine,
    and empty cache before publishing it. The old generation is never mutated
    while rendering. *)
val rebuild : t -> config:Config.t -> unit Core.Or_error.t

val theme_generation : t -> int
val grammar_generation : t -> int

(** [render t job] renders [job] with [t]'s isolated resources.

    @raise Exit when [is_cancelled] requests cooperative cancellation. Rich
    code highlighting polls cancellation between tokenized lines.
    @raise Invalid_argument if the job's theme or grammar generation differs
    from the runtime generation *)
val render
  :  t
  -> ?is_cancelled:(unit -> bool)
  -> Chat_message_render_job.t
  -> Chat_message_render_job.result

module For_testing : sig
  val has_language : t -> string -> bool
  val code_cache_length : t -> int
  val highlight_cache_length : t -> int
  val prepared_cache_length : t -> int
  val wrapped_cache_stats : t -> int * int
end
