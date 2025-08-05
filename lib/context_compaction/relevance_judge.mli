(** Message–relevance scorer used by the {!module:Context_compaction.Compactor}
    when deciding which chat messages to keep below a token budget.

    At its core the module wraps a {!module:Meta_prompting.Evaluator.t}
    that contains a single {e judge}.  The judge runs OpenAI’s public
    *grader* endpoint with a purpose-built system prompt that asks the model
    to rate the {b importance} of a message on a continuous scale from
    [0] (irrelevant) to [1] (crucial).  The grader call is memoised and –
    if the environment variable {!b [OPENAI_API_KEY]} is missing – replaced by
    a deterministic offline fallback that always yields [0.5].  This makes
    unit tests reproducible and keeps the compaction pipeline usable in
    fully offline settings.

    The public surface is intentionally small: callers can obtain the raw
    floating-point relevance score via {!val:score_relevance} or apply the
    user-configurable threshold check via {!val:is_relevant}.  All other
    implementation details (self-consistency aggregation, exception guards,
    etc.) are private. *)

open! Core

(** [score_relevance ?env cfg ~prompt] scores the importance of
    [prompt] on the closed interval {[0,1]}.

    • A score of [0.] indicates the message can be dropped without
      harming the assistant’s ability to resume the conversation.

    • A score of [1.] marks the message as indispensable.

    The evaluation strategy is determined by [cfg] (see
    {!module:Context_compaction.Config}).  Currently only the
    threshold value is used – the function always consumes the full
    message text and does not look at [cfg.context_limit].

    The optional [env] parameter provides the Eio capabilities needed
    for network access.  If omitted or if the {!b [OPENAI_API_KEY]}
    variable is not set, the function falls back to the deterministic
    value [0.5].  This is exactly the midpoint of the default
    threshold and therefore preserves the original semantics in unit
    tests.

    Example (offline default):
    {[ let score = Relevance_judge.score_relevance Config.default ~prompt:"Hello" in
       Float.round_decimal score ~decimal_digits:3 (* => 0.5 *) ]}
    *)
val score_relevance : ?env:Eio_unix.Stdenv.base -> Config.t -> prompt:string -> float

(** [is_relevant ?env cfg ~prompt] returns [true] when the importance
    score is {>=} [cfg.relevance_threshold].  It is a thin wrapper
    around {!val:score_relevance} that saves the caller from manually
    comparing against the threshold.

    Example using a stricter custom threshold:
    {[ let cfg = { Config.default with relevance_threshold = 0.8 } in
       Relevance_judge.is_relevant cfg ~prompt:"FYI, lunch break" (* => false *) ]}
    *)
val is_relevant : ?env:Eio_unix.Stdenv.base -> Config.t -> prompt:string -> bool
