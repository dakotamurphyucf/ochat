(** Persistent configuration record used by the context-compaction
    pipeline.

    Applications can ship a user-writable JSON file to tweak the
    heuristics employed by {!Context_compaction.Relevance_judge} and
    {!Context_compaction.Summarizer}.  If no file is found the library
    falls back on the hard-coded {!default} parameters so that it
    remains completely dependency-free and offline-friendly.

    {1 JSON schema}

    The configuration file must contain a single JSON object with any
    combination of the following keys:

    {ul
    {- ["context_limit"] – maximum number of tokens that the relevance
       judge is allowed to inspect before deciding which messages to
       keep.  Defaults to {!default.context_limit}.}
    {- ["relevance_threshold"] – minimum score (in the inclusive range
       {{:https://en.wikipedia.org/wiki/Unit_interval} [0.0, 1.0]}) a
       message has to achieve in order to survive filtering.  Defaults
       to {!default.relevance_threshold}.}}

    Unknown keys are silently ignored so that future versions can add
    more parameters without breaking backwards compatibility.

    {1 Search path}

    [load] inspects the following locations, returning the first file
    that exists and parses successfully:

    {ol
    {- [$XDG_CONFIG_HOME/ochat/context_compaction.json] or, if the
       variable is unset, [$HOME/.config/ochat/context_compaction.json];}
    {- [$HOME/.ochat/context_compaction.json].}}

    Malformed JSON files are skipped.  If no valid file is found the
    default configuration is returned.

    {1 Usage example}

    {[
      let cfg = Context_compaction.Config.load () in
      let open Context_compaction in
      let judge =
        Relevance_judge.create
          ~context_limit:cfg.context_limit
          ~threshold:cfg.relevance_threshold
      in
      (* … *)
    ]}

    @canonical Context_compaction.Config *)

type t =
  { context_limit : int
    (** Maximum number of tokens that can be considered when
            computing relevance. *)
  ; relevance_threshold : float
    (** Messages with a relevance score **≥** this threshold are
            preserved. *)
  }

(** Hard-coded defaults used when no configuration file is present.

    By default the library is conservative and keeps as much context as
    reasonably possible while discarding messages with a very low
    importance score:

    {[
      { context_limit = 20_000;
        relevance_threshold = 0.5; }
    ]} *)
val default : t

(** [load ()] returns the first valid configuration found in the
    *search&nbsp;path* (see the {b Search path} section above) or {!default}
    if none is available.  The function never raises – unreadable or
    malformed files are skipped. *)
val load : unit -> t

(** Thin wrapper around this module that avoids naming collisions with
    the ubiquitous [Config] module from [compiler-libs]. *)
module Compact_config : sig
  val default : t
  val load : unit -> t
end
