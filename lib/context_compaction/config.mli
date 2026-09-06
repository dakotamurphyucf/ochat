(** Eio-backed configuration for conversation compaction. *)

type t =
  { context_limit : int
    (** Positive o200k_base serialized-history token estimate limit, with eight
        extra tokens per item. Not the provider's exact full-request budget. *)
  ; relevance_threshold : float
    (** Finite score in [0,1], used only when relevance filtering is enabled. *)
  ; relevance_filtering : bool
    (** Opt-in grading before summarization; may incur additional provider calls. *)
  }

(** 20,000 estimated tokens, threshold 0.5, filtering disabled. *)
val default : t

val is_valid : t -> bool

(** [load ~env ()] reads the first valid configuration through Eio, from
    [$XDG_CONFIG_HOME/ochat/context_compaction.json] (defaulting to
    [$HOME/.config/ochat/context_compaction.json]) then
    [$HOME/.ochat/context_compaction.json]. Invalid or unreadable files are
    skipped. Without [env], returns defaults without I/O.
    @raise Eio.Cancel.Cancelled on cancellation. *)
val load : ?env:Eio_unix.Stdenv.base -> unit -> t

(** [load_paths ~env paths] returns the first valid configuration in [paths].
    Duplicate keys and incorrect known-field types are invalid. *)
val load_paths : env:Eio_unix.Stdenv.base -> string list -> t

module Compact_config : sig
  val default : t
  val load : ?env:Eio_unix.Stdenv.base -> unit -> t
end
