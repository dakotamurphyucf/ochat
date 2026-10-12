open! Core

(** CLI presentation of already admitted whole-session live observations.
    No attachment, reader, reducer, history query, export or execution. *)
type t

(** Owns only the preceding rendered observation frame, bounded by [limits].
    Confined to the creating domain and one completion invocation. Caller
    serializes observations. No Projection, output sink or checkpoint is retained. *)
val create : History_chatmd.Rendering.Limits.t -> t

(** Renders actual current/terminal live Draft and tool activity from [projection],
    without traversing canonical or effective history. Returns [None] for an
    unchanged display frame. Changed frames explicitly replace preceding
    whole-session observations; they are not provider deltas or selected-run
    attribution. Missing prefixes/descriptors, gaps and cleared observations stay
    explicit. Operation completion never fabricates a tool outcome.

    Uses existing bounded Rendering admission, quoting and work primitives.
    Untrusted strings are quoted rather than interpreted as terminal controls.
    Complete frames are admitted before retention. Equality polls bounded chunks.
    Expected rendering failure preserves the preceding frame; original checkpoint
    exceptions, cancellation and backtraces propagate. Checkpoint must perform
    the caller's cancellation check. Bounds apply per callback, not to cumulative
    output across an arbitrarily long run.

    Caller writes successful frames to borrowed stderr outside reader callbacks.
    A typed limit error may produce an explicit omission notice; it must not
    trigger an unbounded rendering fallback. Canonical stdout/file export remains
    separate. Foreign-domain use raises Invalid_argument before mutation. *)
val observe
  :  t
  -> Agent_client.Projection.t
  -> checkpoint:(unit -> unit)
  -> (string option, History_chatmd.Rendering.Error.t) Result.t
