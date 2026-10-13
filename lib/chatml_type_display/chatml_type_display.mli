open Core
module Frozen = Chatml_typechecker.Type_snapshot

(** Versioned [chatml-display-v1] IDE presentation of immutable frozen type
    graphs. This is neither executable ChatML annotation syntax nor a type
    equality key. No mutable inference cells, Eio, protocol, UI, runtime
    surface or host authority enter this library. *)

module Limits : sig
  type t

  (** Positive finite ceilings for ONE display session, which may render many
      roots in one authoring query. Node and edge ceilings count the unique
      reachable graph scan for each render, summed across the session. The
      work ceiling separately charges every presentation revisit, identity
      lookup, label byte/hash/comparison and ordinary emitted UTF-8 fragment
      before allocation. The constant three-byte truncation marker uses only
      the remaining byte reserve. Depth is an additional guard, not the sole
      work bound.
      [max_text_bytes] bounds each item for a protocol or terminal presenter;
      [max_output_bytes] bounds their aggregate session output. *)
  val create
    :  max_nodes:int
    -> max_edges:int
    -> max_depth:int
    -> max_work_steps:int
    -> max_text_bytes:int
    -> max_output_bytes:int
    -> t Or_error.t
end

module Kind : sig
  type t =
    | Type
    | Signature
  [@@deriving equal, sexp_of]
end

module Completeness : sig
  type reason =
    | Node_limit
    | Edge_limit
    | Depth_limit
    | Work_limit
    | Byte_limit
  [@@deriving equal, sexp_of]

  type t =
    | Complete
    | Truncated of reason
  [@@deriving equal, sexp_of]
end

module Error : sig
  type t =
    | Foreign_root
    | Invalid_max_bytes
  [@@deriving sexp_of]
end

type t

val kind : t -> Kind.t
val completeness : t -> Completeness.t
val text : t -> string

module Session : sig
  (** Borrowed invocation-owned accounting. A session is not shared across
      fibers/domains or retained after the request. A failed or truncated
      attempt still debits the work it performed; it does not mutate a Frozen
      graph or publish a partially complete item. *)
  type session

  val create : limits:Limits.t -> session

  (** Render a root only when [Frozen.find] admits this owner's ID. Function
      argument count, tuple and variant payload arity, record/module/field
      receiver distinctions, raw row override meaning, grouped quantified
      identity, distinct unquantified variables and lexical recursive binders
      survive presentation. Ordinary closed records, variants, tuples and
      functions use familiar type-help notation; any raw/internal fallback is
      explicitly marked. Same-spelled distinct IDs in one rendered root get
      deterministic distinct labels. Separate binding schemes may each use
      the familiar ['a] spelling. Cyclic links remain finite.
      Internal Row/Payload/Shape nodes are not alleged source value types.
      Escaping arbitrary invalid UTF-8/control label bytes yields valid UTF-8.

      Optional [max_bytes] further restricts THIS render to the caller's
      current remaining aggregate output allowance. The effective cap is the
      minimum of this value, per-item text capacity and session output space.
      Zero returns typed Truncated Byte_limit with empty text; a negative value
      returns Invalid_max_bytes. Omitting it retains the original session
      behavior. The session still charges actual returned bytes and all work;
      the caller can debit those bytes from its own combined result budget.

      A cap returns Truncated with the first violated reason. If even an ASCII
      truncation marker would exceed the remaining byte budget, text may be
      empty; it is never Complete. Traversal is stack-safe with a bounded
      buffer and charged identity lookup, not unbounded recursive rendering,
      repeated string concatenation or uncharged List.mem. Checkpoint
      exceptions retain identity/backtrace, with no callback stored. *)
  val render
    :  session
    -> ?max_bytes:int
    -> Frozen.t
    -> root:Frozen.Id.t
    -> kind:Kind.t
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t
end
