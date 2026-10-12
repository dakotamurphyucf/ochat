open! Core

(** Pure neutral ChatMD presentation; no provider decoder, protocol or I/O.
    Opaque payloads are inspectable. Rendered markup is not replay authority. *)
val render : History_entry.t list -> string

val render_payload : History_entry.Id.t -> History_entry.Payload.t -> string
val history_id : History_entry.Id.t -> string
val role : History_entry.Payload.Role.t -> string
val raw : string -> string

(** Legacy moderator companion content without an invented host occurrence. *)
val render_anonymous : History_entry.Payload.t -> string

module Rendering : sig
  module Error : sig
    type t =
      | Invalid_limits
      | Output_limit
      | Work_limit
      | Json_error of Document_schema.Error.t
    [@@deriving equal, sexp_of]
  end

  module Limits : sig
    type t

    (** Positive complete output-byte and render-work ceilings. JSON also obeys
        the supplied byte/depth/node/field limits, tightened to remaining output
        and work before validation. No default output policy is introduced. *)
    val create
      :  max_bytes:int
      -> max_work_steps:int
      -> json_limits:Document_schema.Limits.t
      -> (t, Error.t) Result.t
  end

  type t

  (** Invokes [f] once, synchronously, to stage exact-sized immutable pieces.
      Expanded quoting/JSON and the complete buffer are allocated only after
      complete admission. The returned string retains no input owner/callback.
      The borrowed output is confined to the creating domain and invocation;
      using it afterward or from another domain raises an invariant exception.

      Only private limit rejection becomes Error. Original callback failures,
      cancellation and backtraces propagate. Text copying and quote measurement
      poll chunks. Existing Printf quoting, JSON validation and Jsonaf encoding
      are finite nonpreemptible stages with checkpoints before/after.
      This pure library does not check Eio cancellation itself: the supplied
      checkpoint must raise it, for example by calling [Eio.Fiber.check].

      Work counts semantic/projection cells and atoms plus admitted scan/copy
      bytes: literal bytes, quoted input plus twice encoded bytes, and three
      compact JSON bytes. It is neither VM fuel nor exact heap accounting. JSON
      key validation can cost O(B log F), with caller-bounded depth and fields.
      Staged cells are O(work); complete output and each encoded intermediate
      are bounded by max_bytes. Encoder/validator auxiliary storage is bounded
      by the admitted input/JSON limits, not an exact allocation ratio. *)
  val render
    :  Limits.t
    -> checkpoint:(unit -> unit)
    -> f:(t -> unit)
    -> (string, Error.t) Result.t

  (** Existing renderer compatibility only: no new validation/work/byte policy.
      Uses the same semantic emission; the output still cannot escape [f]. *)
  val render_unbounded : f:(t -> unit) -> string

  (** Narrow emission primitives shared by neutral payloads and provenance/
      disclosure templates. Empty pieces still cost an atom. [prefix]/[suffix]
      are admitted with [value] before concatenation/quoting. All additions are
      checked. *)
  val text : t -> string -> unit

  val quoted : t -> ?prefix:string -> ?suffix:string -> string -> unit

  (** Uses actual ID components so an arbitrarily long namespace is admitted
      before the complete formatted ID is constructed. Exact [Id.to_string]
      bytes are preserved inside the quoted attribute. *)
  val quoted_history_id : t -> History_entry.Id.t -> unit

  val json : t -> Jsonaf.t -> unit

  (** Charge actual bounded traversal/projection before allocating metadata.
      Negative charges reject; not a sandbox for arbitrary user callbacks. *)
  val charge : t -> steps:int -> unit

  val payload : t -> entry_id:History_entry.Id.t option -> History_entry.Payload.t -> unit
end

val render_bounded
  :  History_entry.t list
  -> limits:Rendering.Limits.t
  -> checkpoint:(unit -> unit)
  -> (string, Rendering.Error.t) Result.t
