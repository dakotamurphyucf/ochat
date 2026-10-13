open! Core

(** Immutable successful decoder results. Decoders must be pure and deterministic,
    and their results deeply immutable. Each token captures the actual decoder,
    including its configuration; readmission cannot substitute another decoder.
    Exact JSON equality includes object ordering and number spelling. Evidence
    supplies no native-state validation, custody or execution authority. *)
module Field : sig
  type 'a t

  val admit
    :  decode:(Jsonaf.t -> ('a, Agent_protocol.Error.t) Result.t)
    -> Jsonaf.t
    -> ('a t, Agent_protocol.Error.t) Result.t

  val readmit : 'a t -> json:Jsonaf.t -> ('a t, Agent_protocol.Error.t) Result.t
  val value : 'a t -> 'a
end

module Sequence : sig
  type 'a t

  val admit
    :  ?identity_field:string
    -> decode:(Jsonaf.t -> ('a, Agent_protocol.Error.t) Result.t)
    -> Jsonaf.t
    -> ('a t, Agent_protocol.Error.t) Result.t

  (** Reuses exact entries at the same index, or with an equal root string field
      when [identity_field] was supplied at admission. Keys are lookup hints;
      only exact complete JSON equality proves reuse. Missing/nonstring keys have
      no hint, and duplicate keys keep the first candidate without rejecting any
      input. Changed entries use the captured decoder. First-error order matches
      Json_codec.list. New tokens own only the new generation. An identity index
      adds O(n log n) construction time and O(n) storage; JSON comparisons and
      decoding are additional. [values] allocates a list sharing decoded values. *)
  val readmit : 'a t -> json:Jsonaf.t -> ('a t, Agent_protocol.Error.t) Result.t

  val values : 'a t -> 'a list
end
