open Chatml_lang

(** Pure composition over the existing runtime variants. Receivers and callback
    results are checked for the required tags and arities. Malformed values raise
    [Failure]; callback failures and cancellation propagate, never becoming an
    [Error] or [None] value. Each selected callback is invoked exactly once. *)
module Outcome : sig
  val map : value -> f:value -> value
  val bind : value -> f:value -> value
  val map_error : value -> f:value -> value
end

module Option : sig
  val map : value -> f:value -> value
  val bind : value -> f:value -> value
end

(** Visit the input left to right, retaining only [Some] payloads in a fresh
    shallow array. The input is not snapshotted: callback mutations to later input
    elements follow the existing eager array traversal convention. Failure stops
    traversal. O(n) time plus callback work, O(n) result storage. *)
val filter_map : value array -> f:value -> value array
