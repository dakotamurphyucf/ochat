open Core
module Range = Chatmd_parser_input.Range

module Limits : sig
  type t

  val create
    :  max_occurrences:int
    -> max_parser_inputs:int
    -> max_input_bytes:int
    -> max_output_bytes:int
    -> max_work_steps:int
    -> max_nodes:int
    -> max_depth:int
    -> max_payloads:int
    -> max_payload_bytes:int
    -> document:Document_schema.Limits.t
    -> t Or_error.t

  val occurrences : t -> int
  val parser_inputs : t -> int
  val input_bytes : t -> int
  val output_bytes : t -> int
  val work_steps : t -> int
  val nodes : t -> int
  val depth : t -> int
  val payloads : t -> int
  val payload_bytes : t -> int
  val document : t -> Document_schema.Limits.t
end

module Error : sig
  type t

  val invalid : ?range:Range.t -> string -> t
  val document : Document_schema.Error.t -> t
  val storage : Agent_store.Store_error.t -> t
  val conversion : Range.t -> Webpage_markdown.Bounded_conversion.Error.t -> t
  val range : t -> Range.t option
  val sexp_of_t : t -> Sexp.t
end

module Accounting : sig
  type t

  val zero : t
  val occurrences : t -> int
  val input_bytes : t -> int
  val output_bytes : t -> int
  val work_steps : t -> int
  val nodes : t -> int

  val create
    :  occurrences:int
    -> input_bytes:int
    -> output_bytes:int
    -> work_steps:int
    -> nodes:int
    -> t
end

(** One computation's logical ledger; no callback retained in returned receipts. *)
type t

val create : Limits.t -> checkpoint:(unit -> unit) -> t
val limits : t -> Limits.t
val checkpoint : t -> unit
val work : t -> int -> (unit, Error.t) Result.t
val work_product : t -> int -> int -> (unit, Error.t) Result.t
val map_work : t -> int -> (unit, Error.t) Result.t
val count : t -> 'a list -> maximum:int -> (int, Error.t) Result.t
val add : t -> Accounting.t -> (unit, Error.t) Result.t
val accounting : t -> Accounting.t
val hash : t -> string -> (string, Error.t) Result.t
val equal_bytes : t -> string -> string -> (bool, Error.t) Result.t
