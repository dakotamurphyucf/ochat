(** Compact durable statement about a completely acknowledged foreground cut.
    The live writer must additionally prove the original worker and attempt. *)
open! Core

module P = Agent_protocol

type pair =
  { call : P.History.Id.t
  ; output : P.History.Id.t
  }
[@@deriving equal, sexp_of]

type t = private
  { operation : P.Id.Operation.t
  ; ordinal : int64
  ; source_revision : int64
  ; pairs : pair list
  }
[@@deriving equal, sexp_of]

val create
  :  operation:P.Id.Operation.t
  -> ordinal:int64
  -> source_revision:int64
  -> pair list
  -> (t, P.Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
