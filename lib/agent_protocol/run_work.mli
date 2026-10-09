(** Exact retained owned occurrence. This identity does not describe its outcome;
    the actor consults the actual work owner before wait/finish/custody transfer. *)
module Key : sig
  type t =
    | Operation of Id.Operation.t
    | Retained of Session_work.Key.t
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { key : Key.t
  ; generation : int
  }
[@@deriving compare, equal, sexp]

include Core.Comparator.S with type t := t

val validate : t -> (unit, Error.t) result
val create : key:Key.t -> generation:int -> (t, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result

module Terminal : sig
  type outcome =
    | Succeeded
    | Failed
    | Cancelled
    | Limited
    | Interrupted
    | Unconfirmed
  [@@deriving compare, equal, sexp]

  type work = t

  (* Immutable evidence captured from the actual owner before a later retry or
      relinquishment could replace its live status. Never rewritten to success. *)
  type t = private
    { work : work
    ; outcome : outcome
    ; revision : int64
    }
  [@@deriving equal, sexp]

  val validate : t -> (unit, Error.t) result
  val create : work:work -> outcome:outcome -> revision:int64 -> (t, Error.t) result
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end
