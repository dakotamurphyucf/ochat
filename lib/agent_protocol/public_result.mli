(** Public success values. Only get/create/attach replace private inline history
    containers. The internal Method_result codec remains the durable receipt
    codec; projecting a result never changes a cached internal success. *)
module Non_history : sig
  type t [@@deriving sexp_of]

  (** Exhaustive method whitelist excluding get/create/attach. This excludes
      inline snapshot/history containers, not authority or disclosure: an export
      result can still reference an artifact containing history. *)
  val of_internal : Method_result.t -> (t, Error.t) result

  val value : t -> Method_result.t
end

module Attach : sig
  type replay =
    | Current
    | Events of Public_durable_event.t list
    | Snapshot of Public_snapshot.t
  [@@deriving sexp_of]

  type t =
    { attachment : Session.Attachment.t
    ; replay : replay
    ; latest_event_sequence : int64
    ; reclaim_token : string option
    }
  [@@deriving sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

module Create : sig
  type t =
    { session : Session.t
    ; mutation : Mutation_result.t
    ; attachment : Attach.t option
    }
  [@@deriving sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t =
  | Session_get of Public_snapshot.t
  | Session_attach of Attach.t
  | Session_create of Create.t
  | Non_history of Non_history.t
[@@deriving sexp_of]

val method_name : t -> string
val to_json : t -> Jsonaf.t
val of_json : method_:string -> Jsonaf.t -> (t, Error.t) result

(** Checks complete response bounds and the same container and child invariants
    as wire admission. Validation retains the original result unchanged. *)
val validate : t -> (unit, Error.t) result
