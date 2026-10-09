(** Captured actor installation identity, independent of checkpoint revisions.
    Epoch is monotone within a session and changes on replacement/removal/reset.
    A matching runtime restore does not grant or extend source authority. *)
type t = private
  { observer : Invocation.observer
  ; generation : int
  ; installation_epoch : int64
  }
[@@deriving equal, sexp]

val create
  :  observer:Invocation.observer
  -> generation:int
  -> installation_epoch:int64
  -> (t, Error.t) result

val validate : t -> (unit, Error.t) result
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
