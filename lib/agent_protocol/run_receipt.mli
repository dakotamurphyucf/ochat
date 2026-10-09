(** Immutable local admission/action receipt. A receipt is evidence, never current
    execution authority. Lost transport replies are reconciled without execution. *)
module Kind : sig
  type t =
    | Admission
    | Action
    | Terminal
  [@@deriving compare, equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { run_id : Id.Run.t
  ; principal_id : Id.Principal.t
  ; source : Run_source.t
  ; key : Idempotency_key.t
  ; request_sha256 : string
  ; kind : Kind.t
  ; run_revision : int64
  ; session_revision : int64
  ; committed_at : Timestamp.t
  }
[@@deriving equal, sexp]

val create
  :  run_id:Id.Run.t
  -> principal_id:Id.Principal.t
  -> source:Run_source.t
  -> key:Idempotency_key.t
  -> request_sha256:string
  -> kind:Kind.t
  -> run_revision:int64
  -> session_revision:int64
  -> committed_at:Timestamp.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
