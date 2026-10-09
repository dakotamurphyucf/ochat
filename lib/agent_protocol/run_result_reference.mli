(** Authorized bounded references, never retained plaintext tool/provider output.
    Read uses owning service/current transcript projection and rechecks authority. *)
type t = private
  | Job of Job_result_reference.t
  | Operation of
      { operation_id : Id.Operation.t
      ; generation : int
      ; history_ids : History.Id.t list
      ; revision : int64
      }
[@@deriving equal, sexp]

val validate : t -> (unit, Error.t) result

(** Wraps an admitted exact job outcome reference without changing its outcome. *)
val of_job_result : Job_result_reference.t -> (t, Error.t) result

val of_job : Job.t -> (t, Error.t) result

val operation
  :  operation_id:Id.Operation.t
  -> generation:int
  -> history_ids:History.Id.t list
  -> revision:int64
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
