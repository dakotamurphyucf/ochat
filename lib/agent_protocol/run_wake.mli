(** Exact host-issued delivered occurrence. A whole schedule/subscription ID is
    insufficient: the actor validates source/install/generation and actual owner
    against its bound timer count, job attempt or subscription delivery/epoch. *)
module Occurrence : sig
  type t =
    | Job_completion of
        { job_id : Id.Job.t
        ; attempt : int
        }
    | Delivered_timer of
        { schedule_id : Id.Schedule.t
        ; delivery_count : int
        ; creator : Job.launch_owner
        ; subscription : (Id.Subscription.t * int) option
        }
    | Subscription_delivery of
        { subscription_id : Id.Subscription.t
        ; epoch : int
        ; delivery_id : Id.Delivery.t
        ; creator : Job.launch_owner
        }
  [@@deriving equal, sexp]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) result
end

type t = private
  { run_id : Id.Run.t
  ; source : Run_source.t
  ; occurrence : Occurrence.t
  }
[@@deriving equal, sexp]

val validate : t -> (unit, Error.t) result

val create
  :  run_id:Id.Run.t
  -> source:Run_source.t
  -> occurrence:Occurrence.t
  -> (t, Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
