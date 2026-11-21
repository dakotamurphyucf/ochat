open! Core
module Session_capacity = Agent_session.Session_context_capacity
module Capacity = Inference_runtime.Context_capacity

(** Reviewed limits for the exact target captured from one installed prompt.
    The named profile and model must match the actual resolved target. *)
module Target : sig
  type t =
    { prompt : string
    ; profile : string
    ; model : string
    ; encoder : Capacity.Encoder.t
    ; total : int64
    ; input : int64 option
    ; output : int64 option
    ; session_total : int64 option
    ; omitted_output_default : int64 option
    ; checkpoint_reserve : int64
    ; warning_lead : int64
    ; estimate_margin : int64
    }
  [@@deriving compare, equal, sexp]

  val model_limit : t -> Capacity.Policy.model_limit
  val allowance : t -> host_total:int64 -> Capacity.Policy.allowance
  val estimate : t -> Capacity.Policy.estimate
end

type t = private
  { mode : Session_capacity.Mode.t
  ; host_total : int64
  ; targets : Target.t list
  }
[@@deriving compare, equal, sexp]

val max_targets : int

(** Positive caps and admitted reserve geometry against the model, host and
    session bounds. At most one qualification per prompt, up to [max_targets].
    Absence from Server configuration leaves the shipped host off. *)
val create
  :  mode:Session_capacity.Mode.t
  -> host_total:int64
  -> targets:Target.t list
  -> (t, Agent_protocol.Error.t) result
