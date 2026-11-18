module P = Agent_protocol
module Carrier = Goal_policy_contributions_carrier
module Receipt = Goal_policy_receipt
module Pending = Receipt.Pending
module Request = Receipt.Request

module Disposition : sig
  type t =
    | Added
    | Replaced
    | Duplicate
    | Stale
  [@@deriving equal, sexp_of]
end

module Update : sig
  type accounting = Carrier.t

  module Reason : sig
    type t =
      | Initialize_goal of P.Id.Goal.t
      | Reserve of Request.t
      | Import of Policy_canonical_usage.Projection.t
      | Close_before_writer of Pending.t
  end

  type t

  val candidate : t -> accounting
  val reason : t -> Reason.t
  val shape : Document_schema.Shape.t
  val to_json : t -> (Jsonaf.t, P.Error.t) result
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

val initialize_goal
  :  Carrier.t option
  -> owner:P.Session_ref.t
  -> goal:P.Goal.t
  -> account:Goal_accounting_document.t
  -> before_ledger:Inference_ledger.t
  -> (Update.t, P.Error.t) result

module Reserve_result : sig
  type t =
    | Reserved of Update.t * Pending.t
    | Already_pending of Pending.t
    | Busy of Pending.t
end

val reserve
  :  Carrier.t option
  -> owner:P.Session_ref.t
  -> request:Request.t
  -> (Reserve_result.t, P.Error.t) result

val import
  :  Carrier.t
  -> projection:Policy_canonical_usage.Projection.t
  -> (Update.t * Disposition.t, P.Error.t) result

val close_before_writer : Carrier.t -> pending:Pending.t -> (Update.t, P.Error.t) result
