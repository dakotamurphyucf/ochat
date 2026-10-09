(** Host admission result, separate from transport uncertainty and run lifecycle.
    Rejected means no run commit was attempted/accepted; completed constructor
    facts remain durable. Uncertain means the final write lacks a known accepted
    receipt and must be reconciled without repeating effects. *)
type t =
  | Admitted of Agent_protocol.Run_receipt.t
  | Rejected of Agent_protocol.Error.t
  | Uncertain of Agent_protocol.Error.t
