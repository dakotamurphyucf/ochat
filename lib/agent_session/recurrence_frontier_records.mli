open! Core

(** Private bounded planning records. No actor authority or arbitrary public
    restoration/import API. The frontier owns all universal carrier semantics. *)
module Summary : sig
  type t =
    { consumed : int64
    ; omitted : int64
    ; withdrawn : int64
    ; settled : int64
    }
  [@@deriving equal, sexp_of]
end

module Reason : sig
  type t =
    | Expired
    | Policy_skip
    | Superseded
    | Pending_capacity
    | Control_withdrawal
  [@@deriving equal, sexp_of]

  val to_string : t -> string
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

module Issue : sig
  type t =
    | Zone_unavailable
    | Coverage_unavailable
    | Timing_exhausted
    | Capacity_saturated
    | Initial_delay_missed
    | Delay_nominal_missed
    | Execution_interrupted
    | Delay_successor_exhausted
  [@@deriving equal, sexp_of]

  val to_string : t -> string
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

module Stored_nominal : sig
  type t

  val create : Recurrence_nominal.t -> (t, Agent_protocol.Error.t) result
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val value : t -> Recurrence_nominal.t
  val json : t -> Jsonaf.t
  val equal : t -> t -> bool
end

module Stored_alarm : sig
  type t

  val value : t -> Recurrence_alarm.t
  val json : t -> Jsonaf.t
  val create : Recurrence_alarm.t -> (t, Agent_protocol.Error.t) result
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val has_extensions : t -> bool
end

module Control_detail : sig
  type kind =
    | Retired_alarm
    | Retired_batch
    | Retired_delay_basis
  [@@deriving equal, sexp_of]

  type t =
    { kind : kind
    ; json : Jsonaf.t
    ; original_json : Jsonaf.t option
    }
end

module Mark : sig
  type t =
    { definition : Recurrence_identity.Definition.t
    ; definition_sha256 : string
    ; through : int64 option
    ; last_at : Time_ns.Alternate_sexp.t option
    ; omitted : int64
    ; withdrawn : int64
    ; settled : int64
    ; last_alarm_sequence : int64 option
    }
  [@@deriving equal]

  val initial : Recurrence_definition.t -> t
  val consumed : t -> (int64, Agent_protocol.Error.t) result
end

module Detail : sig
  type kind =
    | Skipped
    | Withdrawn
  [@@deriving equal, sexp_of]

  type t =
    { definition : Recurrence_identity.Definition.t
    ; first_slot : int64
    ; last_slot : int64
    ; kind : kind
    ; reason : Reason.t
    ; nominal : Stored_nominal.t option
    ; original_json : Jsonaf.t option
    }

  val count : t -> (int64, Agent_protocol.Error.t) result
  val identity : t -> string
end

module Batch : sig
  type t =
    { cutoff : Time_ns.Alternate_sexp.t
    ; candidates : Stored_nominal.t list
    ; original_json : Jsonaf.t option
    }
end

module State : sig
  type t =
    { revision : int64
    ; marks : Mark.t list
    ; ready : Stored_nominal.t list
    ; protected_children : Recurrence_child_reservation.t list
    ; child_associations : Recurrence_child_association.t list
    ; executions : Recurrence_executions.t
    ; delay_basis : Recurrence_delay_basis.t option
    ; batch : Batch.t option
    ; lookahead : Stored_nominal.t option
    ; alarm : Stored_alarm.t option
    ; issue : Issue.t option
    ; history : Detail.t list
    ; control_history : Control_detail.t list
    }

  val initial : Recurrence_definition.t -> t
  val unlinked_children : t -> Recurrence_child_reservation.t list
  val execution_rows : t -> Recurrence_execution.t list

  val find_execution
    :  t
    -> Recurrence_identity.Occurrence.t
    -> Recurrence_execution.t option

  val claimed_nominals : t -> Recurrence_nominal.t list
  val unresolved_count : t -> int
  val pending_count : t -> int
  val requires_execution_semantics : t -> bool
  val summary : t -> (Summary.t, Agent_protocol.Error.t) result
end

val add : int64 -> int64 -> (int64, Agent_protocol.Error.t) result
