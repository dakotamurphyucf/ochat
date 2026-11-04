open! Core
module P = Agent_protocol
module I = Recurrence_identity

let invalid message = Error (P.Error.invalid_request message)

let add old amount =
  if Int64.(old < 0L || amount < 0L || amount > max_value - old)
  then invalid "recurrence lifetime summary exhausted"
  else Ok Int64.(old + amount)
;;

module Summary = struct
  type t =
    { consumed : int64
    ; omitted : int64
    ; withdrawn : int64
    ; settled : int64
    }
  [@@deriving equal, sexp_of]
end

module Reason = struct
  type t =
    | Expired
    | Policy_skip
    | Superseded
    | Pending_capacity
    | Control_withdrawal
  [@@deriving equal, sexp_of]

  let values =
    [ "expired", Expired
    ; "policy_skip", Policy_skip
    ; "superseded", Superseded
    ; "pending_capacity", Pending_capacity
    ; "control_withdrawal", Control_withdrawal
    ]
  ;;

  let to_string = function
    | Expired -> "expired"
    | Policy_skip -> "policy_skip"
    | Superseded -> "superseded"
    | Pending_capacity -> "pending_capacity"
    | Control_withdrawal -> "control_withdrawal"
  ;;

  let of_json = P.Json_codec.enum ~name:"recurrence omission" values
end

module Issue = struct
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

  let to_string = function
    | Zone_unavailable -> "zone_unavailable"
    | Coverage_unavailable -> "coverage_unavailable"
    | Timing_exhausted -> "timing_exhausted"
    | Capacity_saturated -> "capacity_saturated"
    | Initial_delay_missed -> "initial_delay_missed"
    | Delay_nominal_missed -> "delay_nominal_missed"
    | Execution_interrupted -> "execution_interrupted"
    | Delay_successor_exhausted -> "delay_successor_exhausted"
  ;;

  let of_json =
    P.Json_codec.enum
      ~name:"recurrence planning issue"
      [ "zone_unavailable", Zone_unavailable
      ; "coverage_unavailable", Coverage_unavailable
      ; "timing_exhausted", Timing_exhausted
      ; "capacity_saturated", Capacity_saturated
      ; "initial_delay_missed", Initial_delay_missed
      ; "delay_nominal_missed", Delay_nominal_missed
      ; "execution_interrupted", Execution_interrupted
      ; "delay_successor_exhausted", Delay_successor_exhausted
      ]
  ;;
end

module Stored_nominal = struct
  type t =
    { value : Recurrence_nominal.t
    ; json : Jsonaf.t
    }

  let value t = t.value
  let json t = t.json

  let create value =
    let json =
      match Recurrence_nominal.to_json value with
      | `Object fields -> `Object (("status", `String "unadmitted") :: fields)
      | _ -> assert false
    in
    Document_schema.Json.validate ~limits:Recurrence_nominal.limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
    |> Result.map ~f:(fun () -> { value; json })
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () =
      Document_schema.Json.validate ~limits:Recurrence_nominal.limits json
      |> Result.map_error ~f:(fun e ->
        P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t e)))
    in
    let%bind fields = Persistence_codec.object_ json in
    let%bind status = Persistence_codec.required fields "status" P.Json_codec.string in
    let%bind () =
      if String.equal status "unadmitted"
      then Ok ()
      else invalid "unsupported nominal execution status"
    in
    let%map value = Recurrence_nominal.of_json json in
    { value; json }
  ;;

  let equal left right = Document_schema.Json.equal left.json right.json
end

module Stored_alarm = struct
  type t =
    { value : Recurrence_alarm.t
    ; json : Jsonaf.t
    }

  let value t = t.value
  let json t = t.json

  let create value =
    let json = Recurrence_alarm.to_json value in
    Document_schema.Json.validate ~limits:Recurrence_alarm.limits json
    |> Result.map_error ~f:(fun e ->
      P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t e)))
    |> Result.map ~f:(fun () -> { value; json })
  ;;

  let of_json json =
    Result.map (Recurrence_alarm.of_json json) ~f:(fun value -> { value; json })
  ;;

  let has_extensions t =
    not (Document_schema.Json.equal t.json (Recurrence_alarm.to_json t.value))
  ;;
end

module Control_detail = struct
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

module Mark = struct
  type t =
    { definition : I.Definition.t
    ; definition_sha256 : string
    ; through : int64 option
    ; last_at : Time_ns.Alternate_sexp.t option
    ; omitted : int64
    ; withdrawn : int64
    ; settled : int64
    ; last_alarm_sequence : int64 option
    }
  [@@deriving equal]

  let initial definition =
    { definition = Recurrence_definition.key definition
    ; definition_sha256 = Recurrence_definition.digest definition
    ; through = None
    ; last_at = None
    ; omitted = 0L
    ; withdrawn = 0L
    ; settled = 0L
    ; last_alarm_sequence = None
    }
  ;;

  let consumed t =
    match t.through with
    | None -> Ok 0L
    | Some through -> add through 1L
  ;;
end

module Detail = struct
  type kind =
    | Skipped
    | Withdrawn
  [@@deriving equal, sexp_of]

  type t =
    { definition : I.Definition.t
    ; first_slot : int64
    ; last_slot : int64
    ; kind : kind
    ; reason : Reason.t
    ; nominal : Stored_nominal.t option
    ; original_json : Jsonaf.t option
    }

  let count t =
    if Int64.(t.first_slot < 0L || t.last_slot < t.first_slot)
    then invalid "invalid recurrence retained range"
    else add Int64.(t.last_slot - t.first_slot) 1L
  ;;

  let identity t =
    String.concat
      ~sep:":"
      [ Int64.to_string (I.Definition.revision t.definition)
      ; Int64.to_string t.first_slot
      ; Int64.to_string t.last_slot
      ; (match t.kind with
         | Skipped -> "skipped"
         | Withdrawn -> "withdrawn")
      ; Reason.to_string t.reason
      ]
  ;;
end

module Batch = struct
  type t =
    { cutoff : Time_ns.Alternate_sexp.t
    ; candidates : Stored_nominal.t list
    ; original_json : Jsonaf.t option
    }
end

module State = struct
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

  let initial definition =
    { revision = 0L
    ; marks = [ Mark.initial definition ]
    ; ready = []
    ; protected_children = []
    ; child_associations = []
    ; executions = Recurrence_executions.empty
    ; delay_basis = None
    ; batch = None
    ; lookahead = None
    ; alarm = None
    ; issue = None
    ; history = []
    ; control_history = []
    }
  ;;

  let unlinked_children t =
    List.filter t.protected_children ~f:(fun reservation ->
      not
        (List.exists t.child_associations ~f:(fun association ->
           I.Occurrence.equal
             (Recurrence_child_association.occurrence association)
             (Recurrence_child_reservation.occurrence reservation))))
  ;;

  let execution_rows t = Recurrence_executions.rows t.executions
  let find_execution t occurrence = Recurrence_executions.find t.executions occurrence

  (* Lifetime settled totals represent terminal coordinates independently of
     bounded detail. Only unresolved rows participate in the remaining partition;
     a child reservation and its execution consume a single claim. *)
  let claimed_nominals t =
    List.filter (execution_rows t) ~f:Recurrence_execution.is_pending
    |> fun pending ->
    List.map pending ~f:(fun execution ->
      Recurrence_execution.intent execution |> Recurrence_execution_intent.nominal)
    @ List.filter_map t.protected_children ~f:(fun child ->
      match find_execution t (Recurrence_child_reservation.occurrence child) with
      | Some _ -> None
      | None -> Some (Recurrence_child_reservation.nominal child))
  ;;

  let unresolved_count t =
    List.count (execution_rows t) ~f:Recurrence_execution.is_pending
    + List.count t.protected_children ~f:(fun child ->
      Option.is_none (find_execution t (Recurrence_child_reservation.occurrence child)))
  ;;

  let pending_count t = List.length t.ready + unresolved_count t

  let requires_execution_semantics t =
    Recurrence_executions.requires_semantics t.executions
    || List.exists t.marks ~f:(fun mark -> not (Int64.equal mark.settled 0L))
    || Option.is_some t.delay_basis
    || List.exists t.control_history ~f:(fun detail ->
      Control_detail.equal_kind detail.kind Retired_delay_basis)
  ;;

  let summary t =
    List.fold_result
      t.marks
      ~init:Summary.{ consumed = 0L; omitted = 0L; withdrawn = 0L; settled = 0L }
      ~f:(fun sum (mark : Mark.t) ->
        let open Result.Let_syntax in
        let%bind count = Mark.consumed mark in
        let%bind consumed = add sum.consumed count in
        let%bind omitted = add sum.omitted mark.omitted in
        let%bind withdrawn = add sum.withdrawn mark.withdrawn in
        let%map settled = add sum.settled mark.settled in
        Summary.{ consumed; omitted; withdrawn; settled })
  ;;
end
