open! Core
module P = Agent_protocol
module J = P.Json_codec

type hold =
  | Capacity
  | Capacity_unknown
  | Authority_unavailable
  | Required_input_unavailable
  | Preparation_interrupted
[@@deriving equal, sexp_of]

type finish =
  | Script_finished
  | Cancelled
[@@deriving equal, sexp_of]

type phase =
  | Awaiting
  | Waiting
  | Preparing of P.Id.Operation.t
  | Held of hold
  | Admitted of
      { operation : P.Id.Operation.t
      ; ordinal : int64
      }
  | Finished of finish
[@@deriving equal, sexp_of]

type t =
  { owner : P.Id.Operation.t
  ; predecessor : P.Id.Operation.t
  ; phase : phase
  }
[@@deriving equal, sexp_of]

module Hook = struct
  type decision =
    | Continue
    | Wait
    | Finish

  type view =
    { window : P.Recovery_window.Scope.t
    ; checkpoint : [ `Checkpointed | `Archive_only ]
    }
end

type action =
  | Continue
  | Wait_for_wake
  | Finish_followup
  | Cancel_followup

type change =
  | Wait
  | Begin of P.Id.Operation.t
  | Hold of
      { operation : P.Id.Operation.t
      ; reason : hold
      }
  | Admit of
      { operation : P.Id.Operation.t
      ; ordinal : int64
      }
  | Finish of finish
  | Stop_preparation of
      { operation : P.Id.Operation.t
      ; reason : finish
      }

let invalid message = Error (P.Error.invalid_request message)

let fresh_operation t operation =
  not
    (P.Id.Operation.equal operation t.owner
     || P.Id.Operation.equal operation t.predecessor)
;;

let validate t =
  if P.Id.Operation.equal t.owner t.predecessor
  then invalid "rollover continuation overlaps its predecessor"
  else (
    match t.phase with
    | Awaiting | Waiting | Held _ | Finished _ -> Ok t
    | Preparing operation when fresh_operation t operation -> Ok t
    | Admitted { operation; ordinal }
      when fresh_operation t operation && Int64.(ordinal > 0L) -> Ok t
    | Preparing _ | Admitted _ -> invalid "invalid rollover continuation admission")
;;

let create ~owner ~predecessor = validate { owner; predecessor; phase = Awaiting }

let apply t change =
  let open Result.Let_syntax in
  let%bind phase =
    match t.phase, change with
    | (Awaiting | Waiting | Held _), Wait -> Ok Waiting
    | (Awaiting | Waiting | Held _), Begin operation -> Ok (Preparing operation)
    | Preparing actual, Hold { operation; reason }
      when P.Id.Operation.equal actual operation -> Ok (Held reason)
    | Preparing actual, Admit { operation; ordinal }
      when P.Id.Operation.equal actual operation -> Ok (Admitted { operation; ordinal })
    | Preparing actual, Stop_preparation { operation; reason }
      when P.Id.Operation.equal actual operation -> Ok (Finished reason)
    | (Awaiting | Waiting | Held _), Finish reason -> Ok (Finished reason)
    | _ ->
      Error
        (P.Error.create
           Conflict
           ~retryable:true
           ~message:"rollover continuation changed or already admitted"
           ())
  in
  validate { t with phase }
;;

let is_pending t =
  match t.phase with
  | Awaiting | Waiting | Preparing _ | Held _ -> true
  | Admitted _ | Finished _ -> false
;;

let hold_names =
  [ "capacity", Capacity
  ; "capacity_unknown", Capacity_unknown
  ; "authority_unavailable", Authority_unavailable
  ; "required_input_unavailable", Required_input_unavailable
  ; "preparation_interrupted", Preparation_interrupted
  ]
;;

let finish_names = [ "script_finished", Script_finished; "cancelled", Cancelled ]

let hold_name = function
  | Capacity -> "capacity"
  | Capacity_unknown -> "capacity_unknown"
  | Authority_unavailable -> "authority_unavailable"
  | Required_input_unavailable -> "required_input_unavailable"
  | Preparation_interrupted -> "preparation_interrupted"
;;

let finish_name = function
  | Script_finished -> "script_finished"
  | Cancelled -> "cancelled"
;;

let phase_json = function
  | Awaiting -> `Object [ "kind", `String "awaiting" ]
  | Waiting -> `Object [ "kind", `String "waiting" ]
  | Held reason ->
    `Object [ "kind", `String "held"; "reason", `String (hold_name reason) ]
  | Preparing operation ->
    `Object [ "kind", `String "preparing"; "operation", P.Id.Operation.to_json operation ]
  | Admitted { operation; ordinal } ->
    `Object
      [ "kind", `String "admitted"
      ; "operation", P.Id.Operation.to_json operation
      ; "ordinal", `Number (Int64.to_string ordinal)
      ]
  | Finished reason ->
    `Object [ "kind", `String "finished"; "reason", `String (finish_name reason) ]
;;

let phase_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind kind = J.required_as fields "kind" J.string in
  let operation () = J.required_as fields "operation" P.Id.Operation.of_json in
  let reason name codes = J.required_as fields "reason" (J.enum ~name codes) in
  match kind with
  | "awaiting" -> Ok Awaiting
  | "waiting" -> Ok Waiting
  | "held" -> reason "rollover hold reason" hold_names >>| fun value -> Held value
  | "preparing" -> operation () >>| fun value -> Preparing value
  | "admitted" ->
    let%bind operation = operation () in
    let%map ordinal =
      J.required_as fields "ordinal" (J.bounded_int64 ~min:1L ~max:Int64.max_value)
    in
    Admitted { operation; ordinal }
  | "finished" ->
    reason "rollover finish reason" finish_names >>| fun value -> Finished value
  | _ -> invalid "unknown rollover continuation phase"
;;

let to_json t =
  `Object
    [ "owner", P.Id.Operation.to_json t.owner
    ; "predecessor", P.Id.Operation.to_json t.predecessor
    ; "phase", phase_json t.phase
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let%bind owner = J.required_as fields "owner" P.Id.Operation.of_json in
  let%bind predecessor = J.required_as fields "predecessor" P.Id.Operation.of_json in
  let%bind phase = J.required_as fields "phase" phase_of_json in
  validate { owner; predecessor; phase }
;;

let change_to_json = function
  | Wait -> phase_json Waiting
  | Begin operation -> phase_json (Preparing operation)
  | Admit { operation; ordinal } -> phase_json (Admitted { operation; ordinal })
  | Finish finish -> phase_json (Finished finish)
  | Stop_preparation { operation; reason } ->
    `Object
      [ "kind", `String "finished"
      ; "operation", P.Id.Operation.to_json operation
      ; "reason", `String (finish_name reason)
      ]
  | Hold { operation; reason } ->
    `Object
      [ "kind", `String "held"
      ; "operation", P.Id.Operation.to_json operation
      ; "reason", `String (hold_name reason)
      ]
;;

let change_of_json json =
  let open Result.Let_syntax in
  match%bind phase_of_json json with
  | Waiting -> Ok Wait
  | Preparing id -> Ok (Begin id)
  | Admitted { operation; ordinal } -> Ok (Admit { operation; ordinal })
  | Finished reason ->
    let%bind fields = J.fields json in
    let%map operation = J.optional_as fields "operation" P.Id.Operation.of_json in
    (match operation with
     | None -> Finish reason
     | Some operation -> Stop_preparation { operation; reason })
  | Held reason ->
    let%bind fields = J.fields json in
    let%map operation = J.required_as fields "operation" P.Id.Operation.of_json in
    Hold { operation; reason }
  | Awaiting -> invalid "awaiting is not a continuation transition"
;;
