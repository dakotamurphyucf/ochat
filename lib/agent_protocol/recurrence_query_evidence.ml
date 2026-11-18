open! Core
module J = Json_codec
module W = Recurrence_wire

let error_json kind = `String (Protocol_error.code_to_string kind)
let error_of_json json = Result.bind (J.string json) ~f:Protocol_error.code_of_string

module Definition_id = struct
  type t =
    { controller : Session_ref.t
    ; recurrence_id : Id.Recurrence.t
    ; revision : int64
    }

  let to_json t =
    `Object
      [ "controller", Session_ref.to_json t.controller
      ; "recurrence_id", Id.Recurrence.to_json t.recurrence_id
      ; "revision", W.counter_json t.revision
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "controller"; "recurrence_id"; "revision" ] in
    let%bind controller = J.required_as fields "controller" W.session in
    let%bind recurrence_id = J.required_as fields "recurrence_id" Id.Recurrence.of_json in
    let%map revision = J.required_as fields "revision" W.counter in
    { controller; recurrence_id; revision }
  ;;
end

module Occurrence_id = struct
  type t =
    { definition : Definition_id.t
    ; slot : int64
    }

  let to_json t =
    `Object
      [ "definition", Definition_id.to_json t.definition; "slot", W.counter_json t.slot ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "definition"; "slot" ] in
    let%bind definition = J.required_as fields "definition" Definition_id.of_json in
    let%map slot = J.required_as fields "slot" W.counter in
    { definition; slot }
  ;;
end

module Nominal = struct
  type t =
    { occurrence : Occurrence_id.t
    ; at : Timestamp.t
    }

  let to_json t =
    `Object
      [ "occurrence", Occurrence_id.to_json t.occurrence; "at", Timestamp.to_json t.at ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "occurrence"; "at" ] in
    let%bind occurrence = J.required_as fields "occurrence" Occurrence_id.of_json in
    let%map at = J.required_as fields "at" Timestamp.of_json in
    { occurrence; at }
  ;;
end

module Outcome = struct
  type t =
    | Completed
    | Failed of Protocol_error.code option
    | Cancelled
    | Limited
    | Interrupted
  [@@deriving equal]

  let to_json = function
    | Completed -> W.tagged "completed" []
    | Failed code -> W.tagged "failed" [ "code", W.option_json error_json code ]
    | Cancelled -> W.tagged "cancelled" []
    | Limited -> W.tagged "limited" []
    | Interrupted -> W.tagged "interrupted" []
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    match kind with
    | "failed" ->
      let%bind () = Extension_codec.closed fields [ "kind"; "code" ] in
      let%map code = J.required_as fields "code" (W.nullable error_of_json) in
      Failed code
    | "completed" | "cancelled" | "limited" | "interrupted" ->
      let%bind () = Extension_codec.closed fields [ "kind" ] in
      J.enum
        ~name:"recurrence Run outcome"
        [ "completed", Completed
        ; "cancelled", Cancelled
        ; "limited", Limited
        ; "interrupted", Interrupted
        ]
        (`String kind)
    | _ -> W.invalid "unsupported recurrence Run outcome"
  ;;
end

module Run_observation = struct
  type status =
    | Admitted
    | Active
    | Waiting
    | Terminal of Outcome.t

  let status_to_json = function
    | Admitted -> W.tagged "admitted" []
    | Active -> W.tagged "active" []
    | Waiting -> W.tagged "waiting" []
    | Terminal outcome -> Outcome.to_json outcome
  ;;

  let status_of_json json =
    let open Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    match kind with
    | "admitted" | "active" | "waiting" ->
      let%bind () = Extension_codec.closed fields [ "kind" ] in
      J.enum
        ~name:"recurrence Run status"
        [ "admitted", Admitted; "active", Active; "waiting", Waiting ]
        (`String kind)
    | _ -> Result.map (Outcome.of_json json) ~f:(fun outcome -> Terminal outcome)
  ;;

  type t =
    { id : Id.Run.t
    ; revision : int64
    ; status : status
    ; retained_outcome : Outcome.t option
    }

  let to_json t =
    `Object
      [ "id", Id.Run.to_json t.id
      ; "revision", W.counter_json t.revision
      ; "status", status_to_json t.status
      ; "retained_outcome", W.option_json Outcome.to_json t.retained_outcome
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "id"; "revision"; "status"; "retained_outcome" ] in
    let%bind id = J.required_as fields "id" Id.Run.of_json in
    let%bind revision = J.required_as fields "revision" W.counter in
    let%bind status = J.required_as fields "status" status_of_json in
    let%bind retained_outcome =
      J.required_as fields "retained_outcome" (W.nullable Outcome.of_json)
    in
    let t = { id; revision; status; retained_outcome } in
    match t.status, t.retained_outcome with
    | Terminal _, None | (Admitted | Active | Waiting), None -> Ok t
    | Terminal outcome, Some retained when Outcome.equal outcome retained -> Ok t
    | _, Some _ -> W.invalid "recurrence retained outcome differs from observed Run"
  ;;
end

module Target = struct
  type t =
    | Denied
    | Unavailable
    | Visible of
        { session : Session_ref.t
        ; session_revision : int64
        ; run : Run_observation.t option
        }

  let to_json = function
    | Denied -> W.tagged "denied" []
    | Unavailable -> W.tagged "unavailable" []
    | Visible { session; session_revision; run } ->
      W.tagged
        "visible"
        [ "session", Session_ref.to_json session
        ; "session_revision", W.counter_json session_revision
        ; "run", W.option_json Run_observation.to_json run
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind kind, fields = W.tag json in
    match kind with
    | "denied" | "unavailable" ->
      let%map () = Extension_codec.closed fields [ "kind" ] in
      if String.equal kind "denied" then Denied else Unavailable
    | "visible" ->
      let%bind () =
        Extension_codec.closed fields [ "kind"; "session"; "session_revision"; "run" ]
      in
      let%bind session = J.required_as fields "session" W.session in
      let%bind session_revision = J.required_as fields "session_revision" W.counter in
      let%map run = J.required_as fields "run" (W.nullable Run_observation.of_json) in
      Visible { session; session_revision; run }
    | _ -> W.invalid "unsupported recurrence target visibility"
  ;;
end

module Attempt = struct
  type t =
    { number : int
    ; rejection : Protocol_error.code option
    ; uncertainty : Protocol_error.code option
    ; retry_resolution_recorded : bool
    ; admission_recorded : bool
    ; terminal_recorded : bool
    ; target : Target.t
    }

  let to_json t =
    `Object
      [ "number", W.integer_json t.number
      ; "rejection", W.option_json error_json t.rejection
      ; "uncertainty", W.option_json error_json t.uncertainty
      ; "retry_resolution_recorded", W.bool_json t.retry_resolution_recorded
      ; "admission_recorded", W.bool_json t.admission_recorded
      ; "terminal_recorded", W.bool_json t.terminal_recorded
      ; "target", Target.to_json t.target
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "number"
        ; "rejection"
        ; "uncertainty"
        ; "retry_resolution_recorded"
        ; "admission_recorded"
        ; "terminal_recorded"
        ; "target"
        ]
    in
    let%bind number = J.required_as fields "number" (J.bounded_int ~min:0 ~max:7) in
    let%bind rejection = J.required_as fields "rejection" (W.nullable error_of_json) in
    let%bind uncertainty =
      J.required_as fields "uncertainty" (W.nullable error_of_json)
    in
    let%bind retry_resolution_recorded =
      J.required_as fields "retry_resolution_recorded" J.bool
    in
    let%bind admission_recorded = J.required_as fields "admission_recorded" J.bool in
    let%bind terminal_recorded = J.required_as fields "terminal_recorded" J.bool in
    let%bind target = J.required_as fields "target" Target.of_json in
    let retained_outcome_agrees =
      match target with
      | Visible { run = Some run; _ } ->
        Bool.equal terminal_recorded (Option.is_some run.retained_outcome)
      | Denied | Unavailable | Visible { run = None; _ } -> true
    in
    if
      (terminal_recorded && not admission_recorded)
      || (retry_resolution_recorded && admission_recorded)
      || not retained_outcome_agrees
    then W.invalid "recurrence attempt evidence disagrees"
    else
      Ok
        { number
        ; rejection
        ; uncertainty
        ; retry_resolution_recorded
        ; admission_recorded
        ; terminal_recorded
        ; target
        }
  ;;
end

module Occurrence = struct
  type t =
    { nominal : Nominal.t
    ; attempts : Attempt.t list
    }

  let to_json t =
    `Object
      [ "nominal", Nominal.to_json t.nominal
      ; "attempts", W.array Attempt.to_json t.attempts
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = W.fields json [ "nominal"; "attempts" ] in
    let%bind nominal = J.required_as fields "nominal" Nominal.of_json in
    let%bind attempts = J.required_as fields "attempts" (W.list 8 Attempt.of_json) in
    let t = { nominal; attempts } in
    if List.is_empty t.attempts
    then W.invalid "recurrence occurrence requires retained attempt evidence"
    else Ok t
  ;;
end

module Protected_creation = struct
  type t =
    { nominal : Nominal.t
    ; link_recorded : bool
    ; execution_recorded : bool
    }

  let to_json t =
    `Object
      [ "nominal", Nominal.to_json t.nominal
      ; "link_recorded", W.bool_json t.link_recorded
      ; "execution_recorded", W.bool_json t.execution_recorded
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields =
      W.fields json [ "nominal"; "link_recorded"; "execution_recorded" ]
    in
    let%bind nominal = J.required_as fields "nominal" Nominal.of_json in
    let%bind link_recorded = J.required_as fields "link_recorded" J.bool in
    let%map execution_recorded = J.required_as fields "execution_recorded" J.bool in
    { nominal; link_recorded; execution_recorded }
  ;;
end
