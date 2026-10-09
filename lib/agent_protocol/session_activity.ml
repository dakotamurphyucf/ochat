open! Core
module Error = Protocol_error
module J = Json_codec

module Reason = struct
  type t =
    | Approval
    | Input_required
    | Failure
    | Completion_pending
  [@@deriving compare, equal, sexp]

  let to_json = function
    | Approval -> `String "approval"
    | Input_required -> `String "input_required"
    | Failure -> `String "failure"
    | Completion_pending -> `String "completion_pending"
  ;;

  let of_json =
    J.enum
      ~name:"attention reason"
      [ "approval", Approval
      ; "input_required", Input_required
      ; "failure", Failure
      ; "completion_pending", Completion_pending
      ]
  ;;
end

module Attention = struct
  type entity =
    | Permission of Id.Permission.t
    | Operation of Id.Operation.t
    | Work of Session_work.Key.t
    | Session
  [@@deriving compare, equal, sexp]

  type t =
    { entity : entity
    ; reason : Reason.t
    ; unresolved : bool
    ; expired : bool
    }
  [@@deriving sexp]

  let entity_json = function
    | Permission id ->
      `Object [ "kind", `String "permission"; "id", Id.Permission.to_json id ]
    | Operation id ->
      `Object [ "kind", `String "operation"; "id", Id.Operation.to_json id ]
    | Work key -> `Object [ "kind", `String "work"; "key", Session_work.Key.to_json key ]
    | Session -> `Object [ "kind", `String "session" ]
  ;;

  let entity_of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind kind = J.required_as f "kind" J.string in
    match kind with
    | "permission" ->
      J.required_as f "id" Id.Permission.of_json
      |> Result.map ~f:(fun id -> Permission id)
    | "operation" ->
      J.required_as f "id" Id.Operation.of_json |> Result.map ~f:(fun id -> Operation id)
    | "work" ->
      J.required_as f "key" Session_work.Key.of_json
      |> Result.map ~f:(fun key -> Work key)
    | "session" -> Ok Session
    | _ -> Error (Error.invalid_request "unsupported attention entity")
  ;;

  let create ~entity ~(reason : Reason.t) ~unresolved ~expired =
    let open Result.Let_syntax in
    let%bind entity = entity_of_json (entity_json entity) in
    let meaningful =
      match entity, reason with
      | Permission _, Approval
      | (Session | Operation _), (Input_required | Failure)
      | Work _, (Failure | Completion_pending) -> true
      | Permission _, (Input_required | Failure | Completion_pending)
      | (Session | Operation _), (Approval | Completion_pending)
      | Work _, (Approval | Input_required) -> false
    in
    if not meaningful
    then Error (Error.invalid_request "attention reason does not apply to its entity")
    else if expired && not (Reason.equal reason Approval && unresolved)
    then Error (Error.invalid_request "expired attention must be unresolved approval")
    else Ok { entity; reason; unresolved; expired }
  ;;

  let to_json t =
    `Object
      [ "entity", entity_json t.entity
      ; "reason", Reason.to_json t.reason
      ; ("unresolved", if t.unresolved then `True else `False)
      ; ("expired", if t.expired then `True else `False)
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind entity = J.required_as f "entity" entity_of_json in
    let%bind reason = J.required_as f "reason" Reason.of_json in
    let%bind unresolved = J.required_as f "unresolved" J.bool in
    let%bind expired = J.required_as f "expired" J.bool in
    create ~entity ~reason ~unresolved ~expired
  ;;

  let compare_key a b =
    match compare_entity a.entity b.entity with
    | 0 -> Reason.compare a.reason b.reason
    | n -> n
  ;;

  let t_of_sexp sexp =
    let raw = t_of_sexp sexp in
    match of_json (to_json raw) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end

module Transient = struct
  type t =
    | Unavailable
    | Live of
        { tool_calls : int
        ; agent_calls : int
        }
  [@@deriving sexp]

  let to_json = function
    | Unavailable -> `Object [ "kind", `String "unavailable" ]
    | Live { tool_calls; agent_calls } ->
      `Object
        [ "kind", `String "live"
        ; "tool_calls", `Number (Int.to_string tool_calls)
        ; "agent_calls", `Number (Int.to_string agent_calls)
        ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = J.fields json in
    let%bind kind = J.required_as f "kind" J.string in
    match kind with
    | "unavailable" -> Ok Unavailable
    | "live" ->
      let%bind tool_calls =
        J.required_as f "tool_calls" (J.bounded_int ~min:0 ~max:1024)
      in
      let%bind agent_calls =
        J.required_as f "agent_calls" (J.bounded_int ~min:0 ~max:1024)
      in
      if agent_calls > tool_calls
      then Error (Error.invalid_request "agent activity must be subset of tool activity")
      else Ok (Live { tool_calls; agent_calls })
    | _ -> Error (Error.invalid_request "unsupported transient availability")
  ;;

  let t_of_sexp sexp =
    let raw = t_of_sexp sexp in
    match of_json (to_json raw) with
    | Ok t -> t
    | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
  ;;
end

type t =
  { summary : Session_activity_summary.t
  ; attention : Attention.t list
  ; work_count : int
  ; transient : Transient.t
  ; usage : Inference_query.Summary.t
  }
[@@deriving sexp]

let create ~summary ~attention ~work_count ~transient ~usage =
  let open Result.Let_syntax in
  let%bind transient = Transient.of_json (Transient.to_json transient) in
  if work_count < 0 || List.length attention > 4096
  then Error (Error.invalid_request "activity bounds exceeded")
  else (
    let attention = List.sort attention ~compare:Attention.compare_key in
    if List.contains_dup attention ~compare:Attention.compare_key
    then Error (Error.invalid_request "duplicate attention occurrence")
    else Ok { summary; attention; work_count; transient; usage })
;;

let to_json t =
  `Object
    [ "summary", Session_activity_summary.to_json t.summary
    ; "attention", `Array (List.map t.attention ~f:Attention.to_json)
    ; "work_count", `Number (Int.to_string t.work_count)
    ; "transient", Transient.to_json t.transient
    ; "usage", Inference_query.Summary.to_json t.usage
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind summary = J.required_as f "summary" Session_activity_summary.of_json in
  let%bind attention = J.required_as f "attention" (J.list Attention.of_json) in
  let%bind work_count =
    J.required_as f "work_count" (J.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind transient = J.required_as f "transient" Transient.of_json in
  let%bind usage = J.required_as f "usage" Inference_query.Summary.of_json in
  create ~summary ~attention ~work_count ~transient ~usage
;;

let t_of_sexp sexp =
  let raw = t_of_sexp sexp in
  match of_json (to_json raw) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;
