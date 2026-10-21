open! Core
module Error = Protocol_error
module C = Developer_codec
module D = Document_schema
module R = Developer_receipt

let ( let* ) result f = Result.bind result ~f
let get fields name decode = C.required fields name |> Result.bind ~f:decode

module Features = struct
  let run = "developer.run"
  let approvals = "developer.approvals"
  let events = "developer.events"
end

module Method = struct
  type t =
    | Capabilities
    | Submit
    | Observe
    | Await_terminal
    | Cancel_and_wait
    | Approval_list
    | Approval_respond
    | Approval_cancel
    | Watch
    | Unwatch
  [@@deriving compare, equal, sexp_of]

  let to_string = function
    | Capabilities -> "developer.capabilities"
    | Submit -> "developer.submit"
    | Observe -> "developer.observe"
    | Await_terminal -> "developer.await_terminal"
    | Cancel_and_wait -> "developer.cancel_and_wait"
    | Approval_list -> "developer.approval.list"
    | Approval_respond -> "developer.approval.respond"
    | Approval_cancel -> "developer.approval.cancel"
    | Watch -> "developer.watch"
    | Unwatch -> "developer.unwatch"
  ;;

  let of_string = function
    | "developer.capabilities" -> Ok Capabilities
    | "developer.submit" -> Ok Submit
    | "developer.observe" -> Ok Observe
    | "developer.await_terminal" -> Ok Await_terminal
    | "developer.cancel_and_wait" -> Ok Cancel_and_wait
    | "developer.approval.list" -> Ok Approval_list
    | "developer.approval.respond" -> Ok Approval_respond
    | "developer.approval.cancel" -> Ok Approval_cancel
    | "developer.watch" -> Ok Watch
    | "developer.unwatch" -> Ok Unwatch
    | _ -> C.invalid "unknown developer method"
  ;;
end

module Json_limits = struct
  let to_json t =
    `Object
      [ "max_bytes", C.integer_json (D.Limits.max_bytes t)
      ; "max_depth", C.integer_json (D.Limits.max_depth t)
      ; "max_fields", C.integer_json (D.Limits.max_fields t)
      ; "max_nodes", C.integer_json (D.Limits.max_nodes t)
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields ~allowed:[ "max_bytes"; "max_depth"; "max_fields"; "max_nodes" ] json
    in
    let* max_bytes = get fields "max_bytes" C.positive in
    let* max_depth = get fields "max_depth" C.positive in
    let* max_fields = get fields "max_fields" C.positive in
    let* max_nodes = get fields "max_nodes" C.positive in
    D.Limits.create ~max_bytes ~max_depth ~max_fields ~max_nodes
    |> Result.map_error ~f:(fun _ ->
      Error.invalid_request "invalid developer JSON limits")
  ;;
end

module Limits = struct
  type t =
    { request : D.Limits.t
    ; receipt : D.Limits.t
    ; response : D.Limits.t
    ; approval_page : D.Limits.t
    ; max_output_bytes : int
    ; max_fixture_steps : int
    ; max_fixture_bytes : int
    ; max_approval_display_bytes : int
    ; max_approval_decision_reason_bytes : int
    ; max_approval_page : int
    ; max_event_bytes : int
    }

  let finite value = value > 0 && value <= Authoring_codec.maximum_wire_integer

  let finite_json_limits limits =
    List.for_all
      [ D.Limits.max_bytes limits
      ; D.Limits.max_depth limits
      ; D.Limits.max_fields limits
      ; D.Limits.max_nodes limits
      ]
      ~f:finite
  ;;

  let create
        ~request
        ~receipt
        ~response
        ~approval_page
        ~max_output_bytes
        ~max_fixture_steps
        ~max_fixture_bytes
        ~max_approval_display_bytes
        ~max_approval_decision_reason_bytes
        ~max_approval_page
        ~max_event_bytes
    =
    if
      List.for_all
        [ max_output_bytes
        ; max_fixture_steps
        ; max_fixture_bytes
        ; max_approval_display_bytes
        ; max_approval_decision_reason_bytes
        ; max_approval_page
        ; max_event_bytes
        ]
        ~f:finite
      && List.for_all [ request; receipt; response; approval_page ] ~f:finite_json_limits
      && max_output_bytes <= D.Limits.max_bytes receipt
      && max_fixture_bytes <= D.Limits.max_bytes request
      && max_approval_display_bytes <= D.Limits.max_bytes approval_page
      && max_event_bytes <= D.Limits.max_bytes request
      && max_event_bytes <= D.Limits.max_bytes response
      && D.Limits.max_bytes receipt <= D.Limits.max_bytes response
      && D.Limits.max_bytes approval_page <= D.Limits.max_bytes response
    then
      Ok
        { request
        ; receipt
        ; response
        ; approval_page
        ; max_output_bytes
        ; max_fixture_steps
        ; max_fixture_bytes
        ; max_approval_display_bytes
        ; max_approval_decision_reason_bytes
        ; max_approval_page
        ; max_event_bytes
        }
    else C.invalid "invalid developer capability limits"
  ;;

  let request t = t.request
  let receipt t = t.receipt
  let response t = t.response
  let approval_page t = t.approval_page
  let max_output_bytes t = t.max_output_bytes
  let max_fixture_steps t = t.max_fixture_steps
  let max_fixture_bytes t = t.max_fixture_bytes
  let max_approval_display_bytes t = t.max_approval_display_bytes
  let max_approval_decision_reason_bytes t = t.max_approval_decision_reason_bytes
  let max_approval_page t = t.max_approval_page
  let max_event_bytes t = t.max_event_bytes

  let to_json t =
    `Object
      [ "request", Json_limits.to_json t.request
      ; "receipt", Json_limits.to_json t.receipt
      ; "response", Json_limits.to_json t.response
      ; "approval_page", Json_limits.to_json t.approval_page
      ; "max_output_bytes", C.integer_json t.max_output_bytes
      ; "max_fixture_steps", C.integer_json t.max_fixture_steps
      ; "max_fixture_bytes", C.integer_json t.max_fixture_bytes
      ; "max_approval_display_bytes", C.integer_json t.max_approval_display_bytes
      ; ( "max_approval_decision_reason_bytes"
        , C.integer_json t.max_approval_decision_reason_bytes )
      ; "max_approval_page", C.integer_json t.max_approval_page
      ; "max_event_bytes", C.integer_json t.max_event_bytes
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields
        ~allowed:
          [ "request"
          ; "receipt"
          ; "response"
          ; "approval_page"
          ; "max_output_bytes"
          ; "max_fixture_steps"
          ; "max_fixture_bytes"
          ; "max_approval_display_bytes"
          ; "max_approval_decision_reason_bytes"
          ; "max_approval_page"
          ; "max_event_bytes"
          ]
        json
    in
    let* request = get fields "request" Json_limits.of_json in
    let* receipt = get fields "receipt" Json_limits.of_json in
    let* response = get fields "response" Json_limits.of_json in
    let* approval_page = get fields "approval_page" Json_limits.of_json in
    let* max_output_bytes = get fields "max_output_bytes" C.positive in
    let* max_fixture_steps = get fields "max_fixture_steps" C.positive in
    let* max_fixture_bytes = get fields "max_fixture_bytes" C.positive in
    let* max_approval_display_bytes =
      get fields "max_approval_display_bytes" C.positive
    in
    let* max_approval_decision_reason_bytes =
      get fields "max_approval_decision_reason_bytes" C.positive
    in
    let* max_approval_page = get fields "max_approval_page" C.positive in
    let* max_event_bytes = get fields "max_event_bytes" C.positive in
    create
      ~request
      ~receipt
      ~response
      ~approval_page
      ~max_output_bytes
      ~max_fixture_steps
      ~max_fixture_bytes
      ~max_approval_display_bytes
      ~max_approval_decision_reason_bytes
      ~max_approval_page
      ~max_event_bytes
  ;;
end

module Capabilities = struct
  type t =
    { format_version : int
    ; methods : string list
    ; limits : Limits.t
    }

  let max_json_bytes = 64 * 1024

  let bootstrap_limits =
    D.Limits.create ~max_bytes:max_json_bytes ~max_depth:8 ~max_fields:128 ~max_nodes:256
    |> Result.map_error ~f:(fun _ -> "invalid fixed Developer bootstrap limits")
    |> Result.ok_or_failwith
  ;;

  let rec create ~format_version ~methods ~limits =
    if
      format_version <> 1
      || List.length methods > 32
      || List.length methods
         <> List.length (List.dedup_and_sort methods ~compare:String.compare)
      || not
           (List.for_all methods ~f:(fun method_ ->
              (not (String.is_empty method_)) && String.length method_ <= 128))
    then C.invalid "invalid developer capabilities"
    else
      let* () =
        Result.all (List.map methods ~f:Method.of_string) |> Result.map ~f:ignore
      in
      let t = { format_version; methods; limits } in
      let* () = C.validate ~limits:bootstrap_limits (to_json t) in
      Ok t

  and to_json t =
    `Object
      [ "format_version", C.integer_json t.format_version
      ; "methods", `Array (List.map t.methods ~f:(fun value -> `String value))
      ; "limits", Limits.to_json t.limits
      ]
  ;;

  let format_version t = t.format_version

  let supports t method_ =
    List.mem t.methods (Method.to_string method_) ~equal:String.equal
  ;;

  let limits t = t.limits

  let of_json json =
    let* () = C.validate ~limits:bootstrap_limits json in
    let* fields = C.fields ~allowed:[ "format_version"; "methods"; "limits" ] json in
    let* format_version = get fields "format_version" C.positive in
    let* methods =
      get fields "methods" (C.array ~max_items:32 (C.string ~max_bytes:128))
    in
    let* limits = get fields "limits" Limits.of_json in
    create ~format_version ~methods ~limits
  ;;
end

module Owner = struct
  type t = Execution_owner.developer

  let create ~host_id ~run_id ~epoch ~principal_id =
    if epoch > Authoring_codec.maximum_wire_integer
    then C.invalid "developer owner epoch exceeds wire bound"
    else (
      match Execution_owner.developer ~host_id ~run_id ~epoch ~principal_id with
      | Ok (Execution_owner.Developer owner) -> Ok owner
      | Ok (Execution_owner.Conversation _) -> assert false
      | Error _ -> C.invalid "invalid developer owner coordinates")
  ;;

  let to_json (t : t) =
    `Object
      [ "host_id", Id.Server.to_json t.host_id
      ; "run_id", Id.Developer_run.to_json t.run_id
      ; "epoch", C.integer_json t.epoch
      ; "principal_id", Id.Principal.to_json t.principal_id
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields ~allowed:[ "host_id"; "run_id"; "epoch"; "principal_id" ] json
    in
    let* host_id = get fields "host_id" Id.Server.of_json in
    let* run_id = get fields "run_id" Id.Developer_run.of_json in
    let* epoch = get fields "epoch" C.nonnegative in
    let* principal_id = get fields "principal_id" Id.Principal.of_json in
    create ~host_id ~run_id ~epoch ~principal_id
  ;;
end
