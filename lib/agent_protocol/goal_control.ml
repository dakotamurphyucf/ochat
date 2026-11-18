open! Core
module J = Json_codec
module W = Goal_wire

module Features = struct
  let control = "sessions.goal_control"
end

let command_method = "session.goal.command"
let start_method = "session.goal.start"
let resume_method = "session.goal.resume"
let request_max_bytes = Run_limits.max_document_bytes + 8192
let request_max_depth = Run_limits.max_depth + 2

module Start = struct
  type t =
    { generation : int
    ; expected_revision : int64
    ; source : Run_source.t
    ; goal_id : Id.Goal.t
    ; expected_goal_revision : int64
    ; expected_catalog_revision : int64
    ; input : Run_input.t
    ; key : Idempotency_key.t
    }

  let run_request_sha256 t ~session_id =
    Run_start.headless_request_sha256
      ~session_id
      ~generation:t.generation
      ~expected_revision:t.expected_revision
      ~mode:Workflow
      ~input:t.input
      ~key:t.key
  ;;

  let association_sha256
        ~goal_id
        ~expected_goal_revision
        ~expected_catalog_revision
        ~expected_source
    =
    let version, source_fields =
      match expected_source with
      | None -> "goal-episode-admission-v1", []
      | Some source ->
        "goal-episode-admission-v2", [ "expected_source", Run_source.to_json source ]
    in
    `Object
      ([ "version", `String version
       ; "goal_id", Id.Goal.to_json goal_id
       ; "expected_goal_revision", W.counter_json expected_goal_revision
       ; "expected_catalog_revision", W.counter_json expected_catalog_revision
       ]
       @ source_fields)
    |> Jsonaf.to_string
    |> Digestif.SHA256.digest_string
    |> Digestif.SHA256.to_hex
  ;;

  let to_json t =
    `Object
      [ "generation", W.generation_json t.generation
      ; "expected_revision", W.counter_json t.expected_revision
      ; "source", Run_source.to_json t.source
      ; "goal_id", Id.Goal.to_json t.goal_id
      ; "expected_goal_revision", W.counter_json t.expected_goal_revision
      ; "expected_catalog_revision", W.counter_json t.expected_catalog_revision
      ; "input", Run_input.to_json t.input
      ; "key", Idempotency_key.to_json t.key
      ]
  ;;

  let create
        ~generation
        ~expected_revision
        ~source
        ~goal_id
        ~expected_goal_revision
        ~expected_catalog_revision
        ~input
        ~key
    =
    let open Core.Result.Let_syntax in
    let%bind () = Run_source.validate source in
    let%bind () = Run_input.validate input in
    let%bind () = Extension_codec.validate_id Id.Goal.to_json Id.Goal.of_json goal_id in
    let%bind () =
      Extension_codec.validate_id Idempotency_key.to_json Idempotency_key.of_json key
    in
    if
      (not (Int.equal generation source.generation))
      || Int64.(
           expected_revision < 0L
           || expected_goal_revision < 0L
           || expected_catalog_revision < 0L)
    then W.invalid "invalid Goal Start fences"
    else (
      let t =
        { generation
        ; expected_revision
        ; source
        ; goal_id
        ; expected_goal_revision
        ; expected_catalog_revision
        ; input
        ; key
        }
      in
      let%map () =
        Extension_codec.validate_json
          ~max_bytes:(Run_limits.max_document_bytes + 4096)
          ~max_depth:(request_max_depth - 1)
          (to_json t)
      in
      t)
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "generation"
        ; "expected_revision"
        ; "source"
        ; "goal_id"
        ; "expected_goal_revision"
        ; "expected_catalog_revision"
        ; "input"
        ; "key"
        ]
    in
    let%bind generation = J.required_as fields "generation" W.generation in
    let%bind expected_revision = J.required_as fields "expected_revision" W.counter in
    let%bind source = J.required_as fields "source" W.source in
    let%bind goal_id = J.required_as fields "goal_id" Id.Goal.of_json in
    let%bind expected_goal_revision =
      J.required_as fields "expected_goal_revision" W.counter
    in
    let%bind expected_catalog_revision =
      J.required_as fields "expected_catalog_revision" W.counter
    in
    let%bind input = J.required_as fields "input" Run_input.of_json in
    let%bind key = J.required_as fields "key" Idempotency_key.of_json in
    create
      ~generation
      ~expected_revision
      ~source
      ~goal_id
      ~expected_goal_revision
      ~expected_catalog_revision
      ~input
      ~key
  ;;
end

module Request = struct
  type 'intent target =
    { session : Session_ref.t
    ; attachment_id : Id.Attachment.t
    ; intent : 'intent
    }

  type t =
    | Command of Goal_command.t target
    | Start of Start.t target
    | Resume of Goal_resume_request.t target

  let target ~session ~attachment_id intent =
    let open Core.Result.Let_syntax in
    let%bind () =
      Extension_codec.validate_id
        Id.Attachment.to_json
        Id.Attachment.of_json
        attachment_id
    in
    let%map _ = W.session (Session_ref.to_json session) in
    { session; attachment_id; intent }
  ;;

  let check_session session session_id =
    if Id.Session.equal (Session_ref.session_id session) session_id
    then Ok ()
    else W.invalid "Goal command refers to another session"
  ;;

  let command ~session ~attachment_id (intent : Goal_command.t) =
    let%bind.Result () = check_session session intent.session_id in
    let%map.Result target = target ~session ~attachment_id intent in
    Command target
  ;;

  let start ~session ~attachment_id intent =
    let%map.Result target = target ~session ~attachment_id intent in
    Start target
  ;;

  let resume ~session ~attachment_id intent =
    let%bind.Result () = check_session session (Goal_resume_request.session_id intent) in
    let%map.Result target = target ~session ~attachment_id intent in
    Resume target
  ;;

  let session = function
    | Command target -> target.session
    | Start target -> target.session
    | Resume target -> target.session
  ;;

  let attachment_id = function
    | Command target -> target.attachment_id
    | Start target -> target.attachment_id
    | Resume target -> target.attachment_id
  ;;

  let methods = [ command_method; start_method; resume_method ]

  let method_name = function
    | Command _ -> command_method
    | Start _ -> start_method
    | Resume _ -> resume_method
  ;;

  let params request =
    let intent =
      match request with
      | Command target -> Goal_command.to_json target.intent
      | Start target -> Start.to_json target.intent
      | Resume target -> Goal_resume_request.to_json target.intent
    in
    `Object
      [ "session", Session_ref.to_json (session request)
      ; "attachment_id", Id.Attachment.to_json (attachment_id request)
      ; "intent", intent
      ]
  ;;

  let of_method_and_params ~method_ ~params =
    let open Core.Result.Let_syntax in
    let%bind () =
      Extension_codec.validate_json
        ~max_bytes:request_max_bytes
        ~max_depth:request_max_depth
        params
    in
    let%bind fields = W.fields params [ "session"; "attachment_id"; "intent" ] in
    let%bind session = J.required_as fields "session" W.session in
    let%bind attachment_id = J.required_as fields "attachment_id" Id.Attachment.of_json in
    let%bind intent = J.required fields "intent" in
    if String.equal method_ command_method
    then (
      let%bind intent = Goal_command.of_json intent in
      command ~session ~attachment_id intent)
    else if String.equal method_ start_method
    then (
      let%bind intent = Start.of_json intent in
      start ~session ~attachment_id intent)
    else if String.equal method_ resume_method
    then (
      let%bind intent = Goal_resume_request.of_json intent in
      resume ~session ~attachment_id intent)
    else W.invalid "unknown Goal control method"
  ;;
end

module Callback_control = Goal_control_outcome.Callback_control
module Start_result = Goal_control_outcome.Start_result
module Resume_result = Goal_control_outcome.Resume_result

module Result = struct
  type t =
    | Command of Goal_command_receipt.t
    | Start of Start_result.t
    | Resume of Resume_result.t

  let method_name = function
    | Command _ -> command_method
    | Start _ -> start_method
    | Resume _ -> resume_method
  ;;

  let payload = function
    | Command receipt -> Goal_command_receipt.to_json receipt
    | Start result -> Start_result.to_json result
    | Resume result -> Resume_result.to_json result
  ;;

  let of_method_and_result ~method_ ~result =
    let open Core.Result.Let_syntax in
    let%bind () = Extension_codec.validate_json ~max_bytes:262144 ~max_depth:72 result in
    if String.equal method_ command_method
    then (
      let%map receipt = Goal_control_outcome.command_receipt_of_json result in
      Command receipt)
    else if String.equal method_ start_method
    then (
      let%map result = Start_result.of_json result in
      Start result)
    else if String.equal method_ resume_method
    then (
      let%map result = Resume_result.of_json result in
      Resume result)
    else W.invalid "unknown Goal control result method"
  ;;
end
