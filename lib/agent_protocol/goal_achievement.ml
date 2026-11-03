open! Core
module J = Json_codec

let invalid text = Error (Protocol_error.invalid_request text)

let check_receipt episode (receipt : Run_receipt.t) ~kind =
  let admitted = Goal_episode.admission episode in
  if
    Run_receipt.Kind.equal receipt.kind kind
    && Id.Run.equal receipt.run_id admitted.run_id
    && Id.Principal.equal receipt.principal_id admitted.principal_id
    && Run_source.equal receipt.source admitted.source
    && Int64.(receipt.run_revision > admitted.run_revision)
    && Int64.(receipt.session_revision >= admitted.session_revision)
    && Timestamp.compare receipt.committed_at admitted.committed_at >= 0
  then Ok ()
  else invalid "goal achievement receipt does not match its episode"
;;

module Request = struct
  type t =
    { episode : Goal_episode.t
    ; receipt : Run_receipt.t
    ; evidence : Goal_text.Reason.t
    ; requested_action : Run_action.t
    }
  [@@deriving equal, sexp_of]

  let episode t = t.episode
  let receipt t = t.receipt
  let evidence t = t.evidence

  let requested_result t =
    match t.requested_action with
    | Run_action.Finish { terminal = Completed result; _ } -> result
    | Continue | Wait _
    | Finish { terminal = Failed _ | Cancelled | Limited | Interrupted; _ } -> None
  ;;

  let action t = t.requested_action

  let validate_action receipt action =
    let open Result.Let_syntax in
    let%bind () = Run_action.validate action in
    let digest =
      Run_action.to_json action
      |> Jsonaf.to_string
      |> Digestif.SHA256.digest_string
      |> Digestif.SHA256.to_hex
    in
    match action with
    | Run_action.Finish { terminal = Completed _; _ }
      when String.equal receipt.Run_receipt.request_sha256 digest -> Ok ()
    | Continue | Wait _ | Finish _ ->
      invalid "goal achievement action does not match its receipt"
  ;;

  let create ~episode ~(run : Run.t) ~receipt ~action ~evidence =
    let open Result.Let_syntax in
    let%bind () = Run.validate run in
    let%bind () = validate_action receipt action in
    let%bind () = check_receipt episode receipt ~kind:Action in
    let%bind () =
      match run.lifecycle with
      | Terminal (Failed _ | Cancelled | Limited | Interrupted) ->
        invalid "adverse terminal Run cannot request achievement"
      | Admitted | Active | Waiting _ | Terminal (Completed _) -> Ok ()
    in
    if
      (not (Goal_episode.matches_run episode run))
      || not (Int64.equal receipt.Run_receipt.run_revision run.revision)
    then invalid "achievement request requires its exact committed Run revision"
    else (
      match action with
      | Run_action.Finish { terminal = Completed _; relinquish = _ } ->
        Ok { episode; receipt; evidence; requested_action = action }
      | Continue | Wait _
      | Finish { terminal = Failed _ | Cancelled | Limited | Interrupted; _ } ->
        invalid "achievement requires an explicit Completed Finish request")
  ;;

  let to_json t =
    `Object
      [ "episode", Goal_episode.to_json t.episode
      ; "receipt", Run_receipt.to_json t.receipt
      ; "evidence", Goal_text.Reason.to_json t.evidence
      ; "requested_action", Run_action.to_json t.requested_action
      ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Extension_codec.validate_json ~max_depth:Run_limits.max_depth json in
    let%bind f = J.fields json in
    let%bind episode = J.required_as f "episode" Goal_episode.of_json in
    let%bind receipt = J.required_as f "receipt" Run_receipt.of_json in
    let%bind evidence = J.required_as f "evidence" Goal_text.Reason.of_json in
    let%bind requested_action = J.required_as f "requested_action" Run_action.of_json in
    let%bind () = validate_action receipt requested_action in
    let%map () = check_receipt episode receipt ~kind:Action in
    { episode; receipt; evidence; requested_action }
  ;;
end

type t =
  { request : Request.t
  ; settlement : Goal_episode.Settlement.t
  }
[@@deriving equal, sexp_of]

let request t = t.request
let receipt t = Goal_episode.Settlement.receipt t.settlement
let settlement t = t.settlement

let validate_association request (receipt : Run_receipt.t) =
  let open Result.Let_syntax in
  let%bind () = check_receipt (Request.episode request) receipt ~kind:Terminal in
  let action = Request.receipt request in
  if
    Int64.(receipt.run_revision >= action.run_revision)
    && Int64.(receipt.session_revision >= action.session_revision)
    && Timestamp.compare receipt.committed_at action.committed_at >= 0
  then Ok ()
  else invalid "achievement terminal predates its request"
;;

let validate_completion request settlement =
  let open Result.Let_syntax in
  let%bind () =
    validate_association request (Goal_episode.Settlement.receipt settlement)
  in
  if
    not
      (Goal_episode.equal
         (Request.episode request)
         (Goal_episode.Settlement.episode settlement))
  then invalid "goal completion belongs to another episode"
  else (
    let expected = Run.Terminal.Completed (Request.requested_result request) in
    let digest =
      Run.Terminal.to_json expected
      |> Jsonaf.to_string
      |> Digestif.SHA256.digest_string
      |> Digestif.SHA256.to_hex
    in
    if String.equal (Goal_episode.Settlement.receipt settlement).request_sha256 digest
    then Ok ()
    else invalid "goal achievement requires matching immutable Completed evidence")
;;

let of_evidence request settlement =
  Result.map (validate_completion request settlement) ~f:(fun () ->
    { request; settlement })
;;

let create request ~run ~receipt =
  let open Result.Let_syntax in
  let%bind settlement =
    Goal_episode.Settlement.create (Request.episode request) ~run ~receipt
  in
  of_evidence request settlement
;;

let to_json t =
  `Object
    [ "request", Request.to_json t.request
    ; "settlement", Goal_episode.Settlement.to_json t.settlement
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_json ~max_depth:Run_limits.max_depth json in
  let%bind f = J.fields json in
  let%bind request = J.required_as f "request" Request.of_json in
  let%bind settlement = J.required_as f "settlement" Goal_episode.Settlement.of_json in
  of_evidence request settlement
;;
