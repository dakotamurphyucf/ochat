open! Core
module J = Json_codec

type t =
  { session : Session_ref.t
  ; admission : Run_receipt.t
  }
[@@deriving equal, sexp_of]

let admission t = t.admission
let session t = t.session

let matches_run t (run : Run.t) =
  Id.Run.equal t.admission.run_id run.id
  && Id.Principal.equal t.admission.principal_id run.principal_id
  && Run_source.equal t.admission.source run.source
  && Session_ref.equal t.session run.session
  && Run.Mode.equal run.mode Workflow
  && Int64.(t.admission.run_revision <= run.revision)
;;

let create ~(run : Run.t) ~(admission : Run_receipt.t) =
  let open Result.Let_syntax in
  let%bind () = Run.validate run in
  let t = { session = run.session; admission } in
  if Run_receipt.Kind.equal admission.kind Admission && matches_run t run
  then Ok t
  else
    Error
      (Protocol_error.invalid_request
         "goal episode requires its matching Workflow admission")
;;

let validate_terminal t ~(run : Run.t) ~(receipt : Run_receipt.t) =
  let open Result.Let_syntax in
  let%bind () = Run.validate run in
  let terminal =
    match run.lifecycle with
    | Terminal _ -> true
    | Admitted | Active | Waiting _ -> false
  in
  if
    terminal
    && matches_run t run
    && Run_receipt.Kind.equal receipt.kind Terminal
    && Id.Run.equal receipt.run_id run.id
    && Id.Principal.equal receipt.principal_id run.principal_id
    && Run_source.equal receipt.source run.source
    && Int64.equal receipt.run_revision run.revision
    && Int64.(receipt.run_revision > t.admission.run_revision)
    && Int64.(receipt.session_revision >= t.admission.session_revision)
    && Timestamp.compare receipt.committed_at t.admission.committed_at >= 0
  then Ok ()
  else
    Error (Protocol_error.invalid_request "goal episode terminal evidence does not match")
;;

let to_json t =
  `Object
    [ "session", Session_ref.to_json t.session
    ; "admission", Run_receipt.to_json t.admission
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_json ~max_depth:Run_limits.max_depth json in
  let%bind f = J.fields json in
  let%bind session = J.required_as f "session" Session_ref.of_json in
  let%bind admission = J.required_as f "admission" Run_receipt.of_json in
  if Run_receipt.Kind.equal admission.kind Admission
  then Ok { session; admission }
  else Error (Protocol_error.invalid_request "goal episode receipt must be an admission")
;;

type episode = t [@@deriving equal, sexp_of]

module Settlement = struct
  type t =
    { episode : episode
    ; receipt : Run_receipt.t
    }
  [@@deriving equal, sexp_of]

  let episode t = t.episode
  let receipt t = t.receipt

  let validate episode (receipt : Run_receipt.t) =
    let admitted = admission episode in
    if
      Run_receipt.Kind.equal receipt.kind Terminal
      && Id.Run.equal receipt.run_id admitted.run_id
      && Id.Principal.equal receipt.principal_id admitted.principal_id
      && Run_source.equal receipt.source admitted.source
      && Int64.(receipt.run_revision > admitted.run_revision)
      && Int64.(receipt.session_revision >= admitted.session_revision)
      && Timestamp.compare receipt.committed_at admitted.committed_at >= 0
    then Ok ()
    else
      Error (Protocol_error.invalid_request "goal terminal receipt association conflicts")
  ;;

  let create episode ~run ~receipt =
    let open Result.Let_syntax in
    let%bind () = validate_terminal episode ~run ~receipt in
    match run.Run.lifecycle with
    | Terminal outcome ->
      let digest =
        Run.Terminal.to_json outcome
        |> Jsonaf.to_string
        |> Digestif.SHA256.digest_string
        |> Digestif.SHA256.to_hex
      in
      let%bind () = validate episode receipt in
      if String.equal receipt.Run_receipt.request_sha256 digest
      then Ok { episode; receipt }
      else
        Error
          (Protocol_error.invalid_request
             "goal terminal receipt does not match actual outcome")
    | Admitted | Active | Waiting _ ->
      Error (Protocol_error.invalid_request "goal settlement requires terminal evidence")
  ;;

  let to_json t =
    `Object [ "episode", to_json t.episode; "receipt", Run_receipt.to_json t.receipt ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () = Extension_codec.validate_json ~max_depth:Run_limits.max_depth json in
    let%bind f = J.fields json in
    let%bind episode = J.required_as f "episode" of_json in
    let%bind receipt = J.required_as f "receipt" Run_receipt.of_json in
    let%map () = validate episode receipt in
    { episode; receipt }
  ;;
end
