open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Origin = Recurrence_run_origin

module Outcome = struct
  type t =
    | Completed
    | Failed of P.Error.code option
    | Cancelled
    | Limited
    | Interrupted
  [@@deriving equal, sexp_of]

  let of_terminal = function
    | P.Run.Terminal.Completed _ -> Completed
    | Failed code -> Failed code
    | Cancelled -> Cancelled
    | Limited -> Limited
    | Interrupted -> Interrupted
  ;;

  let to_json = function
    | Completed -> `Object [ "kind", `String "completed" ]
    | Failed code ->
      `Object
        [ "kind", `String "failed"
        ; "code", X.option_json (fun code -> `String (P.Error.code_to_string code)) code
        ]
    | Cancelled -> `Object [ "kind", `String "cancelled" ]
    | Limited -> `Object [ "kind", `String "limited" ]
    | Interrupted -> `Object [ "kind", `String "interrupted" ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind fields = X.object_ json in
    let%bind kind = X.required fields "kind" P.Json_codec.string in
    match kind with
    | "completed" -> Ok Completed
    | "failed" ->
      let%map code =
        X.required
          fields
          "code"
          (X.nullable (fun json ->
             Result.bind (P.Json_codec.string json) ~f:P.Error.code_of_string))
      in
      Failed code
    | "cancelled" -> Ok Cancelled
    | "limited" -> Ok Limited
    | "interrupted" -> Ok Interrupted
    | _ -> Error (P.Error.invalid_request "unsupported recurrence terminal outcome")
  ;;
end

type projection =
  { origin_sha256 : string
  ; receipt : P.Run_receipt.t
  ; outcome : Outcome.t
  }

type t =
  { projection : projection
  ; json : Jsonaf.t
  }

let origin_sha256 t = t.projection.origin_sha256
let receipt t = t.projection.receipt
let run_id t = (receipt t).run_id
let outcome t = t.projection.outcome
let equal a b = D.Json.equal a.json b.json
let to_json t = t.json
let invalid message = Error (P.Error.invalid_request message)

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create ~max_bytes:8192 ~max_depth:24 ~max_fields:128 ~max_nodes:256
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let terminal_digest terminal =
  (* Identical to Run_transition's terminal receipt and Run_retirement's
     Interrupted receipt producer: protocol field order, not sorted JSON. *)
  P.Run.Terminal.to_json terminal
  |> Jsonaf.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let decode json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind origin_sha256 = X.required fields "origin_sha256" P.Json_codec.string in
  let%bind receipt = X.required fields "receipt" P.Run_receipt.of_json in
  let%bind outcome = X.required fields "outcome" Outcome.of_json in
  if
    String.length origin_sha256 = 64
    && String.for_all origin_sha256 ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
    && P.Run_receipt.Kind.equal receipt.kind Terminal
  then Ok { origin_sha256; receipt; outcome }
  else invalid "invalid recurrence terminal reference identity or receipt kind"
;;

let encode p =
  `Object
    [ "origin_sha256", `String p.origin_sha256
    ; "receipt", P.Run_receipt.to_json p.receipt
    ; "outcome", Outcome.to_json p.outcome
    ]
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.recurrence_run_reference"
    ~shape:(X.fields_shape [ "origin_sha256"; "receipt"; "outcome" ])
    ~decode
    ~encode:(fun value -> Ok (encode value))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect ~limits json |> protocol in
  let%map carrier = D.Domain_codec.decode codec document |> protocol in
  { projection = D.Extension_carrier.value carrier; json }
;;

let validate_origin t ~origin =
  let admission = Origin.admission origin in
  let terminal_receipt = receipt t in
  if
    String.equal (origin_sha256 t) (Origin.digest origin)
    && P.Run_receipt.Kind.equal terminal_receipt.kind Terminal
    && P.Id.Run.equal terminal_receipt.run_id admission.run_id
    && P.Id.Principal.equal terminal_receipt.principal_id admission.principal_id
    && P.Run_source.equal terminal_receipt.source admission.source
    && Int64.(terminal_receipt.run_revision > admission.run_revision)
    && Int64.(terminal_receipt.session_revision >= admission.session_revision)
  then Ok ()
  else invalid "terminal reference differs from its retained origin"
;;

let validate t ~origin ~(run : P.Run.t) ~receipt:actual_receipt =
  let open Result.Let_syntax in
  let%bind () = validate_origin t ~origin in
  let%bind () = Origin.validate_run origin ~run ~admission:(Origin.admission origin) in
  let terminal_receipt = receipt t in
  match run.lifecycle with
  | Admitted | Active | Waiting _ -> invalid "recurrence Run is not actually terminal"
  | Terminal terminal ->
    if
      P.Run_receipt.equal terminal_receipt actual_receipt
      && P.Id.Run.equal terminal_receipt.run_id run.id
      && P.Id.Principal.equal terminal_receipt.principal_id run.principal_id
      && P.Run_source.equal terminal_receipt.source run.source
      && Int64.equal terminal_receipt.run_revision run.revision
      && Outcome.equal (outcome t) (Outcome.of_terminal terminal)
      && String.equal terminal_receipt.request_sha256 (terminal_digest terminal)
    then Ok ()
    else invalid "terminal reference differs from the actual canonical Run outcome"
;;

let create ~origin ~(run : P.Run.t) ~receipt =
  let open Result.Let_syntax in
  match run.lifecycle with
  | Admitted | Active | Waiting _ -> invalid "recurrence Run is not actually terminal"
  | Terminal terminal ->
    let projection =
      { origin_sha256 = Origin.digest origin
      ; receipt
      ; outcome = Outcome.of_terminal terminal
      }
    in
    let%bind document =
      D.Domain_codec.encode codec (D.Extension_carrier.of_authored_value projection)
      |> protocol
    in
    let%bind t = of_json (D.Document.json document) in
    let%map () = validate t ~origin ~run ~receipt in
    t
;;
