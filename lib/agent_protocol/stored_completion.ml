open Core

type outcome =
  | Succeeded
  | Failed
  | Cancelled
  | Expired
[@@deriving equal, sexp]

type t =
  | Inline of Completion.t
  | Artifact of
      { outcome : outcome
      ; reference : Job_artifact.t
      }
[@@deriving sexp]

let outcome_of_completion = function
  | Completion.Succeeded _ -> Succeeded
  | Failed _ -> Failed
  | Cancelled _ -> Cancelled
  | Expired -> Expired
;;

let outcome = function
  | Inline completion -> outcome_of_completion completion
  | Artifact artifact -> artifact.outcome
;;

let outcome_to_json = function
  | Succeeded -> `String "succeeded"
  | Failed -> `String "failed"
  | Cancelled -> `String "cancelled"
  | Expired -> `String "expired"
;;

let outcome_of_json =
  Json_codec.enum
    ~name:"stored completion outcome"
    [ "succeeded", Succeeded
    ; "failed", Failed
    ; "cancelled", Cancelled
    ; "expired", Expired
    ]
;;

let to_json = function
  | Inline completion -> Completion.to_json completion
  | Artifact { outcome; reference } ->
    `Object
      [ "type", `String "artifact"
      ; "version", `Number "1"
      ; "outcome", outcome_to_json outcome
      ; "reference", Job_artifact.to_json reference
      ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind fields = Json_codec.fields json in
  match Json_codec.optional fields "type" with
  | Some (`String "artifact") ->
    let%bind () = Json_codec.validate_limits ~max_bytes:16384 ~max_depth:10 json in
    let%bind () =
      Extension_codec.closed fields [ "type"; "version"; "outcome"; "reference" ]
    in
    let%bind version =
      Json_codec.required_as
        fields
        "version"
        (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
    in
    let%bind () =
      match version with
      | 1 -> Ok ()
      | _ ->
        Error
          (Protocol_error.create
             Incompatible_protocol
             ~message:"unsupported stored completion version"
             ~retryable:false
             ())
    in
    let%bind outcome = Json_codec.required_as fields "outcome" outcome_of_json in
    let%map reference = Json_codec.required_as fields "reference" Job_artifact.of_json in
    Artifact { outcome; reference }
  | _ -> Completion.of_json json |> Result.map ~f:(fun completion -> Inline completion)
;;

let matches t completion =
  let open Result.Let_syntax in
  let%bind () = Completion.validate completion in
  match t with
  | Inline expected -> Ok (Completion.equal expected completion)
  | Artifact { outcome; reference } ->
    let%bind _ = Job_artifact.of_json (Job_artifact.to_json reference) in
    let content = Completion.to_json completion |> Jsonaf.to_string in
    Ok
      (equal_outcome outcome (outcome_of_completion completion)
       && Int64.equal reference.blob.byte_length (Int64.of_int (String.length content))
       && String.equal
            reference.blob.digest
            Digestif.SHA256.(digest_string content |> to_hex))
;;

let artifact reference completion =
  let result = Artifact { outcome = outcome_of_completion completion; reference } in
  let open Result.Let_syntax in
  let%bind matches = matches result completion in
  match matches with
  | true -> Ok result
  | false -> Error (Protocol_error.invalid_request "artifact differs from its completion")
;;

let materialize ~load = function
  | Inline completion ->
    Result.map (Completion.validate completion) ~f:(fun () -> completion)
  | Artifact { reference; _ } as stored ->
    let open Result.Let_syntax in
    let%bind completion = load reference in
    let%bind matches = matches stored completion in
    (match matches with
     | true -> Ok completion
     | false ->
       Error
         (Protocol_error.create
            Blob_unavailable
            ~message:"stored completion does not match its artifact reference"
            ~retryable:false
            ()))
;;
