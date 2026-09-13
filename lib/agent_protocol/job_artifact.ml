open Core

type t =
  { session_id : Id.Session.t
  ; job_id : Id.Job.t
  ; generation : int
  ; attempt : int
  ; blob : Blob.Metadata.t
  }
[@@deriving sexp]

let media_type = "application/vnd.ochat.completion+json"

let create ~session_id ~job_id ~generation ~attempt ~blob =
  let open Result.Let_syntax in
  let%bind blob = Blob.Metadata.of_json (Blob.Metadata.to_json blob) in
  match
    generation >= 0
    && attempt > 0
    && Blob.equal_kind blob.kind File
    && String.equal blob.media_type media_type
    && Int64.(blob.byte_length > zero)
  with
  | true -> Ok { session_id; job_id; generation; attempt; blob }
  | false -> Error (Protocol_error.invalid_request "invalid job result artifact")
;;

let to_json t =
  `Object
    [ "version", `Number "1"
    ; "session_id", Id.Session.to_json t.session_id
    ; "job_id", Id.Job.to_json t.job_id
    ; "generation", `Number (Int.to_string t.generation)
    ; "attempt", `Number (Int.to_string t.attempt)
    ; "blob", Blob.Metadata.to_json t.blob
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_bytes:8192 ~max_depth:8 json in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "version"; "session_id"; "job_id"; "generation"; "attempt"; "blob" ]
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
           ~message:"unsupported job artifact version"
           ~retryable:false
           ())
  in
  let%bind session_id = Json_codec.required_as fields "session_id" Id.Session.of_json in
  let%bind job_id = Json_codec.required_as fields "job_id" Id.Job.of_json in
  let%bind generation =
    Json_codec.required_as
      fields
      "generation"
      (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind attempt =
    Json_codec.required_as
      fields
      "attempt"
      (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  let%bind blob = Json_codec.required_as fields "blob" Blob.Metadata.of_json in
  create ~session_id ~job_id ~generation ~attempt ~blob
;;

let allowed_use t =
  String.concat
    ~sep:":"
    [ "job_result"
    ; Id.Session.to_string t.session_id
    ; Id.Job.to_string t.job_id
    ; Int.to_string t.generation
    ; Int.to_string t.attempt
    ]
;;
