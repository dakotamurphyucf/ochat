open Core

type t =
  { session_id : Id.Session.t
  ; job_id : Id.Job.t
  ; generation : int
  ; attempt : int
  ; outcome : Stored_completion.outcome
  ; byte_length : int64
  ; sha256 : string
  ; artifact : Job_artifact.t option
  }
[@@deriving sexp]

let invalid message = Error (Protocol_error.invalid_request message)

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    Extension_codec.validate_id Id.Session.to_json Id.Session.of_json t.session_id
  in
  let%bind () = Extension_codec.validate_id Id.Job.to_json Id.Job.of_json t.job_id in
  let%bind () =
    match
      t.generation >= 0
      && t.attempt >= 0
      && Int64.(t.byte_length > zero)
      && String.length t.sha256 = 64
      && String.for_all t.sha256 ~f:(function
        | '0' .. '9' | 'a' .. 'f' -> true
        | _ -> false)
    with
    | true -> Ok ()
    | false -> invalid "invalid retained job result reference"
  in
  match t.artifact with
  | None -> Ok ()
  | Some artifact ->
    let%bind _ = Job_artifact.of_json (Job_artifact.to_json artifact) in
    (match
       Id.Session.equal t.session_id artifact.session_id
       && Id.Job.equal t.job_id artifact.job_id
       && Int.equal t.generation artifact.generation
       && Int.equal t.attempt artifact.attempt
       && Int64.equal t.byte_length artifact.blob.byte_length
       && String.equal t.sha256 artifact.blob.digest
     with
     | true -> Ok ()
     | false -> invalid "retained result reference differs from its artifact")
;;

let outcomes =
  [ "succeeded", Stored_completion.Succeeded
  ; "failed", Failed
  ; "cancelled", Cancelled
  ; "expired", Expired
  ]
;;

let to_json t =
  `Object
    ([ "type", `String "ochat.job_result_reference"
     ; "version", `Number "1"
     ; "session_id", Id.Session.to_json t.session_id
     ; "job_id", Id.Job.to_json t.job_id
     ; "generation", `Number (Int.to_string t.generation)
     ; "attempt", `Number (Int.to_string t.attempt)
     ; ( "outcome"
       , `String
           (fst
              (List.find_exn outcomes ~f:(fun (_, value) ->
                 Stored_completion.equal_outcome value t.outcome))) )
     ; "byte_length", `String (Int64.to_string t.byte_length)
     ; "sha256", `String t.sha256
     ]
     @ Option.to_list
         (Option.map t.artifact ~f:(fun artifact ->
            "artifact", Job_artifact.to_json artifact)))
;;

let equal a b = Jsonaf.exactly_equal (to_json a) (to_json b)

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_bytes:16384 ~max_depth:12 json in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "type"
      ; "version"
      ; "session_id"
      ; "job_id"
      ; "generation"
      ; "attempt"
      ; "outcome"
      ; "byte_length"
      ; "sha256"
      ; "artifact"
      ]
  in
  let get name decode = Json_codec.required_as fields name decode in
  let%bind _ =
    get
      "type"
      (Json_codec.enum ~name:"result reference" [ "ochat.job_result_reference", () ])
  in
  let%bind _ = get "version" (Json_codec.bounded_int ~min:1 ~max:1) in
  let%bind session_id = get "session_id" Id.Session.of_json in
  let%bind job_id = get "job_id" Id.Job.of_json in
  let%bind generation =
    get "generation" (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind attempt = get "attempt" (Json_codec.bounded_int ~min:0 ~max:Int.max_value) in
  let%bind outcome = get "outcome" (Json_codec.enum ~name:"result outcome" outcomes) in
  let%bind byte_length =
    get "byte_length" (fun json ->
      let%bind text = Json_codec.string json in
      match Int64.of_string_opt text with
      | Some value when String.equal text (Int64.to_string value) -> Ok value
      | _ -> invalid "result byte length must be an exact decimal string")
  in
  let%bind sha256 = get "sha256" Json_codec.string in
  let%bind artifact = Json_codec.optional_as fields "artifact" Job_artifact.of_json in
  let t =
    { session_id; job_id; generation; attempt; outcome; byte_length; sha256; artifact }
  in
  let%map () = validate t in
  t
;;

let of_job (job : Job.t) =
  let open Result.Let_syntax in
  let%bind result = Job.terminal_result job in
  let%bind result =
    Result.of_option
      result
      ~error:(Protocol_error.invalid_request "result reference requires terminal work")
  in
  let byte_length, sha256, artifact =
    match result with
    | Stored_completion.Artifact { reference; _ } ->
      reference.blob.byte_length, reference.blob.digest, Some reference
    | Inline completion ->
      let content = Completion.to_json completion |> Jsonaf.to_string in
      ( Int64.of_int (String.length content)
      , Digestif.SHA256.(digest_string content |> to_hex)
      , None )
  in
  let t =
    { session_id = job.session_id
    ; job_id = job.id
    ; generation = job.generation
    ; attempt = job.attempt
    ; outcome = Stored_completion.outcome result
    ; byte_length
    ; sha256
    ; artifact
    }
  in
  let%map () = validate t in
  t
;;

let validate_job t job =
  let open Result.Let_syntax in
  let%bind () = validate t in
  let%bind expected = of_job job in
  match equal t expected with
  | true -> Ok ()
  | false -> invalid "retained result reference differs from its terminal job"
;;
