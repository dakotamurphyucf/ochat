open Core
module P = Agent_protocol
module L = Chatml.Chatml_lang
module V = Chatml.Chatml_value_codec

type t =
  { session_id : P.Id.Session.t
  ; job_id : P.Id.Job.t
  ; generation : int
  ; attempt : int
  ; source : P.Invocation.observer
  ; completed_at : P.Timestamp.t
  ; result : P.Stored_completion.t
  }

let tag = "__Ochat_background_delivery_v1"
let protocol result = Result.map_error result ~f:(fun error -> error.P.Error.message)

let json t =
  `Object
    [ "session_id", P.Id.Session.to_json t.session_id
    ; "job_id", P.Id.Job.to_json t.job_id
    ; "generation", `String (Int.to_string t.generation)
    ; "attempt", `String (Int.to_string t.attempt)
    ; "script_id", `String t.source.script_id
    ; "source_sha256", `String t.source.source_sha256
    ; "completed_at", P.Timestamp.to_json t.completed_at
    ; "result", P.Stored_completion.to_json t.result
    ]
;;

let equal a b = Jsonaf.exactly_equal (json a) (json b)

let of_json value =
  let open Result.Let_syntax in
  let%bind () =
    P.Json_codec.validate_limits
      ~max_depth:132
      ~max_bytes:((16 * 1024 * 1024) + 8192)
      value
    |> protocol
  in
  let%bind fields = P.Json_codec.fields value |> protocol in
  let%bind () =
    match List.length (P.Json_codec.to_alist fields) with
    | 8 -> Ok ()
    | _ -> Error "invalid background delivery fields"
  in
  let get key decode = P.Json_codec.required_as fields key decode |> protocol in
  let integer json =
    let%bind text = P.Json_codec.string json in
    match Int.of_string_opt text with
    | Some value when value >= 0 && String.equal text (Int.to_string value) -> Ok value
    | _ -> Error (P.Error.invalid_request "invalid background delivery integer")
  in
  let%bind session_id = get "session_id" P.Id.Session.of_json in
  let%bind job_id = get "job_id" P.Id.Job.of_json in
  let%bind generation = get "generation" integer in
  let%bind attempt = get "attempt" integer in
  let%bind script_id = get "script_id" P.Json_codec.string in
  let%bind source_sha256 = get "source_sha256" P.Json_codec.string in
  let%bind () =
    match
      String.is_empty script_id
      || String.length source_sha256 <> 64
      || not
           (String.for_all source_sha256 ~f:(function
              | '0' .. '9' | 'a' .. 'f' -> true
              | _ -> false))
    with
    | true -> Error "invalid background delivery source"
    | false -> Ok ()
  in
  let%bind completed_at = get "completed_at" P.Timestamp.of_json in
  let%map result = get "result" P.Stored_completion.of_json in
  { session_id
  ; job_id
  ; generation
  ; attempt
  ; source = { script_id; source_sha256 }
  ; completed_at
  ; result
  }
;;

let create ~source (job : P.Job.t) =
  let open Result.Let_syntax in
  let%bind () =
    match job.kind, job.launch with
    | Async_tool, Some _ -> Ok ()
    | _ -> Error "background delivery requires a generic owned job"
  in
  let%bind result = P.Job.terminal_result job |> protocol in
  match result, job.completed_at with
  | Some result, Some completed_at ->
    of_json
      (json
         { session_id = job.session_id
         ; job_id = job.id
         ; generation = job.generation
         ; attempt = job.attempt
         ; source
         ; completed_at
         ; result
         })
  | _ -> Error "background delivery requires a retained terminal result"
;;

let capture t = L.VVariant (tag, [ L.VString (Jsonaf.to_string (json t)) ])

let decode = function
  | L.VVariant (name, args) when String.equal name tag ->
    let open Result.Let_syntax in
    (match args with
     | [ L.VString encoded ] ->
       let%bind json =
         Chatmd_shell_spec.Tool_schema.parse_json encoded
         |> Result.map_error ~f:(fun _ -> "invalid background delivery JSON")
       in
       let%map frame = of_json json in
       Some frame
     | _ -> Error "invalid background delivery envelope")
  | _ -> Ok None
;;

let script_event value =
  let open Result.Let_syntax in
  let%map frame = decode value in
  match frame with
  | None -> value
  | Some frame ->
    L.VVariant
      ( "Internal_event"
      , [ V.jsonaf_to_value
            (`Object
                [ "kind", `String "background_job_completed"
                ; "job_id", P.Id.Job.to_json frame.job_id
                ; "attempt", `String (Int.to_string frame.attempt)
                ; "result", P.Stored_completion.to_json frame.result
                ])
        ] )
;;
