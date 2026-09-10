open Core

type t =
  { job_attempt : int
  ; contract_sha256 : string
  ; result_sha256 : string
  ; rejected : bool
  ; result_reference : Job_result_reference.t option [@sexp.option]
  }
[@@deriving equal, sexp]

let digest json = Digestif.SHA256.(digest_string (Jsonaf.to_string json) |> to_hex)
let contract_digest contract = Completion_contract.to_json contract |> digest
let result_digest result = Stored_completion.to_json result |> digest

let rejection : Invocation.tool_error =
  { code = "background.invalid_completion"
  ; message = "The eventual result does not satisfy the captured completion contract."
  ; retryable = false
  ; details = `Null
  }
;;

let validate t =
  let valid_digest value =
    String.length value = 64
    && String.for_all value ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  in
  match
    t.job_attempt >= 0 && valid_digest t.contract_sha256 && valid_digest t.result_sha256
  with
  | true ->
    (match t.result_reference, t.rejected with
     | None, _ -> Ok ()
     | Some reference, false when Int.equal t.job_attempt reference.attempt ->
       Job_result_reference.validate reference
     | Some _, false ->
       Error
         (Protocol_error.invalid_request "result reference belongs to another attempt")
     | Some _, true ->
       Error
         (Protocol_error.invalid_request
            "rejected completion cannot disclose a result reference"))
  | false -> Error (Protocol_error.invalid_request "invalid completion projection")
;;

let to_json t =
  `Object
    ([ "version", `Number (if Option.is_some t.result_reference then "2" else "1")
     ; "job_attempt", `Number (Int.to_string t.job_attempt)
     ; "contract_sha256", `String t.contract_sha256
     ; "result_sha256", `String t.result_sha256
     ; ("rejected", if t.rejected then `True else `False)
     ]
     @ Option.to_list
         (Option.map t.result_reference ~f:(fun reference ->
            "result_reference", Job_result_reference.to_json reference)))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_bytes:17408 ~max_depth:13 json in
  let%bind fields = Json_codec.fields json in
  let%bind version =
    Json_codec.required_as fields "version" (Json_codec.bounded_int ~min:1 ~max:2)
  in
  let%bind () =
    Extension_codec.closed
      fields
      ([ "version"; "job_attempt"; "contract_sha256"; "result_sha256"; "rejected" ]
       @ if version = 2 then [ "result_reference" ] else [])
  in
  let get name decode = Json_codec.required_as fields name decode in
  let%bind job_attempt =
    get "job_attempt" (Json_codec.bounded_int ~min:0 ~max:Int.max_value)
  in
  let%bind contract_sha256 = get "contract_sha256" Json_codec.string in
  let%bind result_sha256 = get "result_sha256" Json_codec.string in
  let%bind rejected = get "rejected" Json_codec.bool in
  let%bind result_reference =
    match version with
    | 2 ->
      get "result_reference" Job_result_reference.of_json |> Result.map ~f:Option.some
    | _ -> Ok None
  in
  let t = { job_attempt; contract_sha256; result_sha256; rejected; result_reference } in
  let%map () = validate t in
  t
;;
