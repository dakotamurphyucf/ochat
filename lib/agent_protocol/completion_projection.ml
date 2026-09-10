open Core

type t =
  { job_attempt : int
  ; contract_sha256 : string
  ; result_sha256 : string
  ; rejected : bool
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
    t.job_attempt > 0 && valid_digest t.contract_sha256 && valid_digest t.result_sha256
  with
  | true -> Ok ()
  | false -> Error (Protocol_error.invalid_request "invalid completion projection")
;;

let to_json t =
  `Object
    [ "version", `Number "1"
    ; "job_attempt", `Number (Int.to_string t.job_attempt)
    ; "contract_sha256", `String t.contract_sha256
    ; "result_sha256", `String t.result_sha256
    ; ("rejected", if t.rejected then `True else `False)
    ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Json_codec.validate_limits ~max_bytes:1024 ~max_depth:2 json in
  let%bind fields = Json_codec.fields json in
  let%bind () =
    Extension_codec.closed
      fields
      [ "version"; "job_attempt"; "contract_sha256"; "result_sha256"; "rejected" ]
  in
  let get name decode = Json_codec.required_as fields name decode in
  let%bind _ = get "version" (Json_codec.bounded_int ~min:1 ~max:1) in
  let%bind job_attempt =
    get "job_attempt" (Json_codec.bounded_int ~min:1 ~max:Int.max_value)
  in
  let%bind contract_sha256 = get "contract_sha256" Json_codec.string in
  let%bind result_sha256 = get "result_sha256" Json_codec.string in
  let%bind rejected = get "rejected" Json_codec.bool in
  let t = { job_attempt; contract_sha256; result_sha256; rejected } in
  let%map () = validate t in
  t
;;
