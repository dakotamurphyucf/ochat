open Core
open Runner
module D = Driver

let action_schema =
  Jsonaf.of_string
    {|{"type":"object","properties":{"operation":{"type":"string","enum":["retrieve","submit","decline"]},"payload":{"type":"string"}},"required":["operation","payload"],"additionalProperties":false}|}
;;

let protocol =
  "Authoring evaluation protocol v1: Return exactly one action using the supplied \
   response schema. retrieve: payload is a JSON-encoded ochat_authoring_context request, \
   using the supplied strict parameters. submit: payload is a JSON-encoded candidate \
   envelope. For one-off scripts and standalone tools use the native ochat_validate \
   request (version 1, target one_off_script or standalone_tool, source and tools; \
   standalone also includes input_schema and output_schema). For moderator and child \
   tasks use the envelope specified in the task. The host validates submissions without \
   effects and runs valid submissions against the task checks, then returns feedback. \
   You may retrieve documentation and repair failures. decline: payload explains why you \
   cannot finish. Do not supply scores or attempt to execute host tools directly. \
   Earlier actions and host feedback appear as serialized transcript entries."
;;

(* Keep protocol text in the measured history, rather than adding uncounted
   instructions only in the transport. Structured-output schema/framing costs
   remain provider-specific and are reflected in actual usage when available. *)
let with_protocol (backend : backend) =
  { backend with
    tool_descriptions =
      { backend.tool_descriptions with
        text = protocol ^ "\n" ^ backend.tool_descriptions.text
      }
  }
;;

let invalid message = raise (Infrastructure_failure message)

let object_fields = function
  | `Object fields
    when not (List.contains_dup (List.map fields ~f:fst) ~compare:String.compare) ->
    fields
  | _ -> invalid "provider returned a malformed object"
;;

let validate_config (config : D.config) =
  D.validate config |> Result.ok_or_failwith;
  (* Responses has no seed parameter in this adapter. Repetition indexes still
     permit paired repeated runs. Never record a seed as if it was applied. *)
  (match List.exists config.seeds ~f:Option.is_some with
   | true -> invalid_arg "Responses evaluations require null seeds; use repetitions"
   | false -> ());
  let fields = object_fields config.model_parameters in
  let positive_integer = function
    | `Number value ->
      (match Int.of_string_opt value with
       | Some n -> n > 0
       | None -> false)
    | _ -> false
  in
  let probability ~upper = function
    | `Number value ->
      (match Float.of_string_opt value with
       | Some n -> Float.is_finite n && Float.(n >= 0. && n <= upper)
       | None -> false)
    | _ -> false
  in
  List.iter fields ~f:(fun (name, value) ->
    let valid =
      match name, value with
      | "max_output_tokens", value -> positive_integer value
      | "temperature", value -> probability ~upper:2. value
      | "top_p", value -> probability ~upper:1. value
      | "reasoning", `Object [ ("effort", `String effort) ] ->
        List.mem
          [ "none"; "minimal"; "low"; "medium"; "high"; "xhigh" ]
          effort
          ~equal:String.equal
      | _ -> false
    in
    match valid with
    | true -> ()
    | false -> invalid_arg ("unsupported Responses evaluation parameter: " ^ name));
  match List.Assoc.mem fields "max_output_tokens" ~equal:String.equal with
  | true -> ()
  | false -> invalid_arg "Responses evaluation requires explicit max_output_tokens"
;;

let request ~config ~messages =
  validate_config config;
  let input =
    List.map messages ~f:(fun message ->
      let role =
        match message.category with
        | Primer | Tool_descriptions -> "developer"
        | Documentation | Conversation -> "user"
      in
      `Object [ "role", `String role; "content", `String message.text ])
  in
  `Object
    (object_fields config.model_parameters
     @ [ "model", `String config.model
       ; "input", `Array input
       ; "store", `False
       ; "stream", `False
       ; "tools", `Array []
       ; "truncation", `String "disabled"
       ; ( "text"
         , `Object
             [ ( "format"
               , `Object
                   [ "type", `String "json_schema"
                   ; "name", `String "ochat_authoring_action_v1"
                   ; "strict", `True
                   ; "schema", action_schema
                   ] )
             ] )
       ])
;;

let decode_action text =
  let parsed =
    match Jsonaf.of_string text with
    | value -> value
    | exception _ -> invalid "provider action is not JSON"
  in
  let fields = object_fields parsed in
  match
    ( List.length fields
    , List.Assoc.find fields "operation" ~equal:String.equal
    , List.Assoc.find fields "payload" ~equal:String.equal )
  with
  | 2, Some (`String operation), Some (`String payload) ->
    (match operation with
     | "decline" -> Decline payload
     | "retrieve" | "submit" ->
       let value =
         match Jsonaf.of_string payload with
         | `Object _ as value ->
           ignore (object_fields value : (string * Jsonaf.t) list);
           value
         | _ -> invalid "provider action payload must be a JSON object"
         | exception (Infrastructure_failure _ as exn) -> raise exn
         | exception _ -> invalid "provider action payload is not JSON"
       in
       (match operation with
        | "retrieve" -> Retrieve value
        | _ -> Submit value)
     | _ -> invalid "unknown provider action operation")
  | _ -> invalid "provider action does not match the strict action schema"
;;

let decode_response body =
  let response =
    match Jsonaf.of_string body with
    | value -> value
    | exception _ -> invalid "provider response is not JSON"
  in
  ignore (object_fields response : (string * Jsonaf.t) list);
  (match Jsonaf.member "status" response, Jsonaf.member "error" response with
   | Some (`String "completed"), (None | Some `Null) -> ()
   | _ -> invalid "provider response did not complete successfully");
  let outputs =
    match Jsonaf.member "output" response with
    | Some (`Array outputs) -> outputs
    | _ -> invalid "provider response has no output array"
  in
  let texts =
    List.concat_map outputs ~f:(fun output ->
      ignore (object_fields output : (string * Jsonaf.t) list);
      match Jsonaf.member "type" output with
      | Some (`String "reasoning") -> []
      | Some (`String "message") ->
        (match
           ( Jsonaf.member "role" output
           , Jsonaf.member "status" output
           , Jsonaf.member "content" output )
         with
         | Some (`String "assistant"), Some (`String "completed"), Some (`Array contents)
           ->
           List.map contents ~f:(fun content ->
             ignore (object_fields content : (string * Jsonaf.t) list);
             match Jsonaf.member "type" content, Jsonaf.member "text" content with
             | Some (`String "output_text"), Some (`String text) -> text
             | _ -> invalid "provider refused or returned unsupported content")
         | _ -> invalid "provider output message is incomplete or malformed")
      | _ -> invalid "provider returned an unexpected executable output")
  in
  let action =
    match texts with
    | [ text ] -> decode_action text
    | _ -> invalid "provider must return exactly one complete action"
  in
  let provider_input_tokens =
    match Jsonaf.member "usage" response with
    | None | Some `Null -> None
    | Some usage ->
      ignore (object_fields usage : (string * Jsonaf.t) list);
      (match Jsonaf.member "input_tokens" usage with
       | Some (`Number value) ->
         (match Int.of_string_opt value with
          | Some n when n >= 0 -> Some n
          | _ -> invalid "invalid provider input token usage")
       | _ -> invalid "invalid provider input token usage")
  in
  { action; provider_input_tokens }
;;

let max_response_bytes = 2 * 1024 * 1024

let read_response ~status flow =
  match status with
  | 200 ->
    let body =
      match Eio.Buf_read.(parse_exn take_all) flow ~max_size:(max_response_bytes + 1) with
      | body when String.length body <= max_response_bytes -> body
      | _ -> invalid "provider response body limit exceeded"
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception _ -> invalid "provider response body could not be read within its limit"
    in
    decode_response body
  | status -> invalid (sprintf "provider HTTP status %d" status)
;;

let make_provider ~post ~config ~task:_ ~policy:_ ~repetition:_ ~seed:_ ~step:_ ~messages =
  let request = request ~config ~messages in
  match post request with
  | answer -> answer
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception (Infrastructure_failure _ as exn) -> raise exn
  | exception Eio.Time.Timeout -> invalid "provider transport deadline exceeded"
  | exception _ -> invalid "provider transport failed"
;;
