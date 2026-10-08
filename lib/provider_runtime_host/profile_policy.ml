open! Core
module Driver = Openai.Responses_driver
module D = Driver

type route =
  | Public_api
  | Direct_codex
[@@deriving equal, sexp_of]

module Transport_policy = struct
  let of_string = function
    | "sse" -> Ok Inference.Observation.Transport_policy.Http_sse
    | "prefer-websocket" -> Ok Inference.Observation.Transport_policy.Prefer_websocket
    | "require-websocket" -> Ok Inference.Observation.Transport_policy.Require_websocket
    | _ ->
      Or_error.error_string
        "inference transport must be sse, prefer-websocket or require-websocket"
  ;;

  let to_string = function
    | Inference.Observation.Transport_policy.Http_sse -> "sse"
    | Prefer_websocket -> "prefer-websocket"
    | Require_websocket -> "require-websocket"
  ;;
end

let endpoint = function
  | Public_api -> "https://api.openai.com/v1/responses"
  | Direct_codex -> "https://chatgpt.com/backend-api/codex/responses"
;;

let apply_endpoint_policy profile ~route =
  Driver.Profile.with_truncation_emission
    profile
    (match route with
     | Public_api -> Openai.Responses_request.Truncation_emission.Explicit_disabled
     | Direct_codex -> Omit)
  |> fun profile ->
  Driver.Profile.with_response_content_type_policy
    profile
    (match route with
     | Public_api -> Driver.Profile.Response_content_type_policy.Require_event_stream
     | Direct_codex -> Allow_absent_event_stream)
;;

let baseline route =
  let features =
    match route with
    | Public_api ->
      [ D.Capability.Text_input
      ; Image_input
      ; Document_input
      ; Function_tools
      ; Custom_tools
      ; Opaque_replay
      ]
      @ List.map
          [ "instructions"
          ; "max_output_tokens"
          ; "parallel_tool_calls"
          ; "temperature"
          ; "top_p"
          ; "reasoning"
          ; "text"
          ; "tool_choice"
          ; "prompt_cache_key"
          ; "prompt_cache_retention"
          ; "prompt_cache_options"
          ]
          ~f:(fun name -> D.Capability.Setting name)
      |> List.map ~f:(fun feature -> feature, D.Capability.Supported)
    | Direct_codex ->
      List.map
        [ D.Capability.Text_input
        ; Image_input
        ; Function_tools
        ; Opaque_replay
        ; Setting "instructions"
        ; Setting "parallel_tool_calls"
        ; Setting "reasoning"
        ; Setting "text"
        ; Setting "tool_choice"
        ; Setting "prompt_cache_key"
        ]
        ~f:(fun feature -> feature, D.Capability.Supported)
      @ List.map [ "temperature"; "top_p"; "max_output_tokens" ] ~f:(fun setting ->
        D.Capability.Setting setting, D.Capability.Unsupported)
  in
  (D.Capability.Websocket, D.Capability.Unknown) :: features
;;

(* Exact Public API SSE/WS and Direct Codex device-login WS journeys qualified
   on 2026-10-08, Darwin arm64. Direct evidence is four attempts, three completed
   turns, one local tool effect and restored history; this does not qualify every
   feature/account or warm-channel renewal. See dated provider operator evidence. *)
let qualified_models = function
  | Public_api -> [ "gpt-6-luna", [ D.Capability.Websocket, D.Capability.Supported ] ]
  | Direct_codex -> [ "gpt-6-luna", [ D.Capability.Websocket, D.Capability.Supported ] ]
;;

let capabilities route ~endpoint:selected_endpoint =
  let models =
    if String.equal selected_endpoint (endpoint route) then qualified_models route else []
  in
  D.Capability.create ~baseline:(baseline route) ~models
;;
