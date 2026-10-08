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

(* This catalog is deliberately empty until exact route/model live qualification.
   Candidate declarations belong only to the trusted qualification harness. *)
let qualified_models = function
  | Public_api | Direct_codex -> []
;;

let capabilities route ~endpoint:selected_endpoint =
  let models =
    if String.equal selected_endpoint (endpoint route) then qualified_models route else []
  in
  D.Capability.create ~baseline:(baseline route) ~models
;;
