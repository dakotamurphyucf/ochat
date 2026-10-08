open! Core
module D = Openai.Responses_driver

let responses_endpoint ~api_url =
  let base =
    Option.value api_url ~default:"https://api.openai.com"
    |> String.chop_suffix_if_exists ~suffix:"/"
  in
  let base =
    if Option.is_some (Uri.scheme (Uri.of_string base)) then base else "https://" ^ base
  in
  base ^ "/v1/responses"
;;

let capabilities =
  D.Capability.create
    ~baseline:
      ([ D.Capability.Text_input
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
       |> List.map ~f:(fun feature -> feature, D.Capability.Supported))
    ~models:[]
  |> Or_error.ok_exn
;;
