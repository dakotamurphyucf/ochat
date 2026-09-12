open Core
open Authoring_evaluation

let () =
  let authorized = ref false in
  let model = ref "" in
  let parameters = ref "" in
  let repetitions = ref 1 in
  let provider_timeout = ref 120. in
  let case_timeout = ref 600. in
  Stdlib.Arg.parse
    [ ( "--authorize-real-model"
      , Stdlib.Arg.Set authorized
      , "Explicitly authorize this paid/provider evaluation" )
    ; "--model", Stdlib.Arg.Set_string model, "Exact provider model identifier (required)"
    ; ( "--settings"
      , Stdlib.Arg.Set_string parameters
      , "JSON model settings; max_output_tokens is required" )
    ; ( "--repetitions"
      , Stdlib.Arg.Set_int repetitions
      , "Paired repetitions per task/condition (default 1; no API seed)" )
    ; ( "--provider-timeout"
      , Stdlib.Arg.Set_float provider_timeout
      , "Provider deadline in seconds" )
    ; ( "--case-timeout"
      , Stdlib.Arg.Set_float case_timeout
      , "Whole-case deadline in seconds" )
    ]
    (fun value -> raise (Stdlib.Arg.Bad ("unexpected argument: " ^ value)))
    "evaluate_live.exe --authorize-real-model --model MODEL --settings JSON > \
     private-results.json";
  (match !authorized with
   | false ->
     failwith "live evaluation requires --authorize-real-model; no provider was started"
   | true -> ());
  (match !repetitions > 0 with
   | true -> ()
   | false -> invalid_arg "repetitions must be positive");
  let model_parameters =
    match Jsonaf.of_string !parameters with
    | value -> value
    | exception _ -> invalid_arg "--settings must contain a JSON object"
  in
  let binary_digest =
    In_channel.read_all Stdlib.Sys.executable_name |> Chatmd_shell_spec.Source_ref.digest
  in
  let config : Driver.config =
    { model = !model
    ; model_parameters
    ; seeds = List.init !repetitions ~f:(fun _ -> None)
    ; provenance = Runner.Real_model
    ; runtime_revision = "evaluation-binary-sha256:" ^ binary_digest
    ; oracle_revision = "evaluation-binary-sha256:" ^ binary_digest
    ; provider_timeout_seconds = !provider_timeout
    ; case_timeout_seconds = !case_timeout
    ; max_transcript_bytes = 16000000
    }
  in
  Responses_provider.validate_config config;
  let api_key = Sys.getenv "OPENAI_API_KEY" |> Option.value ~default:"" in
  let artifact =
    Eio_main.run (fun env ->
      Responses_http.with_transport ~env ~api_key (fun post ->
        let with_backend task f =
          Suite.with_backend
            ~env
            ~runtime_revision:config.runtime_revision
            task
            (fun prepared ->
               f
                 { prepared with
                   backend = Responses_provider.with_protocol prepared.backend
                 })
        in
        Driver.run
          ~env
          ~real_model_authorized:!authorized
          ~config
          ~with_backend
          ~make_provider:(Responses_provider.make_provider ~post)
          ()))
  in
  let report = Report.create artifact |> Result.ok_or_failwith in
  print_endline
    (Jsonaf.to_string
       (`Object
           [ "artifact", Driver.jsonaf_of_artifact artifact
           ; "report", Report.jsonaf_of_t report
           ]))
;;
