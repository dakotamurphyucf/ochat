open! Core
module Q = Live_qualification

let numeric_setting name raw =
  let value =
    match raw with
    | None -> Openai.Responses_codec.Request.Field.Absent
    | Some "null" -> Null
    | Some number -> Value (`Number number)
  in
  Openai.Responses_driver.Setting.create ~name ~value ~provenance:Profile_default
  |> Or_error.ok_exn
;;

let command =
  Command.basic
    ~summary:"Bounded provider qualification; offline self-checks run in normal tests"
    (let%map_open.Command live =
       flag "live" no_arg ~doc:" explicitly enable live provider requests"
     and self_check =
       flag
         "self-check"
         no_arg
         ~doc:" run pure checks without opening credentials or network"
     and root =
       flag "root" (optional string) ~doc:"PATH private persistent qualification root"
     and output =
       flag "output" (optional string) ~doc:"PATH allowlisted evidence artifact"
     and model = flag "model" (optional string) ~doc:"MODEL exact declared model"
     and alias =
       flag
         "account-alias"
         (optional_with_default "qualification" string)
         ~doc:"LABEL nonsecret account alias"
     and auth =
       flag
         "auth"
         (optional_with_default "api" string)
         ~doc:"api|browser|device explicit authentication route"
     and transport =
       flag
         "transport"
         (optional_with_default "sse" string)
         ~doc:"sse|require-websocket separate transport trial"
     and phase =
       flag
         "phase"
         (optional_with_default "journey" string)
         ~doc:
           "journey|enrolled-journey|renew|logout|feature|rejection-probe explicit \
            qualification phase"
     and feature =
       flag
         "feature"
         (optional string)
         ~doc:
           "json-schema|image|reasoning|document|function-call isolated fixed probe \
            (feature phase only)"
     and key_file =
       flag
         "key-file"
         (optional string)
         ~doc:"PATH private API-key input file; never an environment fallback"
     and key_env =
       flag
         "key-env"
         (optional string)
         ~doc:
           "NAME explicit local environment input read only after enrollment \
            authorization; no fallback"
     and max_attempts =
       flag
         "max-attempts"
         (optional int)
         ~doc:"N persistent actual-attempt ceiling, at most 16"
     and probe_id =
       flag
         "probe-id"
         (optional string)
         ~doc:
           "ID fresh lowercase probe admission identifier; required only for \
            rejection-probe"
     and resume_enrolled_journey =
       flag
         "resume-enrolled-journey"
         no_arg
         ~doc:
           " explicitly reuse the exact admitted enrolled journey only before any \
            inference/effect"
     and enrolled_session =
       flag
         "enrolled-session"
         no_arg
         ~doc:" explicitly select the enrolled-journey checkpoint for renew/logout"
     and resume_enrolled =
       flag
         "resume-enrolled"
         no_arg
         ~doc:
           " explicitly reconcile original completed OAuth enrollment before the first \
            journey session"
     and open_browser =
       flag
         "open-browser"
         no_arg
         ~doc:
           " explicitly launch the fixed macOS browser opener for browser login only; \
            never print its private URI"
     and advance_expiry =
       flag
         "advance-expiry"
         no_arg
         ~doc:
           " explicit controlled host expiry for renew only; real OAuth \
            validation/network clocks retained; exclusive with hold-until-expiry"
     and hold_until_expiry =
       flag
         "hold-until-expiry"
         no_arg
         ~doc:" warm actual channel and wait only within phase bound for real expiry"
     and max_output_tokens =
       flag
         "max-output-tokens"
         (optional string)
         ~doc:"N|null explicit 1..4096 output cap, omitted by default"
     and temperature =
       flag
         "temperature"
         (optional string)
         ~doc:"N|null explicit 0..2 control, omitted by default"
     and top_p =
       flag
         "top-p"
         (optional string)
         ~doc:"N|null explicit 0..1 control, omitted by default"
     and timeout =
       flag
         "phase-seconds"
         (optional_with_default 300 int)
         ~doc:"SECONDS finite phase deadline, at most 900"
     in
     fun () ->
       if self_check
       then (
         if live then failwith "choose self-check or live";
         Q.self_check ())
       else (
         if not live then failwith "live qualification requires explicit --live";
         let required = function
           | Some value -> value
           | None -> failwith "missing required qualification argument"
         in
         let auth =
           match auth with
           | "api" -> Q.Plan.Api
           | "browser" -> Browser
           | "device" -> Device
           | _ -> failwith "invalid auth route"
         in
         let transport =
           match
             Provider_runtime_host.Profile_policy.Transport_policy.of_string transport
             |> Or_error.ok_exn
           with
           | Inference.Observation.Transport_policy.Http_sse -> Q.Plan.Sse
           | Require_websocket -> Require_websocket
           | Prefer_websocket ->
             failwith "qualification requires an explicit transport without fallback"
         in
         let phase =
           match phase with
           | "journey" -> Q.Plan.Journey
           | "enrolled-journey" -> Enrolled_journey
           | "renew" -> Renew
           | "logout" -> Logout
           | "rejection-probe" -> Rejection_probe
           | "feature" -> Feature
           | _ -> failwith "invalid phase"
         in
         let feature =
           Option.map feature ~f:(function
             | "json-schema" -> Feature_case.Case.Json_schema
             | "image" -> Image
             | "reasoning" -> Reasoning
             | "document" -> Document
             | "function-call" -> Function_call
             | _ -> failwith "invalid fixed feature case")
         in
         if not (Bool.equal (Q.Plan.equal_phase phase Feature) (Option.is_some feature))
         then failwith "feature requires phase feature and a selected fixed case";
         let max_attempts =
           Option.value max_attempts ~default:(if Option.is_some feature then 1 else 12)
         in
         let root = required root
         and output = required output
         and model = required model in
         let key_input =
           match key_file, key_env with
           | None, None -> None
           | Some file, None ->
             Some
               (Q.Key_input.private_file file
                |> Result.map_error ~f:Error.of_string
                |> Or_error.ok_exn)
           | None, Some name ->
             Some
               (Q.Key_input.environment name
                |> Result.map_error ~f:Error.of_string
                |> Or_error.ok_exn)
           | Some _, Some _ -> failwith "key-file and key-env are mutually exclusive"
         in
         if
           Q.Plan.equal_auth auth Api
           && (Q.Plan.equal_phase phase Journey || Q.Plan.equal_phase phase Feature)
           && Option.is_none key_input
         then failwith "API enrollment requires explicit key-file or key-env";
         let plan =
           Q.Plan.create
             ?feature
             ~auth
             ~transport
             ~model
             ~account_alias:alias
             ~max_attempts
             ~maximum_phase:(Time_ns.Span.of_sec (Float.of_int timeout))
             ~settings:
               [ numeric_setting "max_output_tokens" max_output_tokens
               ; numeric_setting "temperature" temperature
               ; numeric_setting "top_p" top_p
               ]
             ()
           |> Result.map_error ~f:Error.of_string
           |> Or_error.ok_exn
         in
         Eio_main.run (fun env ->
           Mirage_crypto_rng_unix.use_default ();
           let evidence =
             Q.run
               ~env
               ~plan
               ~phase
               ~root
               ~key_input
               ~hold_until_expiry
               ~advance_expiry
               ~resume_enrolled
               ~probe_id
               ~enrolled_session
               ~resume_enrolled_journey
               ~browser_presentation:
                 (if open_browser
                  then Q.Browser_presentation.Launch_local
                  else Private_terminal)
           in
           Q.Evidence.write evidence Eio.Path.(Eio.Stdenv.fs env / output))))
;;

let () = Command_unix.run command
