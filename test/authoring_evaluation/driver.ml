open Core
open Jsonaf.Export
open Runner

type config =
  { model : string
  ; model_parameters : Jsonaf.t
  ; seeds : int option list
  ; provenance : provenance
  ; runtime_revision : string
  ; oracle_revision : string
  ; provider_timeout_seconds : float
  ; case_timeout_seconds : float
  ; max_transcript_bytes : int
  }
[@@deriving sexp, jsonaf]

type audit =
  | Unmeasured
  | Observed of string list
[@@deriving sexp, jsonaf]

type prepared =
  { backend : backend
  ; target_identity : string
  ; capability_identity : string
  ; audit : unit -> audit
  }

type exchange =
  { step : int
  ; messages : message list
  ; action : action option
  ; provider_input_tokens : int option
  ; elapsed_seconds : float
  ; failure : string option
  }
[@@deriving sexp, jsonaf]

type row =
  { task_id : string
  ; policy : policy
  ; repetition : int
  ; seed : int option
  ; target_identity : string option
  ; capability_identity : string option
  ; result : result option
  ; failure : string option
  ; audit : audit
  ; exchanges : exchange list
  ; elapsed_seconds : float
  }
[@@deriving sexp, jsonaf]

type artifact =
  { version : int
  ; config : config
  ; manifest : Jsonaf.t
  ; run_fingerprint : string
  ; rows : row list
  }
[@@deriving sexp, jsonaf]

let fingerprint config =
  Jsonaf.to_string
    (`Object [ "config", jsonaf_of_config config; "manifest", Tasks.manifest ])
  |> Chatmd_shell_spec.Source_ref.digest
;;

let validate config =
  let positive value = Float.is_finite value && Float.(value > 0.) in
  match
    String.is_empty config.model
    || String.is_empty config.runtime_revision
    || String.is_empty config.oracle_revision
    || List.is_empty config.seeds
    || (not
          (positive config.provider_timeout_seconds
           && positive config.case_timeout_seconds))
    || config.max_transcript_bytes <= 0
  with
  | true -> Error "model, revisions, seeds and positive finite budgets are required"
  | false ->
    (match config.model_parameters with
     | `Object fields
       when not (List.contains_dup (List.map fields ~f:fst) ~compare:String.compare) ->
       Ok ()
     | _ -> Error "model parameters must be an object with unique keys")
;;

(* Factories are trusted host code. Only provider-visible messages/actions cross
   the authoring boundary. An external adapter must honor the supplied model,
   settings and seed; a seed is recorded, not a determinism guarantee. *)
let run ~env ?(real_model_authorized = false) ~config ~with_backend ~make_provider () =
  validate config |> Result.ok_or_failwith;
  (match config.provenance, real_model_authorized with
   | Real_model, false ->
     invalid_arg "real-model evaluation requires explicit authorization"
   | _ -> ());
  let clock = Eio.Stdenv.clock env in
  let epoch = Eio.Time.Mono.now (Eio.Stdenv.mono_clock env) in
  let now () =
    Mtime.span epoch (Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
    |> Mtime.Span.to_float_ns
    |> fun ns -> ns /. 1e9
  in
  let model_identity =
    Jsonaf.to_string
      (`Object [ "name", `String config.model; "parameters", config.model_parameters ])
  in
  let rows =
    List.concat_mapi config.seeds ~f:(fun repetition seed ->
      List.concat_map Tasks.all ~f:(fun task ->
        List.map policies ~f:(fun policy ->
          let started = now () in
          let exchanges = ref [] in
          let transcript_bytes = ref 0 in
          let add exchange =
            let bytes = String.length (Jsonaf.to_string (jsonaf_of_exchange exchange)) in
            match bytes > config.max_transcript_bytes - !transcript_bytes with
            | true -> raise (Infrastructure_failure "transcript budget exceeded")
            | false ->
              transcript_bytes := !transcript_bytes + bytes;
              exchanges := exchange :: !exchanges
          in
          let failure_message exn =
            match exn with
            | Infrastructure_failure message -> message
            | Eio.Time.Timeout -> "evaluation deadline exceeded"
            | _ -> Exn.to_string exn |> fun text -> String.prefix text 4096
          in
          let protect f =
            match f () with
            | value -> Ok value
            | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
            | exception exn -> Error (failure_message exn)
          in
          let measured =
            protect (fun () ->
              Eio.Time.with_timeout_exn clock config.case_timeout_seconds (fun () ->
                with_backend task (fun prepared ->
                  let provider = make_provider ~config ~task ~policy ~repetition ~seed in
                  let provider ~step ~messages =
                    let request_bytes =
                      `Array (List.map messages ~f:jsonaf_of_message)
                      |> Jsonaf.to_string
                      |> String.length
                    in
                    (match
                       request_bytes > config.max_transcript_bytes - !transcript_bytes
                     with
                     | true ->
                       raise
                         (Infrastructure_failure
                            "transcript budget exceeded before provider request")
                     | false -> ());
                    let request_started = now () in
                    let response : (Runner.answer, string) Result.t =
                      protect (fun () ->
                        Eio.Time.with_timeout_exn
                          clock
                          config.provider_timeout_seconds
                          (fun () -> provider ~step ~messages))
                    in
                    let action, usage, failure =
                      match response with
                      | Ok response ->
                        Some response.action, response.provider_input_tokens, None
                      | Error message -> None, None, Some message
                    in
                    add
                      { step
                      ; messages
                      ; action
                      ; provider_input_tokens = usage
                      ; elapsed_seconds = now () -. request_started
                      ; failure
                      };
                    match response with
                    | Ok response -> response
                    | Error message -> raise (Infrastructure_failure message)
                  in
                  let original = prepared.backend in
                  let references f input =
                    match protect (fun () -> f input) with
                    | Ok messages -> messages
                    | Error message -> raise (Infrastructure_failure message)
                  in
                  let backend =
                    { original with
                      prepare = references original.prepare
                    ; preload = references original.preload
                    ; retrieve = references original.retrieve
                    ; validate =
                        (fun candidate ->
                          match protect (fun () -> original.validate candidate) with
                          | Ok result -> result
                          | Error message -> Invalid (Infrastructure, message))
                    ; execute =
                        (fun candidate ->
                          match protect (fun () -> original.execute candidate) with
                          | Ok result -> result
                          | Error message -> Failed (Infrastructure, message))
                    }
                  in
                  let result =
                    Runner.run
                      ~now
                      ~provenance:config.provenance
                      ~model:model_identity
                      ~suite_revision:Tasks.fingerprint
                      ~runtime_revision:config.runtime_revision
                      ~limits:Tasks.limits
                      ~policy
                      ~backend
                      ~provider
                      task
                  in
                  ( result
                  , prepared.target_identity
                  , prepared.capability_identity
                  , prepared.audit () ))))
          in
          let result, target_identity, capability_identity, audit, failure =
            match measured with
            | Ok (result, target, capabilities, audit) ->
              Some result, Some target, Some capabilities, audit, None
            | Error message -> None, None, None, Unmeasured, Some message
          in
          { task_id = task.id
          ; policy
          ; repetition
          ; seed
          ; target_identity
          ; capability_identity
          ; result
          ; failure
          ; audit
          ; exchanges = List.rev !exchanges
          ; elapsed_seconds = now () -. started
          })))
  in
  { version = 1
  ; config
  ; manifest = Tasks.manifest
  ; run_fingerprint = fingerprint config
  ; rows
  }
;;
