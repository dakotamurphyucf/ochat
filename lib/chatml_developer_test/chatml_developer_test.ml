open Core
module Capture = Chatml_developer_capture
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Compilation = Chatml_compilation
module Linked = Chatml_linked_program
module Execution = Chatml_execution
module Runtime = Chatml_host_runtime
module Lang = Chatml.Chatml_lang
module Codec = Chatml.Chatml_value_codec

module Diagnostic = struct
  type t =
    { code : string
    ; message : string
    ; location : Captured.Location.t option
    }

  let create ?location ~code ~message () =
    { code = String.prefix code 256
    ; message = String.prefix message (16 * 1024)
    ; location
    }
  ;;

  let code t = t.code
  let message t = t.message
  let location t = t.location

  let verified_location prepared diagnostic =
    match diagnostic.Chatml_diagnostic.site with
    | None -> None
    | Some site ->
      let source = Chatml_diagnostic.Site.source site in
      (match Prepared.find_source prepared (Captured.id source) with
       | Some selected
         when Captured.Digest.equal (Captured.digest selected) (Captured.digest source) ->
         Some (Chatml_diagnostic.Site.location site)
       | _ -> None)
  ;;

  let of_compile prepared (error : Compilation.error) =
    let location =
      Option.bind error.diagnostic ~f:(fun diagnostic ->
        verified_location prepared diagnostic.diagnostic)
    in
    create ?location ~code:error.code ~message:error.message ()
  ;;

  let of_runtime (error : Execution.error) =
    create ~code:error.code ~message:error.message ()
  ;;
end

module Check = struct
  module Id = struct
    type t = string [@@deriving equal, sexp_of]

    let create value =
      if String.length value = 0 || String.length value > 256
      then Or_error.error_string "invalid developer check ID"
      else Ok value
    ;;
  end

  module Outcome = struct
    type t =
      | Ready
      | Rejected of Diagnostic.t
  end

  type state =
    | Ready of Linked.t
    | Rejected of Diagnostic.t

  type t =
    { id : Id.t
    ; capture : Capture.t
    ; state : state
    }

  let target = function
    | Chatml.Chatml_compiler_contract.Target.One_off_v1 -> Compilation.One_off_v1
    | Tool_v1 -> Tool_v1
    | Moderator_v1 -> Moderator_v1
    | Delegated_moderator_v1 -> Delegated_moderator_v1
    | Developer_tool_v1 -> Developer_tool_v1
  ;;

  let perform capture ~id ~policy ~max_dependency_depth ~env =
    let result =
      Compilation.compile_prepared_with_policy
        (Capture.prepared capture)
        ~policy
        ~max_dependency_depth
        ~env
        ~target:(target (Capture.target capture))
        ()
    in
    let state =
      match result with
      | Ok linked -> Ready linked
      | Error error -> Rejected (Diagnostic.of_compile (Capture.prepared capture) error)
    in
    { id; capture; state }
  ;;

  let id t = t.id
  let capture_fingerprint t = Capture.fingerprint t.capture

  let outcome t =
    match t.state with
    | Ready _ -> Outcome.Ready
    | Rejected diagnostic -> Rejected diagnostic
  ;;
end

module Run_id = struct
  type t = string [@@deriving equal, sexp_of]

  let create value =
    if String.length value = 0 || String.length value > 256
    then Or_error.error_string "invalid developer run ID"
    else Ok value
  ;;
end

module Failure = struct
  type t =
    | Compile of Diagnostic.t
    | Fixture_mismatch of
        { expected_step : int
        ; actual_tool : string
        }
    | Fixture_missing of { next_step : int }
    | Unsupported_effect of { operation : string }
    | Runtime of Diagnostic.t
    | Invalid_result of Diagnostic.t
    | Output_limit
end

module Error = struct
  type t =
    | No_fixtures
    | Mismatched_check
    | Invalid_output_limit
    | Invalid_execution_limits
  [@@deriving equal, sexp_of]
end

module Receipt = struct
  type outcome =
    | Succeeded of Jsonaf.t
    | Failed of Failure.t

  module Log = struct
    type t =
      { level : Runtime.log_level
      ; message : string
      }

    let level t = t.level
    let message t = t.message
  end

  type t =
    { id : Run_id.t
    ; capture_fingerprint : Captured.Digest.t
    ; graph_fingerprint : Captured.Digest.t
    ; fixture_digest : Captured.Digest.t
    ; matched_steps : int
    ; logs : Log.t list
    ; outcome : outcome
    }

  let id t = t.id
  let capture_fingerprint t = t.capture_fingerprint
  let graph_fingerprint t = t.graph_fingerprint
  let fixture_digest t = t.fixture_digest
  let matched_steps t = t.matched_steps
  let logs t = t.logs
  let outcome t = t.outcome
end

let allowed_effect ~name ~spawned =
  (not spawned)
  && List.mem
       [ "Tool.call"; "Log.debug"; "Log.info"; "Log.warn"; "Log.error" ]
       name
       ~equal:String.equal
;;

let failure_bytes = function
  | Failure.Compile diagnostic | Runtime diagnostic | Invalid_result diagnostic ->
    String.length (Diagnostic.code diagnostic)
    + String.length (Diagnostic.message diagnostic)
  | Fixture_mismatch { actual_tool; _ } -> String.length actual_tool + 32
  | Fixture_missing _ -> 32
  | Unsupported_effect { operation } -> String.length operation + 32
  | Output_limit -> 0
;;

let run capture ~check ~id ~limits ~max_output_bytes ~env =
  let open Result.Let_syntax in
  let%bind fixtures =
    Capture.fixtures capture |> Result.of_option ~error:Error.No_fixtures
  in
  let%bind () =
    if phys_equal check.Check.capture capture then Ok () else Error Error.Mismatched_check
  in
  let%bind () =
    if max_output_bytes <= 0 then Error Error.Invalid_output_limit else Ok ()
  in
  let%bind () =
    if Execution.valid_limits limits then Ok () else Error Error.Invalid_execution_limits
  in
  let cursor = ref 0 in
  let steps = Capture.Fixture.steps fixtures |> Array.of_list in
  let sticky = ref None in
  let logs_rev = ref [] in
  let retained = ref 0 in
  let reserve bytes =
    if bytes < 0 || bytes > max_output_bytes - !retained
    then false
    else (
      retained := !retained + bytes;
      true)
  in
  let receipt outcome =
    { Receipt.id
    ; capture_fingerprint = Capture.fingerprint capture
    ; graph_fingerprint = Capture.graph_fingerprint capture
    ; fixture_digest = Capture.Fixture.digest fixtures
    ; matched_steps = !cursor
    ; logs = List.rev !logs_rev
    ; outcome
    }
  in
  let bounded_failure failure =
    if reserve (failure_bytes failure)
    then Receipt.Failed failure
    else Receipt.Failed Failure.Output_limit
  in
  match check.state with
  | Rejected diagnostic -> Ok (receipt (bounded_failure (Failure.Compile diagnostic)))
  | Ready linked ->
    let result =
      Execution.with_control ~policy:(Bounded limits) ~env (fun control ->
        let control =
          Option.map control ~f:(fun original ->
            { original with
              before_effect =
                (fun ~name ~spawned ->
                  original.before_effect ~name ~spawned;
                  if (not (allowed_effect ~name ~spawned)) && Option.is_none !sticky
                  then
                    sticky
                    := Some
                         (Failure.Unsupported_effect
                            { operation = String.prefix name 256 }))
            })
        in
        let on_log _session ~level ~message =
          let bytes =
            String.length message + String.length (Runtime.string_of_log_level level) + 1
          in
          if reserve bytes
          then (
            logs_rev := { Receipt.Log.level; message } :: !logs_rev;
            Ok ())
          else (
            sticky := Some Failure.Output_limit;
            Error "developer test output limit exceeded")
        in
        let on_tool_call _session ~name ~args =
          let mismatch () =
            if Option.is_none !sticky
            then
              sticky
              := Some
                   (Failure.Fixture_mismatch
                      { expected_step = !cursor; actual_tool = String.prefix name 256 });
            Error "developer fixture call did not match"
          in
          if !cursor >= Array.length steps
          then mismatch ()
          else (
            let step = steps.(!cursor) in
            if not (String.equal name (Capture.Fixture.Step.tool_name step))
            then mismatch ()
            else (
              match Codec.export_json ?control args with
              | Error _ -> mismatch ()
              | Ok input ->
                if not (Capture.Fixture.Step.matches_input step input)
                then mismatch ()
                else (
                  incr cursor;
                  match Capture.Fixture.Step.response step with
                  | Return value ->
                    Ok (Lang.VVariant ("Ok", [ Codec.import_json ?control value ]))
                  | Fail message -> Ok (Lang.VVariant ("Error", [ Lang.VString message ])))))
        in
        let handlers = { Runtime.default_handlers with on_log; on_tool_call } in
        let operations =
          Runtime.default_operations ~handlers ()
          |> List.filter ~f:(fun operation ->
            allowed_effect ~name:operation.Runtime.name ~spawned:false)
        in
        let config : Runtime.runtime_config =
          { surface =
              (match Capture.target capture with
               | One_off_v1 -> Chatml.Chatml_extension_surface.one_off_v1
               | Tool_v1 | Developer_tool_v1 | Moderator_v1 | Delegated_moderator_v1 ->
                 assert false)
          ; operations
          }
        in
        let argument = Codec.import_json ?control (Capture.input capture) in
        let evaluated =
          Execution.run_prepared_in_scope
            linked
            ~control
            ~config
            ~entrypoint:(Capture.entrypoint capture)
            ~arguments:[ argument ]
            ()
        in
        match evaluated with
        | Error error -> Error (Failure.Runtime (Diagnostic.of_runtime error))
        | Ok value ->
          (match Codec.export_json ?control value with
           | Error message ->
             Error
               (Failure.Invalid_result
                  (Diagnostic.create ~code:"chatml.invalid_result" ~message ()))
           | Ok json ->
             let remaining = max_output_bytes - !retained in
             let size =
               if remaining <= 0
               then Error (Core.Error.of_string "output limit")
               else
                 Capture.Json_bounds.create
                   ~max_bytes:(Int.min remaining 1_048_576)
                   ~max_depth:(Int.min limits.max_depth 128)
                   ~max_nodes:100_000
                 |> Result.bind ~f:(fun bound ->
                   Capture.Json_bounds.measure bound json ~checkpoint:(fun () ->
                     Option.iter control ~f:(fun control ->
                       control.diagnostic_checkpoint ())))
             in
             (match size with
              | Error _ -> Error Failure.Output_limit
              | Ok bytes -> Ok (json, bytes))))
    in
    let outcome =
      match !sticky, result with
      | Some failure, _ -> Receipt.Failed failure
      | None, Error error ->
        Receipt.Failed (Failure.Runtime (Diagnostic.of_runtime error))
      | None, Ok (Error failure) -> Receipt.Failed failure
      | None, Ok (Ok _) when !cursor < Array.length steps ->
        Receipt.Failed (Failure.Fixture_missing { next_step = !cursor })
      | None, Ok (Ok (value, bytes)) ->
        if reserve bytes
        then Receipt.Succeeded value
        else Receipt.Failed Failure.Output_limit
    in
    let outcome =
      match outcome with
      | Receipt.Failed failure -> bounded_failure failure
      | _ -> outcome
    in
    Ok (receipt outcome)
;;
