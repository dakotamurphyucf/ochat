open! Core
module P = Agent_protocol
module Value = Chatml.Chatml_value_codec

module Format = struct
  type t =
    | Legacy
    | Extensibility_v1
  [@@deriving equal, sexp_of]

  let of_manager manager =
    match Chat_response.Moderator_manager.extension_definition manager with
    | None -> Legacy
    | Some _ -> Extensibility_v1
  ;;
end

let failure message = P.Error.create Invalid_state ~message ~retryable:false ()

let job_recipe_and_result (job : Agent_protocol.Job.t) =
  match job.payload with
  | `Object fields ->
    (match List.Assoc.find fields "recipe" ~equal:String.equal with
     | Some (`String recipe) -> Ok (recipe, job.result)
     | Some _ | None -> Error (failure "model job payload has no recipe"))
  | _ -> Error (failure "model job payload must be an object")
;;

let legacy (job : Agent_protocol.Job.t) =
  let open Result.Let_syntax in
  let%bind recipe, result = job_recipe_and_result job in
  let job_id = Agent_protocol.Id.Job.to_string job.id in
  match job.status with
  | Agent_protocol.Job.Succeeded ->
    let%map result =
      result |> Result.of_option ~error:(failure "succeeded model job has no result")
    in
    Chatml.Chatml_lang.VVariant
      ( "Model_job_succeeded"
      , [ VString job_id
        ; VString recipe
        ; Chatml.Chatml_value_codec.jsonaf_to_value result
        ] )
  | Failed error ->
    Ok
      (Chatml.Chatml_lang.VVariant
         ("Model_job_failed", [ VString job_id; VString recipe; VString error.message ]))
  | Interrupted reason ->
    Ok
      (Chatml.Chatml_lang.VVariant
         ("Model_job_failed", [ VString job_id; VString recipe; VString reason ]))
  | Cancelled ->
    Ok
      (Chatml.Chatml_lang.VVariant
         ( "Model_job_failed"
         , [ VString job_id; VString recipe; VString "job was cancelled" ] ))
  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
    Error (failure "model job is not terminal")
;;

let render ?(format = Format.Legacy) (job : P.Job.t) =
  match format with
  | Legacy -> legacy job
  | Extensibility_v1 ->
    let open Result.Let_syntax in
    let%bind () =
      match job.kind with
      | Model_call -> Ok ()
      | Async_tool | Nested_agent | Scheduled_event | Shell_process | Compaction ->
        Error (failure "Model completion requires a Model Job")
    in
    let%bind recipe, result = job_recipe_and_result job in
    let%bind () =
      match job.status, result with
      | Succeeded, None -> Error (failure "succeeded model job has no result")
      | (Succeeded | Failed _ | Interrupted _ | Cancelled), _ -> Ok ()
      | (Queued | Running | Waiting_permission _ | Waiting_completion _), _ ->
        Error (failure "model job is not terminal")
    in
    let%bind result = P.Job.terminal_result job in
    let%bind result =
      Result.of_option result ~error:(failure "model job has no terminal result")
    in
    let payload =
      `Object
        [ "kind", `String "model_job_completed"
        ; "job_id", P.Id.Job.to_json job.id
        ; "attempt", `String (Int.to_string job.attempt)
        ; "recipe", `String recipe
        ; "result", P.Stored_completion.to_json result
        ]
    in
    Chat_response.Moderator_invocation.internal_event (Value.jsonaf_to_value payload)
    |> Result.map_error ~f:failure
;;
