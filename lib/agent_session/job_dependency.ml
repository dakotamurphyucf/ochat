open Core
module P = Agent_protocol
module J = P.Job

let schema (dependency : J.dependency) =
  match dependency.completion_schema with
  | None -> Ok None
  | Some schema ->
    Chatmd_shell_spec.Tool_schema.compile schema
    |> Result.map ~f:Option.some
    |> Result.map_error ~f:(fun _ ->
      P.Error.invalid_request "invalid captured job completion schema")
;;

let completion dependency outcome =
  let open Result.Let_syntax in
  let%map schema = schema dependency in
  let valid =
    Result.is_ok
      (P.Json_codec.validate_limits
         ~max_bytes:dependency.max_output_bytes
         ~max_depth:dependency.max_output_depth
         (P.Completion.to_json outcome))
    &&
    match outcome, schema with
    | P.Completion.Succeeded value, Some schema ->
      Result.is_ok (Chatmd_shell_spec.Tool_schema.validate schema value)
    | _ -> true
  in
  match valid with
  | true -> outcome
  | false ->
    P.Completion.Failed
      { code = "background.invalid_completion"
      ; message = "The eventual result does not satisfy the captured completion contract."
      ; retryable = false
      ; details = `Null
      }
;;

let validate ~invocations ~events ~jobs ~subscriptions (parent : J.t) =
  let invalid message = Error (P.Error.invalid_request message) in
  match parent.status with
  | Waiting_completion dependency ->
    let open Result.Let_syntax in
    let%bind _ = J.of_json (J.to_json parent) in
    let%bind _ = schema dependency in
    let%bind invocation =
      List.find invocations ~f:(fun invocation ->
        P.Id.Invocation.equal invocation.P.Invocation.context.id dependency.invocation_id)
      |> Result.of_option
           ~error:(P.Error.invalid_request "job dependency invocation is missing")
    in
    let%bind () =
      match invocation.status with
      | (Resolved (Pending (work, _)) | Published (Pending (work, _)))
        when P.Invocation.equal_work work dependency.work -> Ok ()
      | _ -> invalid "job dependency does not match its invocation's Pending outcome"
    in
    let%bind target_session, target_generation =
      match dependency.work with
      | P.Invocation.Job target ->
        let%bind child =
          List.find jobs ~f:(fun job -> P.Id.Job.equal job.J.id target)
          |> Result.of_option
               ~error:(P.Error.invalid_request "job dependency target is missing")
        in
        (match child.kind, child.launch with
         | ( Async_tool
           , Some { owner = Invocation owner; parent_job = Some (id, attempt); _ } )
           when P.Id.Invocation.equal owner invocation.context.id
                && P.Id.Job.equal id parent.id
                && Int.equal attempt parent.attempt
                && not (P.Id.Job.equal child.id parent.id) ->
           Ok (child.session_id, child.generation)
         | _ -> invalid "job dependency does not belong to this parent attempt")
      | Subscription target ->
        let%bind child =
          List.find subscriptions ~f:(fun subscription ->
            P.Id.Subscription.equal subscription.P.Subscription.context.id target)
          |> Result.of_option
               ~error:
                 (P.Error.invalid_request "subscription dependency target is missing")
        in
        let%bind () = P.Subscription.validate child in
        let%bind () = Job_launch.validate_subscription ~invocations ~events ~jobs child in
        (match child.context.parent_job with
         | Some (id, attempt)
           when P.Id.Job.equal id parent.id
                && Int.equal attempt parent.attempt
                && P.Id.Invocation.equal child.context.invocation_id invocation.context.id
           -> Ok (child.context.session_id, child.context.generation)
         | _ -> invalid "subscription dependency does not belong to this parent attempt")
    in
    let%bind () =
      match invocation.context.deadline with
      | Some deadline when P.Timestamp.equal deadline dependency.deadline -> Ok ()
      | _ -> invalid "job dependency changed its invocation deadline"
    in
    (match
       P.Id.Session.equal parent.session_id target_session
       && P.Id.Session.equal parent.session_id invocation.context.session_id
       && Int.equal parent.generation target_generation
       && Int.equal parent.generation invocation.context.generation
     with
     | true -> Ok ()
     | false -> invalid "job dependency crosses session or generation")
  | _ -> Ok ()
;;
