open! Core
module P = Agent_protocol
module Publisher = Agent_store.Job_result_store.Publisher
module Request = Chat_response.Background_request

type t =
  | Artifact
  | Bounded_inline of int

let insufficient () =
  Error
    (P.Error.create
       Resource_limit
       ~message:"The job wait has no producer within its reserved delivery capacity."
       ~retryable:false
       ())
;;

let capture (job : P.Job.t) ~publisher =
  match job.kind, job.launch with
  | Async_tool, Some _ ->
    (match publisher with
     | Some _ -> Ok Artifact
     | None ->
       (match
          Request.of_json ~policy:Chat_response.One_off_request.default_policy job.payload
        with
        | Error _ -> insufficient ()
        | Ok request ->
          let policy = Request.policy request in
          if policy.max_output_bytes <= 512
          then Ok (Bounded_inline 515)
          else insufficient ()))
  | Async_tool, None
  | ( (Model_call | Nested_agent | Scheduled_event | Shell_process | Compaction)
    , (Some _ | None) ) -> insufficient ()
;;

let storage = function
  | Artifact -> Publisher.Storage.Artifact
  | Bounded_inline _ -> Automatic
;;

let normalize_completion t completion =
  let open Result.Let_syntax in
  let%bind () = P.Completion.validate completion in
  match t with
  | Artifact -> Ok (completion, false)
  | Bounded_inline max_bytes ->
    (match
       P.Json_codec.validate_limits
         ~max_depth:132
         ~max_bytes
         (P.Completion.to_json completion)
     with
     | Ok () -> Ok (completion, false)
     | Error _ ->
       let completion =
         match completion with
         | Succeeded _ ->
           P.Completion.Failed
             { code = "background.result_limit"
             ; message = "The actual completion exceeds its captured run delivery limit."
             ; retryable = false
             ; details = `Null
             }
         | Failed failure ->
           let rec prefix bytes =
             let text = String.prefix failure.message bytes in
             if Stdlib.String.is_valid_utf_8 text || bytes = 0
             then text
             else prefix (bytes - 1)
           in
           let candidate =
             P.Completion.Failed
               { failure with
                 message =
                   prefix (Int.min 128 (String.length failure.message))
                   ^ " [diagnostic truncated]"
               ; details = `Null
               }
           in
           (match
              P.Json_codec.validate_limits
                ~max_depth:132
                ~max_bytes
                (P.Completion.to_json candidate)
            with
            | Ok () -> candidate
            | Error _ ->
              let digest =
                P.Completion.to_json completion
                |> Jsonaf.to_string
                |> Digestif.SHA256.digest_string
                |> Digestif.SHA256.to_hex
              in
              Failed
                { code = "background.result_limit"
                ; message = "Failure diagnostic omitted; sha256:" ^ digest
                ; retryable = failure.retryable
                ; details = `Null
                })
         | Cancelled _ ->
           Cancelled
             "The job was cancelled; its diagnostic exceeds the captured delivery limit."
         | Expired -> Expired
       in
       let%map () =
         P.Json_codec.validate_limits
           ~max_depth:132
           ~max_bytes
           (P.Completion.to_json completion)
       in
       completion, true)
;;

let normalize_host_control completion =
  normalize_completion (Bounded_inline 515) completion
;;

let validate_wait (job : P.Job.t) ~(run : P.Run.t) ~publisher =
  let open Result.Let_syntax in
  match job.status with
  | Queued | Running | Waiting_permission _ | Waiting_completion _ ->
    Result.map (capture job ~publisher) ~f:ignore
  | Succeeded | Failed _ | Cancelled | Interrupted _ ->
    let%bind frame =
      Chat_response.Background_delivery.create ~source:run.source.observer job
      |> Result.map_error ~f:(fun _ ->
        P.Error.create
          Resource_limit
          ~message:"The retained job occurrence has no bounded delivery frame."
          ~retryable:false
          ())
    in
    Result.map (Run_job_delivery.capture run ~frame) ~f:ignore
;;

let bookkeeping_reserve index ~jobs =
  let open Result.Let_syntax in
  let%bind jobs =
    match
      String.Map.of_alist
        (List.map jobs ~f:(fun (job : P.Job.t) -> P.Id.Job.to_string job.id, job))
    with
    | `Ok jobs -> Ok jobs
    | `Duplicate_key _ ->
      Error (P.Error.invalid_request "duplicate job in capacity basis")
  in
  List.fold_result (Run_state.runs index) ~init:0 ~f:(fun bytes run ->
    match run.P.Run.lifecycle with
    | Terminal _ -> Ok bytes
    | Admitted | Active | Waiting _ ->
      let known =
        Set.of_list
          (module P.Run_work)
          (List.map run.terminal_work ~f:(fun proof -> proof.P.Run_work.Terminal.work))
      in
      let%bind remaining, additional =
        List.fold_result run.owned_work ~init:(0, 0) ~f:(fun (count, additional) work ->
          if Set.mem known work
          then Ok (count, additional)
          else (
            match work.P.Run_work.key with
            | Operation _
            | Retained
                ( Schedule _
                | Invocation _
                | Subscription _
                | Delivery _
                | Moderator_execution _ ) -> Ok (count, additional)
            | Retained (Job { id; attempt }) ->
              let maximum =
                match Map.find jobs (P.Id.Job.to_string id) with
                | None -> attempt
                | Some job ->
                  (match job.retry_policy with
                   | Never -> attempt
                   | Safe_retry { max_attempts; _ } | Idempotent { max_attempts; _ } ->
                     max_attempts)
              in
              let difference = Int.max 0 (maximum - attempt) in
              if difference >= P.Run_limits.max_occurrences - count
              then insufficient ()
              else Ok (count + difference + 1, additional + difference)))
      in
      let%bind () = P.Run_limits.check_count (List.length run.owned_work + additional) in
      if remaining > (P.Run_limits.max_document_bytes - bytes - 4096) / 4096
      then insufficient ()
      else Ok (bytes + ((remaining + 1) * 4096)))
;;
