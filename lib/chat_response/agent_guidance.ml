open! Core
module P = Agent_protocol

module Prepared = struct
  type t =
    { guidance : Inference.Request.Guidance.t option
    ; guard : Inference_runtime.Submission_guard.t
    ; reserved_input_tokens : int
    }

  let create ~guidance ~guard ~reserved_input_tokens =
    if
      reserved_input_tokens < 0
      || Option.value_map guidance ~default:(reserved_input_tokens <> 0) ~f:(fun _ ->
        reserved_input_tokens = 0)
    then Error (P.Error.invalid_request "invalid instruction token reservation")
    else Ok { guidance; guard; reserved_input_tokens }
  ;;
end

type t = { with_guidance : 'a. (Prepared.t -> 'a) -> ('a, P.Error.t) result }

module Scope = struct
  type nonrec t =
    | Eligible of t option
    | Excluded
end

let with_request t ~request ~submission_guard ~limits f =
  match t with
  | None -> Ok (f request ~guard:submission_guard ~reserved_input_tokens:0)
  | Some t ->
    let nested =
      t.with_guidance (fun prepared ->
        let open Result.Let_syntax in
        if Option.is_some (Inference.Request.guidance request)
        then Error (P.Error.invalid_request "agent request already contains guidance")
        else (
          let%map request =
            Inference.Request.with_guidance request ~guidance:prepared.guidance ~limits
            |> Result.map_error ~f:(fun _ ->
              P.Error.invalid_request
                "agent request guidance exceeds complete request limits")
          in
          f
            request
            ~guard:
              (Inference_runtime.Submission_guard.compose submission_guard prepared.guard)
            ~reserved_input_tokens:prepared.reserved_input_tokens))
    in
    Result.join nested
;;

let with_optional_hint t ~request ~submission_guard ~limits f =
  let module R = Inference.Request in
  let use request guard ~included = f request ~guard ~included in
  match t with
  | None -> Ok (use request submission_guard ~included:false)
  | Some t ->
    let entered = ref false in
    let result =
      t.with_guidance (fun prepared ->
        entered := true;
        match prepared.guidance with
        | None -> use request submission_guard ~included:false
        | Some hint ->
          let hint =
            match R.guidance request with
            | None -> Ok hint
            | Some installed ->
              R.Guidance.create
                ~text:(R.Guidance.text installed ^ "\n\n" ^ R.Guidance.text hint)
                ~provenance:"ochat installed guidance and recovery hint"
                ~limits
              |> Result.map_error ~f:(fun _ -> ())
          in
          let authorized =
            Result.bind hint ~f:(fun hint ->
              R.with_guidance request ~guidance:(Some hint) ~limits
              |> Result.map_error ~f:(fun _ -> ()))
          in
          (match authorized with
           | Ok request ->
             let guard =
               Inference_runtime.Submission_guard.compose submission_guard prepared.guard
             in
             use request guard ~included:true
           | Error _ -> use request submission_guard ~included:false))
    in
    (match result with
     | Error _ when not !entered -> Ok (use request submission_guard ~included:false)
     | result -> result)
;;
