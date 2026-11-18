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
