open! Core
module P = Agent_protocol

type t =
  { origin : Recurrence_run_origin.t
  ; run : P.Run.t
  ; admission : P.Run_receipt.t
  ; terminal : Recurrence_run_reference.t option
  }

let equal t other =
  Recurrence_run_origin.equal t.origin other.origin
  && P.Run.equal t.run other.run
  && P.Run_receipt.equal t.admission other.admission
  && Option.equal Recurrence_run_reference.equal t.terminal other.terminal
;;

let conflict message = P.Error.create Conflict ~message ~retryable:false ()

let find index ~intent ~attempt =
  let open Result.Let_syntax in
  let%bind receipt =
    Run_state.receipt
      index
      ~principal_id:(Recurrence_execution_attempt.principal_id attempt)
      ~key:(Recurrence_execution_attempt.key attempt)
      ~request_sha256:(Recurrence_execution_attempt.request_sha256 attempt)
  in
  match receipt with
  | None -> Ok None
  | Some admission ->
    let%bind origin =
      Result.of_option
        (Run_state.recurrence_origin index admission.run_id)
        ~error:(conflict "retained Run lacks its exact recurrence origin")
    in
    let%bind run =
      Result.of_option
        (Run_state.find index admission.run_id)
        ~error:(conflict "recurrence origin lost its retained Run")
    in
    let%bind () = Recurrence_run_origin.validate origin ~intent ~attempt in
    let%bind () = Recurrence_run_origin.validate_run origin ~run ~admission in
    let%map terminal =
      match run.lifecycle with
      | Admitted | Active | Waiting _ -> Ok None
      | Terminal _ ->
        let receipts =
          List.filter (Run_state.receipts index) ~f:(fun receipt ->
            P.Run_receipt.Kind.equal receipt.kind Terminal
            && P.Id.Run.equal receipt.run_id run.id
            && Int64.equal receipt.run_revision run.revision)
        in
        (match receipts with
         | [ receipt ] ->
           Recurrence_run_reference.create ~origin ~run ~receipt
           |> Result.map ~f:Option.some
         | [] | _ :: _ :: _ ->
           Error (conflict "recurrence terminal Run lacks one exact terminal receipt"))
    in
    Some { origin; run; admission; terminal }
;;
