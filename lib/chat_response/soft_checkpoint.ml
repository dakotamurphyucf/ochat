open! Core
module P = Agent_protocol
module R = Inference_runtime
module Preparation = Inference_client.Prepared_selection

type outcome =
  | Settled of Settled_tool_batch.pair
  | Ineligible
  | Interrupted

module Selected = struct
  type t =
    { candidate : Preparation.candidate
    ; for_attempt : R.Attempt.t -> R.Submission_guard.t
    ; approve_call : R.Attempt.t -> name:string -> arguments:string -> bool
    ; settle : R.Attempt.t -> outcome -> (unit, P.Error.t) result
    }

  let completed_call t attempt receipt =
    let module E = Inference.Event in
    let module S = History_entry.Payload.Semantic in
    let terminal = R.Receipt.terminal receipt in
    match
      ( R.Receipt.output_coverage receipt
      , E.Terminal.outcome terminal
      , R.Receipt.output receipt )
    with
    | Response_output, Completed, [ event ]
      when Transcript.Scope.equal (R.Attempt.scope attempt) (E.Terminal.scope terminal) ->
      (match E.view event with
       | Candidate_ready { item; payload; local_execution = Tool_candidate }
         when Transcript.Scope.equal item.scope (R.Attempt.scope attempt) ->
         let semantic = History_entry.Payload.semantic payload in
         (match S.view semantic, (S.metadata semantic).call_id with
          | Call { kind = Function; name; input_bytes; _ }, Value alias
            when (not (String.is_empty alias))
                 && t.approve_call attempt ~name ~arguments:input_bytes -> Some event
          | _ -> None)
       | Live _ | Terminal _ | Candidate_ready _ -> None)
    | _ -> None
  ;;
end

type tools =
  | Unselected
  | Register
  | Register_and_write
[@@deriving equal]

type select =
  tools:tools
  -> request:Inference.Request.t
  -> prepare_isolated:
       (Inference.Request.t -> (Preparation.candidate, R.Preparation_error.t) result)
  -> R.Prepared.t
  -> Selected.t option

exception Tool_refused
