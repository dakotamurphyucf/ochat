open! Core

type pair =
  { call : History_entry.Id.t
  ; output : History_entry.Id.t
  }

type t =
  { attempt : Inference_runtime.Attempt.t
  ; terminal : Inference.Event.Terminal.t
  ; pairs : pair list
  }

let create attempt receipt committed =
  let module R = Inference_runtime.Receipt in
  let module T = Inference.Event.Terminal in
  let terminal = R.terminal receipt in
  let scope = Inference_runtime.Attempt.scope attempt in
  match
    scope.relation, R.output_coverage receipt, T.outcome terminal, T.delivery terminal
  with
  | Root, Response_output, Completed, Response_started
    when Transcript.Scope.equal scope (T.scope terminal) && not (List.is_empty committed)
    ->
    let expected =
      List.filter_map (R.output receipt) ~f:(fun event ->
        match Inference.Event.view event with
        | Candidate_ready { item; local_execution = Tool_candidate; _ } ->
          Some (Transcript.Item.key item)
        | Candidate_ready { local_execution = Not_eligible; _ } | Live _ | Terminal _ ->
          None)
    in
    let observed, pairs = List.unzip committed in
    if List.equal Transcript.Item.Key.equal expected observed
    then Some { attempt; terminal; pairs }
    else None
  | _ -> None
;;

let attempt t = t.attempt
let terminal t = t.terminal
let pairs t = t.pairs

type decision =
  | Continue
  | Retire
