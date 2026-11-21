open! Core
module Wire = Agent_protocol.Recovery_rollover_control
module Rollover = Recovery_rollover
module A = Inference_runtime.Context_capacity.Assessment

let receipt (receipt : Rollover.Receipt.t) =
  let module R = Wire.Receipt in
  let open Result.Let_syntax in
  let expected = receipt.intent.expected in
  let%bind basis = Wire.Basis.create expected.scope ~revision:expected.revision in
  let outcome =
    match receipt.status with
    | Pending -> R.Pending
    | Terminal (Committed { successor; cut_revision; checkpoint; _ }) ->
      let quality =
        match checkpoint with
        | Checkpointed _ -> R.Checkpointed
        | Archive_only Checkpoint_missing -> Archive_only_missing_checkpoint
        | Archive_only Checkpoint_unavailable -> Archive_only_unavailable_checkpoint
      in
      Committed { successor; cut_revision; quality }
    | Terminal (Superseded reason) ->
      Failed
        (match reason with
         | Basis_changed -> R.Basis_changed
         | Cancelled -> Cancelled
         | Checkpoint_required -> Checkpoint_required
         | Hook_vetoed -> Hook_vetoed
         | Hook_failed -> Hook_failed)
  in
  R.create basis outcome
;;

let capacity (state : Session_state.t) (status : Session_context_capacity.status) =
  let module C = Wire.Capacity in
  let module T = Inference.Request.Target in
  let module O = Inference.Observation.Configuration in
  let display_target ~profile ~model =
    C.target ~profile ~model ~configuration_revision:state.spec.configuration_revision
    |> Result.ok
  in
  let target =
    Result.ok (Configuration_transition.target state)
    |> Option.bind ~f:(fun target ->
      display_target ~profile:(T.profile target) ~model:(T.model target))
  in
  let unknown reason = C.Unknown { target; reason } in
  match status with
  | Disabled -> unknown Disabled
  | Awaiting_preparation _ -> unknown Awaiting_preparation
  | Stale_preparation _ -> unknown Stale_preparation
  | Current { phase; configuration; assessment; _ } ->
    (match
       display_target ~profile:(O.profile configuration) ~model:(O.model configuration)
     with
     | None -> unknown Evidence_unavailable
     | Some target ->
       let remaining =
         match A.view assessment with
         | Unavailable _ -> None
         | Assessed { evidence; remaining; _ } ->
           let kind =
             match evidence with
             | Measured _ -> C.Measured
             | Estimated _ -> C.Estimated
           in
           let remaining =
             match remaining with
             | Available n -> C.Available n
             | Exceeded_by n -> Exceeded_by n
             | Excess_out_of_range -> Excess_out_of_range
           in
           Some (kind, remaining)
       in
       Observed { target; freshness = phase; remaining })
;;
