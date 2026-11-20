open! Core
module Observation = Inference.Observation

module Encoder = struct
  type t = Openai_responses_v1 [@@deriving equal, sexp_of]
end

let positive tokens = Int64.(tokens > zero)

let cap limit = function
  | None -> limit
  | Some value -> Int64.min limit value
;;

let input_limits ~total ~provider_input ~output ~checkpoint_reserve ~warning_lead =
  if Int64.(output >= total)
  then None
  else (
    let hard = cap Int64.(total - output) provider_input in
    if Int64.(checkpoint_reserve >= hard)
    then None
    else (
      let soft = Int64.(hard - checkpoint_reserve) in
      if Int64.(warning_lead >= soft)
      then None
      else Some (hard, soft, Int64.(soft - warning_lead))))
;;

module Policy = struct
  type model_limit =
    | Undeclared
    | Unbounded
    | Bounded of
        { total : int64
        ; input : int64 option
        ; output : int64 option
        }

  type allowance =
    { host_total : int64 option
    ; session_total : int64 option
    ; omitted_output_default : int64 option
    ; checkpoint_reserve : int64
    ; warning_lead : int64
    }

  type estimate =
    { estimator : Observation.Estimator.t
    ; margin : int64
    }

  type t =
    { target : Inference.Request.Target.t
    ; encoder : Encoder.t
    ; model_limit : model_limit
    ; allowance : allowance
    ; estimate : estimate option
    ; stamp : unit ref
    }

  type error =
    | Invalid_limits
    | Invalid_reservations
  [@@deriving equal, sexp_of]

  let allowed_total ~total t = cap (cap total t.host_total) t.session_total

  let create ~target ~encoder ~model_limit ~allowance ~estimate =
    let { host_total
        ; session_total
        ; omitted_output_default
        ; checkpoint_reserve
        ; warning_lead
        }
      =
      allowance
    in
    let minimum_output = Option.value omitted_output_default ~default:1L in
    let valid_bound total =
      Option.for_all ~f:(fun value -> positive value && Int64.(value <= total))
    in
    let result =
      if not (List.for_all [ host_total; session_total ] ~f:(Option.for_all ~f:positive))
      then Error Invalid_limits
      else if
        not
          (positive checkpoint_reserve
           && positive warning_lead
           && positive minimum_output
           && Option.for_all estimate ~f:(fun e -> Int64.(e.margin >= zero)))
      then Error Invalid_reservations
      else (
        match model_limit with
        | Undeclared | Unbounded -> Ok ()
        | Bounded { total; input; output } ->
          if not (positive total && valid_bound total input && valid_bound total output)
          then Error Invalid_limits
          else if
            (not
               (Option.for_all output ~f:(fun limit -> Int64.(minimum_output <= limit))))
            || Option.is_none
                 (input_limits
                    ~total:(allowed_total ~total allowance)
                    ~provider_input:input
                    ~output:minimum_output
                    ~checkpoint_reserve
                    ~warning_lead)
          then Error Invalid_reservations
          else Ok ())
    in
    Result.map result ~f:(fun () ->
      { target; encoder; model_limit; allowance; estimate; stamp = ref () })
  ;;
end

module Source = struct
  type incomplete =
    | Image
    | Document
    | Opaque_input
    | Unknown_wire_shape
    | Counter_limit
    | Counter_unavailable
  [@@deriving equal, sexp_of]

  type reading =
    | Covered of Observation.Count.t
    | Incomplete of incomplete

  type t =
    { encoder : Encoder.t
    ; read : unit -> reading
    }

  let create ~encoder ~read =
    let cached = Atomic.make None in
    let read () =
      match Atomic.get cached with
      | Some reading -> reading
      | None ->
        let reading = read () in
        if Atomic.compare_and_set cached None (Some reading)
        then reading
        else Option.value (Atomic.get cached) ~default:reading
    in
    { encoder; read }
  ;;
end

module Assessment = struct
  type unavailable =
    | Policy_missing
    | Target_changed
    | Encoder_changed
    | Source_missing
    | Limit_undeclared
    | Limit_unbounded
    | Output_unresolved
    | Output_outside_limit
    | Reserves_exhaust_limit
    | Coverage_incomplete of Source.incomplete
    | Estimate_unqualified
  [@@deriving equal, sexp_of]

  type evidence =
    | Measured of int64
    | Estimated of Observation.Context_estimate.t

  type pressure =
    | Below_warning
    | Near_limit
    | Checkpoint_due
    | Hard_limit_exceeded
  [@@deriving equal, sexp_of]

  type remaining =
    | Available of int64
    | Exceeded_by of int64
    | Excess_out_of_range
  [@@deriving equal, sexp_of]

  type view =
    | Unavailable of unavailable
    | Assessed of
        { evidence : evidence
        ; total : int64
        ; output : int64
        ; hard_input : int64
        ; soft_input : int64
        ; warn_input : int64
        ; margin : int64
        ; remaining : remaining
        ; pressure : pressure
        }

  type t =
    { preparation_id : string
    ; preparation_stamp : unit ref
    ; policy_stamp : unit ref option
    ; view : view
    }

  let preparation_id t = t.preparation_id
  let view t = t.view
end

module Binding = struct
  type t =
    { target : Inference.Request.Target.t
    ; configuration : Observation.Configuration.t
    ; source : Source.t option
    ; stamp : unit ref
    }

  let create ~target ~configuration ~source =
    { target; configuration; source; stamp = ref () }
  ;;

  let policy_stamp policy = Option.map policy ~f:(fun p -> p.Policy.stamp)

  let matches ?policy t (assessment : Assessment.t) =
    phys_equal t.stamp assessment.preparation_stamp
    && Option.equal phys_equal (policy_stamp policy) assessment.policy_stamp
  ;;

  let output_allowance t (allowance : Policy.allowance) =
    let selection =
      List.find_map
        (Observation.Configuration.settings t.configuration)
        ~f:(fun setting ->
          match setting.name with
          | Max_output_tokens -> Some setting.selection
          | _ -> None)
    in
    match selection with
    | None | Some Omitted -> allowance.omitted_output_default
    | Some (Value (Tokens value)) -> Some value
    | Some (Explicit_null | Withheld | Value _) -> None
  ;;

  let declared_source t (policy : Policy.t) =
    match t.source with
    | None -> Error Assessment.Source_missing
    | Some source when Encoder.equal source.encoder policy.encoder -> Ok source
    | Some _ -> Error Assessment.Encoder_changed
  ;;

  let remaining ~hard ~tokens ~margin =
    let difference = Int64.(hard - tokens) in
    if Int64.(difference >= margin)
    then Assessment.Available Int64.(difference - margin)
    else if Int64.(difference >= zero)
    then Exceeded_by Int64.(margin - difference)
    else if Int64.(margin > max_value + difference)
    then Excess_out_of_range
    else Exceeded_by Int64.(margin - difference)
  ;;

  let pressure ~hard ~soft ~warn ~tokens ~margin =
    (* Compare without adding the estimate and margin, which may overflow. *)
    let at_least limit = Int64.(tokens >= limit || margin >= limit - tokens) in
    if Int64.(tokens > hard || margin > hard - tokens)
    then Assessment.Hard_limit_exceeded
    else if at_least soft
    then Checkpoint_due
    else if at_least warn
    then Near_limit
    else Below_warning
  ;;

  let evidence (source : Source.t) (policy : Policy.t) ~preparation_id ~declared =
    match source.read () with
    | Incomplete coverage -> Error (Assessment.Coverage_incomplete coverage)
    | Covered count ->
      (match Observation.Count.view count with
       | Unknown _ -> Error (Assessment.Coverage_incomplete Counter_unavailable)
       | Actual tokens -> Ok (Assessment.Measured tokens, tokens, 0L)
       | Estimated { tokens; estimator } ->
         (match policy.estimate with
          | Some estimate when Observation.Estimator.equal estimate.estimator estimator ->
            (match
               Observation.Context_estimate.create
                 ~preparation_id
                 ~count
                 ~capacity:(Declared declared)
             with
             | Ok value -> Ok (Assessment.Estimated value, tokens, estimate.margin)
             | Error _ -> Error (Assessment.Coverage_incomplete Counter_unavailable))
          | None | Some _ -> Error Assessment.Estimate_unqualified))
  ;;

  let assess_view t ~preparation_id (policy : Policy.t) =
    let open Result.Let_syntax in
    if not (Inference.Request.Target.equal t.target policy.target)
    then Error Assessment.Target_changed
    else (
      match policy.model_limit with
      | Undeclared -> Error Assessment.Limit_undeclared
      | Unbounded -> Error Assessment.Limit_unbounded
      | Bounded { total = declared; input = provider_input; output = provider_output } ->
        let%bind source = declared_source t policy in
        let allowance = policy.allowance in
        let total = Policy.allowed_total ~total:declared allowance in
        let%bind output =
          match output_allowance t allowance with
          | None -> Error Assessment.Output_unresolved
          | Some output ->
            if
              positive output
              && Int64.(output < total)
              && Option.for_all provider_output ~f:(fun limit -> Int64.(output <= limit))
            then Ok output
            else Error Assessment.Output_outside_limit
        in
        let%bind hard_input, soft_input, warn_input =
          input_limits
            ~total
            ~provider_input
            ~output
            ~checkpoint_reserve:allowance.checkpoint_reserve
            ~warning_lead:allowance.warning_lead
          |> Result.of_option ~error:Assessment.Reserves_exhaust_limit
        in
        let%map evidence, tokens, margin =
          evidence source policy ~preparation_id ~declared
        in
        Assessment.Assessed
          { evidence
          ; total
          ; output
          ; hard_input
          ; soft_input
          ; warn_input
          ; margin
          ; remaining = remaining ~hard:hard_input ~tokens ~margin
          ; pressure =
              pressure ~hard:hard_input ~soft:soft_input ~warn:warn_input ~tokens ~margin
          })
  ;;

  let assess ?policy t =
    let preparation_id = Observation.Configuration.preparation_id t.configuration in
    let view =
      match policy with
      | None -> Assessment.Unavailable Policy_missing
      | Some policy ->
        (match assess_view t ~preparation_id policy with
         | Ok view -> view
         | Error reason -> Unavailable reason)
    in
    { Assessment.preparation_id
    ; preparation_stamp = t.stamp
    ; policy_stamp = policy_stamp policy
    ; view
    }
  ;;
end
