open! Core
module R = Inference_runtime
module Capacity = R.Context_capacity
module Mode = Session_context_capacity.Mode

type t =
  { policy : Capacity.Policy.t
  ; mode : Mode.t
  }

let of_selection selection =
  Option.map (Session_context_capacity.Selection.policy selection) ~f:(fun policy ->
    { policy; mode = (Session_context_capacity.availability selection).effective })
;;

let revision t =
  let policy = Capacity.Policy.revision t.policy |> Chatml_source.Digest.to_hex in
  let mode =
    match t.mode with
    | Manual -> "manual"
    | Automatic -> "automatic"
  in
  Chatml_source.Digest.of_contents
    ("ochat.near-limit.v1:" ^ mode ^ ":" ^ policy)
    ~checkpoint:ignore
;;

let pressure t prepared =
  let assessment = R.Prepared.assess_capacity ~policy:t.policy prepared in
  if
    R.Prepared.binding_guard prepared ()
    && R.Prepared.capacity_assessment_is_current ~policy:t.policy prepared assessment
  then (
    match Capacity.Assessment.view assessment with
    | Assessed { pressure; _ } -> Some pressure
    | Unavailable _ -> None)
  else None
;;

let warns t prepared =
  match t.mode, pressure t prepared with
  | _, Some Near_limit | Manual, Some Checkpoint_due -> true
  | _ -> false
;;

let fits t prepared =
  match t.mode, pressure t prepared with
  | _, Some (Below_warning | Near_limit) | Manual, Some Checkpoint_due -> true
  | _ -> false
;;

let text =
  "This request is nearing its configured context limit. Keep subsequent steps \
   focused, and preserve essential decisions if your instructions and available \
   recovery tools allow."
;;

let with_reminder request =
  let open Option.Let_syntax in
  let module Guidance = Inference.Request.Guidance in
  let previous = Inference.Request.guidance request in
  let text =
    Option.value_map previous ~default:text ~f:(fun prior ->
      Guidance.text prior ^ "\n\n" ^ text)
  in
  let limits = Transcript.Admission.default in
  let%bind guidance =
    Guidance.create ~text ~provenance:"host recovery capacity reminder" ~limits
    |> Result.ok
  in
  Inference.Request.with_guidance request ~guidance:(Some guidance) ~limits |> Result.ok
;;
