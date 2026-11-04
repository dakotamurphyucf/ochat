open! Core
module Budget = Goal_budget
module Reason = Goal_text.Reason
module Achievement = Goal_achievement
module J = Json_codec

let invalid text = Error (Protocol_error.invalid_request text)

module Hold = struct
  type t =
    | Operator_pause
    | Needs_input of Reason.t
    | Budget_gate of Budget.Dimension.t
    | Usage_uncertain of Inference.Observation.Usage.Component.t
    | Deadline_reached
    | Source_review
  [@@deriving equal, sexp_of]
end

module Closure = struct
  type t =
    | Achieved of Achievement.t
    | Abandoned of Reason.t
  [@@deriving equal, sexp_of]
end

type admission =
  | Enabled
  | Held of Hold.t
[@@deriving equal, sexp_of]

type t =
  | Open of admission
  | Closed of Closure.t
[@@deriving equal, sexp_of]

let tag kind fields = `Object (("kind", `String kind) :: fields)

let component_json component =
  let open Inference.Observation.Usage.Component in
  `String
    (match component with
     | Input -> "input"
     | Output -> "output"
     | Reported_total -> "reported_total"
     | Cached_input -> "cached_input"
     | Cache_write_input -> "cache_write_input"
     | Reasoning_output -> "reasoning_output")
;;

let component_decode =
  J.enum
    ~name:"goal usage component"
    [ "input", Inference.Observation.Usage.Component.Input
    ; "output", Output
    ; "reported_total", Reported_total
    ; "cached_input", Cached_input
    ; "cache_write_input", Cache_write_input
    ; "reasoning_output", Reasoning_output
    ]
;;

let hold_json = function
  | Hold.Operator_pause -> tag "operator_pause" []
  | Needs_input reason -> tag "needs_input" [ "reason", Reason.to_json reason ]
  | Budget_gate dimension ->
    tag
      "budget_gate"
      [ ( "dimension"
        , `String
            (match dimension with
             | Budget.Dimension.Admitted_turns -> "admitted_turns"
             | Admitted_attempts -> "admitted_attempts"
             | Observed_tokens -> "observed_tokens") )
      ]
  | Usage_uncertain component ->
    tag "usage_uncertain" [ "component", component_json component ]
  | Deadline_reached -> tag "deadline_reached" []
  | Source_review -> tag "source_review" []
;;

let hold_decode json =
  let open Result.Let_syntax in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "kind" J.string in
  match kind with
  | "operator_pause" -> Ok Hold.Operator_pause
  | "source_review" -> Ok Source_review
  | "deadline_reached" -> Ok Deadline_reached
  | "needs_input" ->
    Result.map (J.required_as f "reason" Reason.of_json) ~f:(fun x -> Hold.Needs_input x)
  | "budget_gate" ->
    Result.map
      (J.required_as
         f
         "dimension"
         (J.enum
            ~name:"goal budget dimension"
            [ "admitted_turns", Budget.Dimension.Admitted_turns
            ; "admitted_attempts", Admitted_attempts
            ; "observed_tokens", Observed_tokens
            ]))
      ~f:(fun x -> Hold.Budget_gate x)
  | "usage_uncertain" ->
    Result.map (J.required_as f "component" component_decode) ~f:(fun x ->
      Hold.Usage_uncertain x)
  | _ -> invalid "unknown goal hold kind"
;;

let to_json = function
  | Open Enabled -> tag "open" [ "admission", tag "enabled" [] ]
  | Open (Held reason) ->
    tag "open" [ "admission", tag "held" [ "reason", hold_json reason ] ]
  | Closed (Achieved achievement) ->
    tag "achieved" [ "achievement", Achievement.to_json achievement ]
  | Closed (Abandoned reason) -> tag "abandoned" [ "reason", Reason.to_json reason ]
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind () = Extension_codec.validate_json ~max_depth:64 json in
  let%bind f = J.fields json in
  let%bind kind = J.required_as f "kind" J.string in
  match kind with
  | "open" ->
    let%bind admission =
      J.required_as f "admission" (fun json ->
        let%bind f = J.fields json in
        let%bind kind = J.required_as f "kind" J.string in
        match kind with
        | "enabled" -> Ok Enabled
        | "held" -> Result.map (J.required_as f "reason" hold_decode) ~f:(fun x -> Held x)
        | _ -> invalid "unknown goal admission policy")
    in
    Ok (Open admission)
  | "achieved" ->
    Result.map (J.required_as f "achievement" Achievement.of_json) ~f:(fun x ->
      Closed (Achieved x))
  | "abandoned" ->
    Result.map (J.required_as f "reason" Reason.of_json) ~f:(fun x ->
      Closed (Abandoned x))
  | _ -> invalid "unknown goal objective disposition"
;;
