(* Wire representation of local and descendant Goal usage. *)
open! Core
module J = Json_codec
module W = Goal_wire

module Usage = struct
  module Summary = Inference_query.Summary

  type t =
    { attempts : int64
    ; untracked_attempts : int64
    ; components : Summary.components
    }

  let create ~attempts ~untracked_attempts ~components =
    if Int64.(untracked_attempts < 0L || attempts < untracked_attempts)
    then W.invalid "invalid Goal usage attempt counts"
    else
      let open Core.Result.Let_syntax in
      let retained = Int64.(attempts - untracked_attempts) in
      let%map () =
        List.fold_result
          (Summary.component_metrics components)
          ~init:()
          ~f:(fun () metric ->
            let%bind observed = Inference_query.Metric.count metric in
            if Int64.equal observed retained
            then Ok ()
            else W.invalid "Goal usage metric covers a different attempt set")
      in
      { attempts; untracked_attempts; components }
  ;;

  let to_json t =
    `Object
      [ "attempts", W.counter_json t.attempts
      ; "untracked_attempts", W.counter_json t.untracked_attempts
      ; "components", Summary.components_to_json t.components
      ]
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind fields = W.fields json [ "attempts"; "untracked_attempts"; "components" ] in
    let%bind attempts = J.required_as fields "attempts" W.counter in
    let%bind untracked_attempts = J.required_as fields "untracked_attempts" W.counter in
    let%bind components = J.required_as fields "components" Summary.components_of_json in
    create ~attempts ~untracked_attempts ~components
  ;;
end

module Accounting = struct
  type counts =
    { turns : int64
    ; attempts : int64
    }

  type t =
    { revision : int64
    ; local : Usage.t
    ; physical : Usage.t
    ; causal : Usage.t
    ; physical_unknown : bool
    ; historical_causal_unknown : bool
    ; admitted : counts
    ; pending : counts
    }

  let validate t =
    if
      Int64.(
        t.revision < 0L
        || t.admitted.turns < 0L
        || t.admitted.attempts < 0L
        || t.pending.turns < 0L
        || t.pending.attempts < 0L)
    then W.invalid "negative Goal accounting counter"
    else Ok ()
  ;;

  let counts_to_json t =
    `Object [ "turns", W.counter_json t.turns; "attempts", W.counter_json t.attempts ]
  ;;

  let counts_of_json json =
    let open Core.Result.Let_syntax in
    let%bind fields = W.fields json [ "turns"; "attempts" ] in
    let%bind turns = J.required_as fields "turns" W.counter in
    let%map attempts = J.required_as fields "attempts" W.counter in
    { turns; attempts }
  ;;

  let to_json t =
    `Object
      [ "revision", W.counter_json t.revision
      ; "local", Usage.to_json t.local
      ; "physical", Usage.to_json t.physical
      ; "causal", Usage.to_json t.causal
      ; "physical_unknown", W.bool_json t.physical_unknown
      ; "historical_causal_unknown", W.bool_json t.historical_causal_unknown
      ; "admitted", counts_to_json t.admitted
      ; "pending", counts_to_json t.pending
      ]
  ;;

  let of_json json =
    let open Core.Result.Let_syntax in
    let%bind fields =
      W.fields
        json
        [ "revision"
        ; "local"
        ; "physical"
        ; "causal"
        ; "physical_unknown"
        ; "historical_causal_unknown"
        ; "admitted"
        ; "pending"
        ]
    in
    let%bind revision = J.required_as fields "revision" W.counter in
    let%bind local = J.required_as fields "local" Usage.of_json in
    let%bind physical = J.required_as fields "physical" Usage.of_json in
    let%bind causal = J.required_as fields "causal" Usage.of_json in
    let%bind physical_unknown = J.required_as fields "physical_unknown" J.bool in
    let%bind historical_causal_unknown =
      J.required_as fields "historical_causal_unknown" J.bool
    in
    let%bind admitted = J.required_as fields "admitted" counts_of_json in
    let%map pending = J.required_as fields "pending" counts_of_json in
    { revision
    ; local
    ; physical
    ; causal
    ; physical_unknown
    ; historical_causal_unknown
    ; admitted
    ; pending
    }
  ;;
end
