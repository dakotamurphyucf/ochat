open! Core
module P = Agent_protocol
module Session_capacity = Agent_session.Session_context_capacity
module Capacity = Inference_runtime.Context_capacity

module Target = struct
  type t =
    { prompt : string
    ; profile : string
    ; model : string
    ; encoder : Capacity.Encoder.t
    ; total : int64
    ; input : int64 option
    ; output : int64 option
    ; session_total : int64 option
    ; omitted_output_default : int64 option
    ; checkpoint_reserve : int64
    ; warning_lead : int64
    ; estimate_margin : int64
    }
  [@@deriving compare, equal, sexp]

  let model_limit (t : t) =
    Capacity.Policy.Bounded { total = t.total; input = t.input; output = t.output }
  ;;

  let allowance (t : t) ~host_total =
    Capacity.Policy.
      { host_total = Some host_total
      ; session_total = t.session_total
      ; omitted_output_default = t.omitted_output_default
      ; checkpoint_reserve = t.checkpoint_reserve
      ; warning_lead = t.warning_lead
      }
  ;;

  let estimate (t : t) =
    Capacity.Policy.
      { estimator = Openai.Inference_adapter.capacity_estimator
      ; margin = t.estimate_margin
      }
  ;;

  let validate t ~host_total =
    let valid label =
      String.length label > 0
      && String.length label <= 128
      && Stdlib.String.is_valid_utf_8 label
      && String.for_all label ~f:(fun char ->
        let code = Char.to_int char in
        code > 0x20 && not (Int.equal code 0x7f))
    in
    if not (List.for_all [ t.prompt; t.profile; t.model ] ~f:valid)
    then
      Error
        (P.Error.invalid_request
           "recovery capacity target labels must be bounded identifiers")
    else
      Capacity.Policy.validate
        ~model_limit:(model_limit t)
        ~allowance:(allowance t ~host_total)
        ~estimate:(Some (estimate t))
      |> Result.map_error ~f:(function
        | Invalid_limits ->
          P.Error.invalid_request "recovery capacity token limits are invalid"
        | Invalid_reservations ->
          P.Error.invalid_request
            "recovery capacity output, checkpoint or warning reserves are invalid")
  ;;
end

type t =
  { mode : Session_capacity.Mode.t
  ; host_total : int64
  ; targets : Target.t list
  }
[@@deriving compare, equal, sexp]

let unchecked_t_of_sexp = t_of_sexp
let max_targets = 32

let create ~mode ~host_total ~targets =
  let open Result.Let_syntax in
  if Int64.(host_total <= zero)
  then Error (P.Error.invalid_request "recovery capacity host_total must be positive")
  else if Stdlib.List.compare_length_with targets max_targets > 0
  then Error (P.Error.invalid_request "too many recovery capacity target qualifications")
  else (
    match
      List.find_a_dup targets ~compare:(fun (a : Target.t) (b : Target.t) ->
        String.compare a.prompt b.prompt)
    with
    | Some _ ->
      Error (P.Error.invalid_request "duplicate recovery capacity prompt qualification")
    | None ->
      let%map () =
        List.fold_result targets ~init:() ~f:(fun () target ->
          Target.validate target ~host_total)
      in
      { mode; host_total; targets })
;;

let t_of_sexp sexp =
  let raw = unchecked_t_of_sexp sexp in
  match create ~mode:raw.mode ~host_total:raw.host_total ~targets:raw.targets with
  | Ok config -> config
  | Error error -> Sexplib.Conv.of_sexp_error error.P.Error.message sexp
;;
