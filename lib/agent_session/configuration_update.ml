open! Core
module P = Agent_protocol
module R = Inference.Request

module Owner = struct
  type t = unit ref

  let create () = ref ()
end

type t =
  { owner : Owner.t
  ; policy : Configuration_policy.t
  ; request : P.Session_configuration.Update_request.t
  ; current : R.Target.t
  ; canonical : P.History.entry list
  ; deferred : P.History.entry list
  ; history : History_entry.t list
  }

module Validated = struct
  type basis = t

  type t =
    { basis : basis
    ; proposed : R.Target.t
    ; context : Inference_runtime.Context.t
    }

  let request t = t.basis.request
  let proposed t = t.proposed
end

let conflict () =
  Error
    (P.Error.create
       Conflict
       ~message:"configuration admission basis changed"
       ~retryable:false
       ())
;;

let create ~owner ~policy ~request ~(state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () =
    if
      (not
         (P.Id.Session.equal
            request.P.Session_configuration.Update_request.session_id
            state.identity.session_id))
      || (not (Int.equal request.expected_generation state.identity.generation))
      || not (Int64.equal request.expected_revision state.spec.configuration_revision)
    then conflict ()
    else if Int64.equal state.spec.configuration_revision Int64.max_value
    then Error (P.Error.invalid_request "configuration revision exhausted")
    else Ok ()
  in
  let%bind current = Configuration_transition.target state in
  let canonical = state.conversation.canonical_history in
  let deferred = state.conversation.deferred_user_entries in
  let%map history = History_codec.all_of_protocol (canonical @ deferred) in
  { owner; policy; request; current; canonical; deferred; history }
;;

let validate t =
  let open Result.Let_syntax in
  let%bind profile_target =
    match P.Session_configuration.Patch.profile t.request.patch with
    | None -> Ok None
    | Some profile ->
      Result.map (t.policy.select_profile ~current:t.current ~profile) ~f:Option.some
  in
  let%bind proposed =
    Configuration_transition.apply t.current ~patch:t.request.patch ~profile_target
  in
  let%map context =
    Configuration_policy.validate t.policy ~current:t.current ~proposed ~history:t.history
  in
  ({ basis = t; proposed; context } : Validated.t)
;;

let recheck t ~owner ~policy ~(state : Session_state.t) =
  let open Result.Let_syntax in
  let basis = t.Validated.basis in
  let request = basis.request in
  let%bind current = Configuration_transition.target state in
  let%bind () =
    if
      (not (phys_equal owner basis.owner))
      || (not (phys_equal policy basis.policy))
      || (not (P.Id.Session.equal request.session_id state.identity.session_id))
      || (not (Int.equal request.expected_generation state.identity.generation))
      || (not (Int64.equal request.expected_revision state.spec.configuration_revision))
      || (not (R.Target.equal basis.current current))
      || (not
            (List.equal
               P.History.equal_entry
               basis.canonical
               state.conversation.canonical_history))
      || not
           (List.equal
              P.History.equal_entry
              basis.deferred
              state.conversation.deferred_user_entries)
    then conflict ()
    else Ok ()
  in
  Inference_runtime.Context.preflight_history t.context basis.history
  |> Result.map_error ~f:(fun _ ->
    P.Error.create
      Permission_denied
      ~message:"validated configuration binding is no longer current"
      ~retryable:false
      ())
;;
