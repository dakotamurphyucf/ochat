open! Core
module P = Agent_protocol

module Suspension = struct
  type t =
    { goal_id : P.Id.Goal.t
    ; hold : P.Goal.Hold.t
    }
end

type t =
  | Eligible
  | Suspended_goal of Suspension.t

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let of_run (state : Session_state.t) (run : P.Run.t) =
  let open Result.Let_syntax in
  let%bind index =
    Result.of_option
      state.run_state
      ~error:(P.Error.invalid_request "selected execution Run index is absent")
  in
  let%bind () =
    if Option.exists (Run_state.find index run.id) ~f:(P.Run.equal run)
    then Ok ()
    else conflict "selected execution Run is no longer current"
  in
  let%bind attribution = Goal_work_attribution.of_run state run in
  match attribution with
  | None -> Ok Eligible
  | Some attribution ->
    let goal_id = Goal_work_attribution.goal_id attribution in
    let%bind goal =
      Option.bind state.goal_catalog ~f:(fun catalog -> Goal_catalog.find catalog goal_id)
      |> Result.of_option ~error:(P.Error.invalid_request "selected Goal is absent")
    in
    let%bind () =
      if
        P.Goal_episode.matches_run (Goal_work_attribution.episode attribution) run
        && P.Id.Session.equal
             (P.Session_ref.session_id run.session)
             state.identity.session_id
        && Int.equal run.source.generation state.identity.generation
      then Ok ()
      else conflict "selected Goal Run or episode is no longer current"
    in
    let%bind captured_index, source = Moderator_source_installation.capture state in
    let%bind () =
      if Run_state.equal index captured_index && P.Run_source.equal source run.source
      then Ok ()
      else conflict "selected Goal Run source installation changed"
    in
    (match P.Goal.state goal with
     | Open Enabled -> Ok Eligible
     | Open (Held hold) -> Ok (Suspended_goal { goal_id; hold })
     | Closed _ ->
       Error
         (P.Error.create
            Permission_denied
            ~message:"selected execution belongs to a closed Goal"
            ~retryable:false
            ()))
;;
