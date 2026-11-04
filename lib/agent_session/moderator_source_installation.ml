open! Core
module P = Agent_protocol

let capture (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind observer = Moderator_checkpoint.observer state.moderator in
  let%bind observer =
    Result.of_option
      observer
      ~error:
        (P.Error.create
           Invalid_state
           ~message:"moderator capture requires an actually installed source"
           ~retryable:false
           ())
  in
  let index = Option.value state.run_state ~default:Run_state.empty in
  let installed = Run_state.installation index in
  let%bind index =
    match installed.source with
    | Some current ->
      if P.Invocation.equal_observer current observer
      then Ok index
      else
        Error
          (P.Error.create
             Conflict
             ~message:"moderator source differs from actor installation"
             ~retryable:false
             ())
    | None ->
      let%bind installation =
        Run_source_installation.apply installed ~change:(Replace observer)
      in
      Run_state.replace_installation index ~installation ~retired_runs:[]
  in
  let%map source =
    Run_source_installation.captured
      (Run_state.installation index)
      ~generation:state.identity.generation
  in
  index, source
;;
