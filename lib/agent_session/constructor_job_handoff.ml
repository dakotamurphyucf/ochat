open! Core
module M = Chat_response.Moderator_manager
module S = Session.Moderator_state.Identity_snapshot

let conflict message =
  Error (Agent_protocol.Error.create Conflict ~message ~retryable:false ())
;;

let prepare (state : Session_state.t) ~manager =
  let open Result.Let_syntax in
  let%bind () = Moderator_checkpoint.validate_encoded_capacity state.moderator in
  let%bind snapshot = Moderator_checkpoint.decode state.moderator in
  let%bind () =
    match snapshot, manager with
    | None, None | Some _, None -> Ok ()
    | None, Some _ -> conflict "constructor Manager has no admitted checkpoint"
    | Some expected, Some manager ->
      let%bind actual =
        M.identity_snapshot manager
        |> Result.map_error ~f:Agent_protocol.Error.invalid_request
      in
      if Sexp.equal (S.sexp_of_t expected) (S.sexp_of_t actual)
      then Ok ()
      else conflict "constructor Manager differs from its actual checkpoint"
  in
  match snapshot with
  | None -> Ok (state.run_state, Job_custody.Moderator_binding.No_captured_moderator)
  | Some _ ->
    let%map index, source = Moderator_source_installation.capture state in
    Some index, Job_custody.Moderator_binding.Captured { source; manager }
;;
