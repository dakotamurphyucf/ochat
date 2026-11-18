open! Core
module M = Chat_response.Moderator_manager
module S = Session.Moderator_state.Identity_snapshot

type t =
  { index : Run_state.t
  ; source : Agent_protocol.Run_source.t
  ; moderator : Jsonaf.t
  }

let conflict message =
  Error (Agent_protocol.Error.create Conflict ~message ~retryable:false ())
;;

let prepare (state : Session_state.t) ~manager ~callback =
  let open Result.Let_syntax in
  let snapshot = M.Callback.snapshot callback in
  let%bind () =
    if
      M.callback_is_current manager callback
      && Option.is_none (M.invocation_observer manager)
    then Ok ()
    else conflict "constructor callback requires its actual legacy Manager"
  in
  (* Callback.snapshot was captured by this exact Manager under its execution
     gate. Do not reacquire that gate from the actor while admission holds it. *)
  let%bind () = Moderator_checkpoint.validate_encoded_capacity state.moderator in
  let%bind previous = Moderator_checkpoint.decode state.moderator in
  let%bind initial =
    match previous with
    | None -> Ok (Some (Runtime_builder.encode_moderator_snapshot snapshot))
    | Some previous ->
      if Sexp.equal (S.sexp_of_t previous) (S.sexp_of_t snapshot)
      then Ok state.moderator
      else conflict "constructor callback differs from the original checkpoint"
  in
  let%bind index, source =
    Moderator_source_installation.capture { state with moderator = initial }
  in
  let%bind metadata = Moderator_checkpoint.queue initial in
  let%bind metadata =
    match metadata with
    | None -> Moderator_queue.bootstrap ~source ~queue:snapshot.queued_internal_events
    | Some metadata ->
      let%bind () =
        Moderator_queue.validate_queue metadata snapshot.queued_internal_events
      in
      if Agent_protocol.Run_source.equal (Moderator_queue.source metadata) source
      then Ok metadata
      else conflict "constructor queue differs from its installed source"
  in
  let%map moderator =
    Moderator_checkpoint.update_with_queue initial ~snapshot ~queue:metadata
  in
  { index; source; moderator }
;;
