open! Core
module P = Agent_protocol

type t =
  { session : P.Id.Session.t
  ; generation : int
  ; stop_epoch : int64
  ; spec : Session_state.Spec.Resource_identity.t
  ; source : P.Run_source.t option
  }

let source (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind observer = Moderator_checkpoint.observer state.moderator in
  match observer with
  | None -> Ok None
  | Some _ ->
    Moderator_source_installation.capture state
    |> Result.map ~f:(fun (_, source) -> Some source)
;;

let eligible (state : Session_state.t) =
  match state.spec.delegation, state.runtime_initialization with
  | None, Ready -> Ok ()
  | Some _, (Ready | Pending _) | None, Pending _ ->
    Error
      (P.Error.create
         Conflict
         ~message:"ordinary attachment requires its ready root source"
         ~retryable:true
         ())
;;

let capture (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () = Session_state.validate state in
  let%bind () = eligible state in
  let%map source = source state in
  { session = state.identity.session_id
  ; generation = state.identity.generation
  ; stop_epoch = state.stop_epoch
  ; spec = Session_state.Spec.Resource_identity.capture state.spec
  ; source
  }
;;

let check t (state : Session_state.t) =
  let open Result.Let_syntax in
  let%bind () = eligible state in
  let%bind source = source state in
  if
    P.Id.Session.equal t.session state.identity.session_id
    && Int.equal t.generation state.identity.generation
    && Int64.equal t.stop_epoch state.stop_epoch
    && Session_state.Spec.Resource_identity.equal
         t.spec
         (Session_state.Spec.Resource_identity.capture state.spec)
    && Option.equal P.Run_source.equal t.source source
  then Ok ()
  else
    Error
      (P.Error.create
         Conflict
         ~message:"ordinary retained attachment source or Stop basis changed"
         ~retryable:true
         ())
;;
