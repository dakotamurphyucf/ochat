open! Core
module P = Agent_protocol

type t =
  { run_id : P.Id.Run.t
  ; principal_id : P.Id.Principal.t
  ; source : P.Run_source.t
  ; mutable revision : int64
  ; execution_id : P.Id.Moderator_execution.t
  ; mutable is_open : bool
  }

let create ~(run : P.Run.t) ~execution_id =
  let open Result.Let_syntax in
  let%map execution_id =
    P.Id.Moderator_execution.of_json (P.Id.Moderator_execution.to_json execution_id)
  in
  { run_id = run.id
  ; principal_id = run.principal_id
  ; source = run.source
  ; revision = run.revision
  ; execution_id
  ; is_open = true
  }
;;

let run_id t = t.run_id
let principal_id t = t.principal_id
let source t = t.source
let revision t = t.revision
let execution_id t = t.execution_id
let close t = t.is_open <- false
let is_open t = t.is_open

module Host = struct
  type scope = t

  type advance =
    { scope : scope
    ; previous_revision : int64
    ; next_revision : int64
    ; mutable acknowledged : bool
    }

  let prepare_advance scope ~previous ~next ~execution_id =
    let open Result.Let_syntax in
    let%bind () = P.Run.validate_transition ~previous next in
    let%bind () =
      match next.P.Run.lifecycle with
      | Admitted | Active | Waiting _ -> Ok ()
      | Terminal _ ->
        Error (P.Error.invalid_request "a terminal Run cannot extend callback custody")
    in
    if
      scope.is_open
      && Int64.equal scope.revision previous.P.Run.revision
      && P.Id.Run.equal scope.run_id previous.id
      && P.Id.Principal.equal scope.principal_id previous.principal_id
      && P.Run_source.equal scope.source previous.source
      && P.Id.Moderator_execution.equal scope.execution_id execution_id
    then
      Ok
        { scope
        ; previous_revision = previous.revision
        ; next_revision = next.revision
        ; acknowledged = false
        }
    else
      Error (P.Error.invalid_request "callback basis changed before immediate admission")
  ;;

  let acknowledge_advance t =
    if
      t.acknowledged
      || (not t.scope.is_open)
      || not (Int64.equal t.scope.revision t.previous_revision)
    then
      Error
        (P.Error.invalid_request "callback basis advance expired or already acknowledged")
    else (
      t.acknowledged <- true;
      t.scope.revision <- t.next_revision;
      Ok ())
  ;;
end
