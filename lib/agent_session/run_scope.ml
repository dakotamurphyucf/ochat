open! Core
module P = Agent_protocol

type t =
  { run_id : P.Id.Run.t
  ; principal_id : P.Id.Principal.t
  ; source : P.Run_source.t
  ; revision : int64
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
