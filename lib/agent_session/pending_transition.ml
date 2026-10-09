open! Core
module P = Agent_protocol

type t =
  { delta : Session_delta.t
  ; plan : Pending_plan.t
  ; expiry_archive : Pending_archive.t option
  }

let prepare state ~change ~retention ~archive ~limits =
  let open Result.Let_syntax in
  let mutation = Pending_mutation.create state ~change ~retention in
  let%bind plan = Pending_mutation.prepare mutation state ~limits in
  let%map expiry_archive =
    match Pending_plan.expired_dispositions plan with
    | [] -> Ok None
    | records ->
      Pending_archive.create
        state
        ~operation_id:(P.Id.Operation.create ())
        ~pending_revision:(Pending_plan.revision plan)
        ~records
        ~limits
      |> Result.map ~f:Option.some
  in
  { plan
  ; expiry_archive
  ; delta =
      Session_delta.Pending_inputs_changed
        (mutation, archive, Option.map expiry_archive ~f:Pending_archive.reference)
  }
;;

let delta t = t.delta
let plan t = t.plan
let expiry_archive t = t.expiry_archive
