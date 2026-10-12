open Core

type t =
  { implementation : Ochat_function.t
  ; prepare :
      owner:Agent_protocol.Job.launch_owner
      -> request:Background_request.t
      -> checkpoint:(unit -> unit)
      -> (Background_request.Agent_artifact.t option, Agent_protocol.Error.t) result
  }

let create ~implementation ~prepare = { implementation; prepare }

let prepare t ~binding ~owner ~request ~checkpoint =
  match Tool_capability.native_implementation binding with
  | Some implementation when phys_equal implementation t.implementation ->
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ();
    let result = t.prepare ~owner ~request ~checkpoint in
    Eio.Fiber.check ();
    checkpoint ();
    Eio.Fiber.check ();
    result
  | Some _ | None ->
    Error
      (Agent_protocol.Error.invalid_request
         "source preparer belongs to another native implementation")
;;
