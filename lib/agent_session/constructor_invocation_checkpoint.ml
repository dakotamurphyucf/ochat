open! Core
module I = Agent_protocol.Invocation

let validate
      ~(previous : Session_state.t)
      ~(incoming : Session_state.t)
      ~before
      ~(after : I.t)
  =
  let open Result.Let_syntax in
  let selected (invocation : I.t) =
    Agent_protocol.Id.Invocation.equal invocation.context.id after.context.id
  in
  let previous_row, previous_rest = List.partition_tf previous.invocations ~f:selected in
  let incoming_row, incoming_rest = List.partition_tf incoming.invocations ~f:selected in
  let%bind () = I.validate after in
  if
    (match before, previous_row with
     | None, [] -> true
     | Some expected, [ actual ] -> I.equal expected actual
     | None, _ :: _ | Some _, ([] | _ :: _ :: _) -> false)
    && List.equal I.equal incoming_row [ after ]
    && List.equal I.equal previous_rest incoming_rest
  then Ok ()
  else
    Error
      (Agent_protocol.Error.create
         Conflict
         ~message:"constructor Invocation ACK changed another retained invocation"
         ~retryable:false
         ())
;;
