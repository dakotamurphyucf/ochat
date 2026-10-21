open! Core

let connect ~sw ~net ~socket_path ~max_line_length ~notification_capacity =
  if max_line_length <= 0 || notification_capacity <= 0
  then invalid_arg "invalid socket client limits";
  let flow =
    (Eio.Net.connect ~sw net (`Unix socket_path)
      :> [ Eio.Net.socket_ty | Eio.Flow.two_way_ty | `Stream ] Eio.Resource.t)
  in
  let close_flows () =
    Eio.Cancel.protect (fun () ->
      ignore
        (Result.try_with (fun () -> Eio.Flow.shutdown flow `All) : (unit, exn) result);
      Eio.Flow.close flow)
  in
  match
    Agent_transport_ndjson.Client.create
      ~sw
      ~transport_name:"socket"
      ~input:flow
      ~output:flow
      ~close_flows
      ~max_line_length
      ~notification_capacity
  with
  | Ok owner -> Agent_transport_ndjson.Client.connection owner
  | Error _ ->
    close_flows ();
    invalid_arg "invalid socket client limits"
;;
