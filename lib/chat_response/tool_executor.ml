module Event = Tool_execution_event

let notify observer event =
  match observer with
  | None -> ()
  | Some observer ->
    (try observer event with
     | _ -> ())
;;

let invocation observer ~call_id =
  match observer with
  | None -> Ochat_function.Invocation.silent
  | Some _ ->
    Ochat_function.Invocation.create_with_trace
      ~progress:(fun progress -> notify observer (Event.Progress { call_id; progress }))
      ~trace:(fun trace -> notify observer (Event.Trace { call_id; trace }))
;;

let notify_finished observer ~call_id outcome output =
  notify observer (Event.Finished { call_id; outcome; output })
;;

let run ~kind ~call_id ~name ~payload ~runner ?on_tool_execution () =
  notify on_tool_execution (Event.Started { call_id; name; kind; payload });
  let invocation = invocation on_tool_execution ~call_id in
  match runner ~invocation payload with
  | result ->
    notify_finished on_tool_execution ~call_id Returned (Some result);
    result
  | exception (Eio.Cancel.Cancelled _ as exn) ->
    Eio.Cancel.protect (fun () ->
      notify_finished on_tool_execution ~call_id Cancelled None);
    raise exn
  | exception exn ->
    notify_finished on_tool_execution ~call_id Raised None;
    raise exn
;;
