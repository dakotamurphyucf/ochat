open! Core
module Event = Tool_execution_event

let notify observer event =
  match observer with
  | None -> ()
  | Some observer ->
    (try observer event with
     | _ -> ())
;;

let deliver observer execution_event event =
  notify observer event;
  Option.iter execution_event ~f:(fun observer -> observer event)
;;

let invocation observer execution_event ~call_id =
  match observer, execution_event with
  | None, None -> Ochat_function.Invocation.silent
  | (None | Some _), (None | Some _) ->
    let create =
      match execution_event with
      | None -> Ochat_function.Invocation.create_with_trace
      | Some _ -> Ochat_function.Invocation.create_strict_with_trace
    in
    create
      ~progress:(fun progress ->
        deliver observer execution_event (Event.Progress { call_id; progress }))
      ~trace:(fun trace ->
        deliver observer execution_event (Event.Trace { call_id; trace }))
;;

let run ~kind ~call_id ~name ~payload ~runner ?on_tool_execution ?on_execution_event () =
  deliver
    on_tool_execution
    on_execution_event
    (Event.Started { call_id; name; kind; payload });
  let invocation = invocation on_tool_execution on_execution_event ~call_id in
  let terminal outcome output =
    deliver
      on_tool_execution
      on_execution_event
      (Event.Finished { call_id; outcome; output })
  in
  match runner ~invocation payload with
  | result ->
    terminal Returned (Some result);
    result
  | exception exn ->
    let backtrace = Stdlib.Printexc.get_raw_backtrace () in
    let reraised () = Stdlib.Printexc.raise_with_backtrace exn backtrace in
    (match exn with
     | Eio.Cancel.Cancelled _ ->
       (* Strict delivery is not called during cancellation cleanup. The owning
          operation terminal fences the live view; the original cancellation
          and backtrace cannot be replaced by another presentation failure. *)
       Eio.Cancel.protect (fun () ->
         notify
           on_tool_execution
           (Event.Finished { call_id; outcome = Cancelled; output = None }));
       reraised ()
     | _ -> Exn.protect ~f:reraised ~finally:(fun () -> terminal Raised None))
;;
