open! Core
module R = Openai.Responses
module S = Chat_response.In_memory_stream
module P = History_entry.Payload
module T = Transcript

let ok = Result.ok_or_failwith

let selected_run ~env ~post_stream =
  let fixture =
    Inference_fixture.create
      ~namespace:"streaming-offline"
      ~default_model:"fixture"
      ~post_stream
  in
  let target =
    Inference_fixture.capture_config fixture Chat_response.Config.default
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let context =
    Inference_fixture.resolve fixture target
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  S.run_completion_stream_in_memory_entries
    ~env
    ~inference_context:context
    ~inference_identity:(Inference_fixture.identity fixture)
    ~on_inference_attempt:ignore
    ~on_inference_completion:ignore
;;

let call =
  R.Function_call.
    { name = "echo"
    ; arguments = {|{"value":"original"}|}
    ; call_id = "call"
    ; _type = "function_call"
    ; id = Some "provider-call"
    ; status = None
    }
;;

let done_ item =
  R.Response_stream.Output_item_done
    { item; output_index = 0; type_ = "response.output_item.done" }
;;

let call_events (call : R.Function_call.t) =
  [ R.Response_stream.Output_item_added
      { item = Function_call { call with arguments = "" }
      ; output_index = 0
      ; type_ = "response.output_item.added"
      }
  ; Function_call_arguments_done
      { item_id = Option.value call.id ~default:call.call_id
      ; output_index = 0
      ; arguments = call.arguments
      ; type_ = "response.function_call_arguments.done"
      }
  ; done_ (Function_call call)
  ]
;;

let message text =
  R.Response_stream.Item.Output_message
    { role = Assistant
    ; id = "provider-message"
    ; content = [ { annotations = []; text; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
;;

let allocator () =
  History_entry.Allocator.create ~namespace:"transcript-producer" ~next_sequence:0 |> ok
;;

let exact_entry left right =
  History_entry.Id.equal (History_entry.id left) (History_entry.id right)
  && Jsonaf.exactly_equal
       (P.to_json (History_entry.payload left))
       (P.to_json (History_entry.payload right))
;;

let post responses ~sw:_ ~inputs:_ = Queue.dequeue_exn responses |> Stdlib.List.to_seq

let table () =
  let table = Hashtbl.create (module String) in
  Hashtbl.set table ~key:"echo" ~data:(fun ~invocation:_ input ->
    R.Tool_output.Output.Text input);
  table
;;

let service ~commit_call ~commit_output ?prepare_call () : S.Tool_dispatch.t =
  { for_fork = None
  ; commit_call
  ; prepare_call
  ; validate_original = (fun ~kind:_ ~name:_ ~payload:_ -> Ok ())
  ; run =
      (fun ?run_native:_ request ~authorize ->
        authorize ();
        Some
          { output = R.Tool_output.Output.Text request.payload
          ; commit_output = Some commit_output
          ; runtime_requests = []
          })
  }
;;

let run env ~on_history_item_appended ~on_transcript_event ?dispatch_tool responses =
  selected_run
    ~env
    ~datadir:(Eio.Stdenv.cwd env)
    ~allocator:(allocator ())
    ~history:[]
    ~tools:(Some [])
    ~tool_tbl:(table ())
    ~on_history_item_appended
    ~on_transcript_event
    ?dispatch_tool
    ~parallel_tool_calls:false
    ~post_stream:(post responses)
    ()
;;

let%test_unit "all three owner commit seams precede exact finalization" =
  Eio_main.run (fun env ->
    let committed = Queue.create () in
    let finalized = Queue.create () in
    let events = Queue.create () in
    let commit = Queue.enqueue committed in
    let dispatch =
      service
        ~commit_call:(fun request ->
          commit request.call;
          true)
        ~commit_output:commit
        ()
    in
    let responses = Queue.of_list [ call_events call; [ done_ (message "done") ] ] in
    let history =
      run
        env
        responses
        ~dispatch_tool:dispatch
        ~on_history_item_appended:commit
        ~on_transcript_event:(fun event ->
          Queue.enqueue events event;
          match T.Stream.view event with
          | Item_finalized { entry; _ } ->
            assert (Queue.exists committed ~f:(exact_entry entry));
            Queue.enqueue finalized entry
          | _ -> ())
    in
    assert (List.equal exact_entry history (Queue.to_list committed));
    assert (List.equal exact_entry history (Queue.to_list finalized));
    assert (List.length history = 3);
    let starts =
      Queue.to_list events
      |> List.filter_map ~f:(fun event ->
        match T.Stream.view event with
        | Source_started { scope; _ } -> Some scope
        | _ -> None)
    in
    assert (List.length starts = 2);
    let first = List.nth_exn starts 0
    and second = List.nth_exn starts 1 in
    assert (T.Source_id.equal first.key.source second.key.source);
    assert (not (T.Attempt_id.equal first.key.attempt second.key.attempt));
    let first_terminal =
      Queue.to_list events
      |> List.find_exn ~f:(fun event ->
        match T.Stream.view event with
        | Source_finished { scope; _ } -> T.Scope.Key.equal scope.key first.key
        | _ -> false)
    in
    let before_terminal =
      Queue.to_list events
      |> List.take_while ~f:(fun event ->
        not
          (Jsonaf.exactly_equal
             (T.Stream.to_json event)
             (T.Stream.to_json first_terminal)))
    in
    assert (
      List.count before_terminal ~f:(fun event ->
        match T.Stream.view event with
        | Item_finalized _ -> true
        | _ -> false)
      = 2))
;;

exception Commit_failed

let%test_unit "failed owning commits never finalize rejected entries" =
  List.iter [ "message"; "call"; "output" ] ~f:(fun seam ->
    Eio_main.run (fun env ->
      let finalized = Queue.create () in
      let commit entry =
        let actual =
          match P.Semantic.view (P.semantic (History_entry.payload entry)) with
          | Message _ -> "message"
          | Call _ -> "call"
          | Result _ -> "output"
          | Reasoning _ | Unknown _ -> "other"
        in
        if String.equal actual seam then raise Commit_failed
      in
      let dispatch =
        service
          ~commit_call:(fun request ->
            commit request.call;
            true)
          ~commit_output:commit
          ()
      in
      let responses =
        if String.equal seam "message"
        then [ [ done_ (message "rejected") ] ]
        else [ call_events call; [ done_ (message "done") ] ]
      in
      (match
         run
           env
           (Queue.of_list responses)
           ~dispatch_tool:dispatch
           ~on_history_item_appended:commit
           ~on_transcript_event:(fun event ->
             match T.Stream.view event with
             | Item_finalized { entry; _ } -> Queue.enqueue finalized entry
             | _ -> ())
       with
       | _ -> assert false
       | exception Commit_failed -> ());
      assert (Queue.length finalized = if String.equal seam "output" then 1 else 0)))
;;

let%test_unit
    "prepared call payload is the sole finalized call and observer failures propagate"
  =
  Eio_main.run (fun env ->
    let final_calls = Queue.create () in
    let dispatch =
      service
        ~commit_call:(fun _ -> true)
        ~commit_output:ignore
        ~prepare_call:(fun _ ->
          Ok
            (Some
               (Chat_response.Moderation.Tool_moderation.Redirect
                  ("redirected", `Object [ "value", `String "edited" ]))))
        ()
    in
    let responses = Queue.of_list [ call_events call; [ done_ (message "done") ] ] in
    ignore
      (run
         env
         responses
         ~dispatch_tool:dispatch
         ~on_history_item_appended:ignore
         ~on_transcript_event:(fun event ->
           match T.Stream.view event with
           | Item_finalized { entry; _ } ->
             (match P.Semantic.view (P.semantic (History_entry.payload entry)) with
              | Call { name; input_bytes; _ } ->
                Queue.enqueue final_calls (name, input_bytes)
              | Message _ | Result _ | Reasoning _ | Unknown _ -> ())
           | _ -> ())
       : History_entry.t list);
    assert (
      List.equal
        (fun (name, input) (other_name, other_input) ->
           String.equal name other_name && String.equal input other_input)
        (Queue.to_list final_calls)
        [ "redirected", {|{"value":"edited"}|} ]);
    let committed = ref false in
    match
      run
        env
        (Queue.of_list [ [ done_ (message "done") ] ])
        ~on_history_item_appended:(fun _ -> committed := true)
        ~on_transcript_event:(fun event ->
          match T.Stream.view event with
          | Item_finalized _ -> raise Commit_failed
          | _ -> ())
    with
    | _ -> assert false
    | exception Commit_failed -> assert !committed)
;;

let%test_unit "strict delivery propagates every lifecycle observer failure" =
  let module Executor = Chat_response.Tool_executor in
  let module Event = Chat_response.Tool_execution_event in
  List.iter [ "started"; "progress"; "trace"; "finished" ] ~f:(fun failing ->
    let reached_after_emit = ref false in
    let runner ~invocation _ =
      Ochat_function.Invocation.emit
        invocation
        { channel = `Activity; update = Append "progress" };
      Ochat_function.Invocation.emit_trace
        invocation
        (Tool_started
           { call_id = "nested"; name = "nested"; kind = `Function; payload = "{}" });
      reached_after_emit := true;
      R.Tool_output.Output.Text "result"
    in
    (match
       Executor.run
         ~kind:`Function
         ~call_id:"strict"
         ~name:"strict"
         ~payload:"{}"
         ~runner
         ~on_tool_execution:(fun _ -> raise Commit_failed)
         ~on_execution_event:(fun event ->
           let actual =
             match event with
             | Event.Started _ -> "started"
             | Progress _ -> "progress"
             | Trace _ -> "trace"
             | Finished _ -> "finished"
           in
           if String.equal actual failing then raise Commit_failed)
         ()
     with
     | _ -> assert false
     | exception Commit_failed -> ());
    assert (Bool.equal !reached_after_emit (String.equal failing "finished")))
;;

let%test_unit "strict progress cancellation aborts the runner and keeps cancellation" =
  Eio_main.run (fun _ ->
    let continued = ref false in
    let runner ~invocation _ =
      Ochat_function.Invocation.emit
        invocation
        { channel = `Activity; update = Append "progress" };
      continued := true;
      R.Tool_output.Output.Text "unexpected"
    in
    (match
       Chat_response.Tool_executor.run
         ~kind:`Function
         ~call_id:"cancel"
         ~name:"cancel"
         ~payload:"{}"
         ~runner
         ~on_execution_event:(function
           | Chat_response.Tool_execution_event.Progress _ ->
             raise (Eio.Cancel.Cancelled (Failure "strict observer"))
           | Started _ | Trace _ | Finished _ -> ())
         ()
     with
     | _ -> assert false
     | exception Eio.Cancel.Cancelled (Failure message) ->
       assert (String.equal message "strict observer")
     | exception _ -> assert false);
    assert (not !continued))
;;

let%test_unit
    "nested direct tool events retain actual child attempts and parent correlation"
  =
  Eio_main.run (fun env ->
    let fork_call =
      { call with
        R.Function_call.name = "fork"
      ; arguments = {|{"command":"inspect","arguments":[]}|}
      ; call_id = "fork-parent"
      ; id = Some "reused-alias"
      }
    in
    let child_call = { call with id = Some "reused-alias" } in
    let responses =
      Queue.of_list
        [ call_events fork_call
        ; call_events child_call
        ; call_events child_call
        ; [ done_ (message "child result") ]
        ; [ done_ (message "parent result") ]
        ]
    in
    let requests = ref 0 in
    let post_with_retry ~sw ~inputs =
      Int.incr requests;
      post responses ~sw ~inputs
    in
    let events = Queue.create ()
    and tools = Queue.create ()
    and legacy = Queue.create () in
    let table = table () in
    Hashtbl.set table ~key:"echo" ~data:(fun ~invocation input ->
      Ochat_function.Invocation.emit
        invocation
        { channel = `Activity; update = Append "child progress" };
      Ochat_function.Invocation.emit_trace
        invocation
        (Tool_started
           { call_id = "native-nested"
           ; name = "native"
           ; kind = `Function
           ; payload = "{}"
           });
      R.Tool_output.Output.Text input);
    let history =
      selected_run
        ~env
        ~datadir:(Eio.Stdenv.cwd env)
        ~allocator:(allocator ())
        ~history:[]
        ~tools:(Some [])
        ~tool_tbl:table
        ~parallel_tool_calls:false
        ~on_transcript_event:(Queue.enqueue events)
        ~on_tool_execution:(Queue.enqueue legacy)
        ~on_scoped_tool_execution:(fun ~scope event -> Queue.enqueue tools (scope, event))
        ~post_stream:post_with_retry
        ()
    in
    assert (List.length history = 3);
    let child_starts =
      Queue.to_list tools
      |> List.filter_map ~f:(fun (scope, event) ->
        match event with
        | Chat_response.Tool_execution_event.Started { name = "echo"; _ } -> Some scope
        | _ -> None)
    in
    assert (List.length child_starts = 2);
    let first = List.nth_exn child_starts 0
    and second = List.nth_exn child_starts 1 in
    assert (T.Source_id.equal first.key.source second.key.source);
    assert (not (T.Attempt_id.equal first.key.attempt second.key.attempt));
    let root_call = List.hd_exn history in
    List.iter child_starts ~f:(fun scope ->
      match scope.T.Scope.relation with
      | Root -> assert false
      | Nested parent ->
        assert (
          Option.equal
            History_entry.Id.equal
            parent.call_entry_id
            (Some (History_entry.id root_call)));
        assert (Option.equal String.equal parent.call_alias (Some "fork-parent")));
    let nested_final =
      Queue.to_list events
      |> List.count ~f:(fun event ->
        match T.Stream.view event with
        | Item_finalized { item; _ } ->
          (match item.scope.relation with
           | Nested _ -> true
           | Root -> false)
        | _ -> false)
    in
    assert (nested_final = 5);
    assert (!requests = 5);
    assert (
      Queue.exists legacy ~f:(function
        | Chat_response.Tool_execution_event.Trace _ -> true
        | _ -> false));
    assert (
      not
        (Queue.exists tools ~f:(fun (scope, event) ->
           match scope.relation, event with
           | Root, Chat_response.Tool_execution_event.Trace _ -> true
           | _ -> false)));
    assert (
      Queue.exists tools ~f:(fun (scope, event) ->
        match scope.relation, event with
        | Nested _, Chat_response.Tool_execution_event.Trace _ -> true
        | _ -> false)))
;;

let%test_unit "primary cancellation survives an armed failing terminal observer" =
  List.iter [ false; true ] ~f:(fun cancel_from_progress ->
    Eio_main.run (fun _ ->
      let finished = ref false in
      let legacy = Queue.create () in
      let cancellation = Eio.Cancel.Cancelled (Failure "primary cancellation") in
      let runner ~invocation _ =
        if cancel_from_progress
        then
          Ochat_function.Invocation.emit
            invocation
            { channel = `Activity; update = Append "cancel" };
        raise cancellation
      in
      (match
         Chat_response.Tool_executor.run
           ~kind:`Function
           ~call_id:"cancel"
           ~name:"cancel"
           ~payload:"{}"
           ~runner
           ~on_tool_execution:(Queue.enqueue legacy)
           ~on_execution_event:(function
             | Chat_response.Tool_execution_event.Progress _ when cancel_from_progress ->
               raise cancellation
             | Finished _ ->
               finished := true;
               raise Commit_failed
             | Started _ | Progress _ | Trace _ -> ())
           ()
       with
       | _ -> assert false
       | exception Eio.Cancel.Cancelled (Failure message) ->
         assert (String.equal message "primary cancellation")
       | exception _ -> assert false);
      assert (not !finished);
      assert (
        Queue.count legacy ~f:(function
          | Chat_response.Tool_execution_event.Finished { outcome = Cancelled; _ } -> true
          | _ -> false)
        = 1)))
;;

let%test_unit "non-cancellation primary and secondary failures are both retained" =
  let runner ~invocation:_ _ = failwith "primary runner failure" in
  match
    Chat_response.Tool_executor.run
      ~kind:`Function
      ~call_id:"failure"
      ~name:"failure"
      ~payload:"{}"
      ~runner
      ~on_execution_event:(function
        | Chat_response.Tool_execution_event.Finished _ -> raise Commit_failed
        | Started _ | Progress _ | Trace _ -> ())
      ()
  with
  | _ -> assert false
  | exception Exn.Finally (Failure message, Commit_failed) ->
    assert (String.equal message "primary runner failure")
  | exception _ -> assert false
;;
