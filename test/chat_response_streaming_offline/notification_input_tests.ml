open Core
module F = Fixtures
module Stream = Chat_response.In_memory_stream
module Input = Stream.Safe_point_input
module P = Agent_protocol
module Res = Openai.Responses

let ok result =
  Result.map_error result ~f:(fun error -> error.P.Error.message) |> Result.ok_or_failwith
;;

let notification sequence =
  let delivery =
    P.Delivery.create
      { id = P.Id.Delivery.of_string (Printf.sprintf "dlv_notice_%d" sequence) |> ok
      ; session_id = P.Id.Session.of_string "ses_AAAAAAAAAAAAAAAAAAAAAAAA" |> ok
      ; generation = 0
      ; invocation_id = None
      ; work = None
      ; correlation = "stream-test"
      ; source = Moderator
      ; completion = Succeeded (`Object [ "result", `String "ordinary result data" ])
      ; wake = No_wake
      ; created_at = P.Timestamp.of_string "2026-09-10T00:00:00Z" |> ok
      ; ownership = None
      }
    |> ok
  in
  let id =
    History_entry.Id.create ~namespace:"runtime-data" ~sequence |> Result.ok_or_failwith
  in
  Agent_session.Notification_history.create ~id delivery
  |> ok
  |> Agent_session.History_codec.of_protocol
  |> ok
;;

let is_notification entry =
  String.equal (History_entry.Id.namespace (History_entry.id entry)) "runtime-data"
;;

type mode =
  | Quiet
  | Disabled
  | Wake
  | Repeated
  | User_with_quiet
  | Moderator_wake
  | Moderator_end
[@@deriving sexp_of]

let%expect_test
    "notification input stays quiet, coalesces wakes and cannot reset the self-trigger \
     budget"
  =
  List.iter
    [ Quiet; Disabled; Wake; Repeated; User_with_quiet; Moderator_wake; Moderator_end ]
    ~f:(fun mode ->
      Eio_main.run (fun env ->
        let allocator =
          History_entry.Allocator.create ~namespace:"notification-root" ~next_sequence:0
          |> Result.ok_or_failwith
        in
        let requests = ref 0
        and consumed = ref 0
        and notifications = ref 0
        and inserted = ref 0
        and wake_requests = ref 0
        and ends = ref 0 in
        let policy =
          { Chat_response.Runtime_semantics.default_policy with
            honor_request_turn =
              (match mode with
               | Disabled | User_with_quiet -> false
               | _ -> true)
          ; budget =
              { Chat_response.Runtime_semantics.default_budget_policy with
                max_self_triggered_turns = 2
              }
          }
        in
        let moderator =
          F.moderator_of_source
            ~runtime_policy:policy
            (match mode with
             | Moderator_wake ->
               "let initial_state = false\n\
                let on_event ctx state event = match event with | `Item_appended(item) \
                -> Task.pure(true) | `Turn_end -> (match state with | true -> let* () = \
                Runtime.request_turn() in Task.pure(false) | false -> Task.pure(false)) \
                | _ -> Task.pure(state)"
             | Moderator_end ->
               "let initial_state = 0\n\
                let on_event ctx state event = match event with | `Item_appended(item) \
                -> let* () = Runtime.end_session(\"done\") in Task.pure(state) | _ -> \
                Task.pure(state)"
             | _ ->
               "let initial_state = 0\nlet on_event ctx state event = Task.pure(state)")
        in
        let source : Input.t =
          { consume_entries =
              (fun () ->
                Int.incr consumed;
                match mode, !consumed with
                | Repeated, _ ->
                  Int.incr inserted;
                  Input.notification_entries ~request_turn:true [ notification !consumed ]
                | ( ( Quiet
                    | Disabled
                    | Wake
                    | User_with_quiet
                    | Moderator_wake
                    | Moderator_end )
                  , 1 ) ->
                  inserted := !inserted + 3;
                  let data =
                    Input.notification_entries
                      ~request_turn:
                        (match mode with
                         | Disabled | Wake | Moderator_end -> true
                         | _ -> false)
                      [ notification 0; notification 1; notification 2 ]
                  in
                  (match mode with
                   | User_with_quiet ->
                     Input.append (Input.user_entries [ F.input_entry allocator ]) data
                   | _ -> data)
                | _ -> Input.empty)
          ; consume_compatibility_text = (fun () -> None)
          }
        in
        let post_stream ~sw:_ ~inputs =
          Int.incr requests;
          List.iter inputs ~f:(function
            | Res.Item.Input_message { role = Developer; _ } ->
              failwith "notification became developer input"
            | _ -> ());
          match !requests <= 4 with
          | true -> Stdlib.Seq.empty
          | false -> failwith "notification loop bypassed its budget"
        in
        let run () =
          Stream.run_completion_stream_in_memory_entries
            ~env
            ~allocator
            ~history:[ F.input_entry allocator ]
            ~moderator
            ~safe_point_input:source
            ~on_history_item_appended:(fun entry ->
              if is_notification entry then Int.incr notifications)
            ~on_runtime_request:(function
              | Request_turn -> Int.incr wake_requests
              | End_session _ -> Int.incr ends
              | Request_compaction -> ())
            ~tools:(Some [])
            ~tool_tbl:(String.Table.create ())
            ~post_stream
            ()
        in
        let outcome =
          try
            let history = run () in
            [%test_eq: int] 3 (List.count history ~f:is_notification);
            "stopped"
          with
          | Failure message
            when String.equal
                   message
                   "Exceeded maximum consecutive moderator-requested turns (2)." ->
            (match mode with
             | Repeated -> "budget"
             | _ -> failwith "unexpected budget rejection")
        in
        [%test_eq: int] 0 !notifications;
        print_s
          [%sexp
            (mode : mode)
          , (!requests : int)
          , (!inserted : int)
          , (!wake_requests : int)
          , (!ends : int)
          , (outcome : string)]));
  [%expect
    {|
    (Quiet 1 3 0 0 stopped)
    (Disabled 1 3 1 0 stopped)
    (Wake 2 3 1 0 stopped)
    (Repeated 3 3 3 0 budget)
    (User_with_quiet 2 3 0 0 stopped)
    (Moderator_wake 2 3 1 0 stopped)
    (Moderator_end 1 3 0 1 stopped)
    |}]
;;

let%expect_test
    "notification delivery waits for all sibling tool results before the next provider \
     request"
  =
  Eio_main.run (fun env ->
    let allocator =
      History_entry.Allocator.create ~namespace:"notification-batch" ~next_sequence:0
      |> Result.ok_or_failwith
    in
    let fast, signal_fast = Eio.Promise.create () in
    let slow, signal_slow = Eio.Promise.create () in
    let release, release_slow = Eio.Promise.create () in
    let slow_done = ref false
    and delivered = ref false
    and requests = ref 0
    and consumed = ref 0 in
    let tool_tbl = String.Table.create () in
    Hashtbl.set tool_tbl ~key:"echo" ~data:(fun ~invocation:_ input ->
      match String.equal input "fast" with
      | true ->
        Eio.Promise.resolve signal_fast ();
        Res.Tool_output.Output.Text "fast-result"
      | false ->
        Eio.Promise.resolve signal_slow ();
        Eio.Promise.await release;
        slow_done := true;
        Res.Tool_output.Output.Text "slow-result");
    let source : Input.t =
      { consume_entries =
          (fun () ->
            assert !slow_done;
            Int.incr consumed;
            match !delivered with
            | true -> Input.empty
            | false ->
              delivered := true;
              Input.notification_entries ~request_turn:true [ notification 0 ])
      ; consume_compatibility_text = (fun () -> None)
      }
    in
    let post_stream ~sw ~inputs =
      Int.incr requests;
      match !requests with
      | 1 ->
        Eio.Fiber.fork ~sw (fun () ->
          Eio.Promise.await fast;
          Eio.Promise.await slow;
          [%test_eq: int] 0 !consumed;
          [%test_eq: int] 1 !requests;
          Eio.Promise.resolve release_slow ());
        [ "fast"; "slow" ]
        |> List.concat_mapi ~f:(fun output_index arguments ->
          let added, done_ =
            F.stream_function_call
              ~output_index
              ~item_id:arguments
              ~call_id:arguments
              ~arguments
          in
          [ added; done_ ])
        |> Stdlib.List.to_seq
      | 2 ->
        let after_result = ref 0 in
        let notifications_in_request = ref 0 in
        List.iter inputs ~f:(function
          | Res.Item.Function_call_output _ -> Int.incr after_result
          | Input_message { role = User; content = Text { text; _ } :: _; _ }
            when String.is_prefix text ~prefix:"Ochat runtime notification." ->
            Int.incr notifications_in_request;
            [%test_eq: int] 2 !after_result
          | _ -> ());
        [%test_eq: int] 2 !after_result;
        [%test_eq: int] 1 !notifications_in_request;
        Stdlib.Seq.empty
      | _ -> failwith "coalesced wake issued an extra request"
    in
    let history =
      Stream.run_completion_stream_in_memory_entries
        ~env
        ~allocator
        ~history:[ F.input_entry allocator ]
        ~safe_point_input:source
        ~tools:(Some [])
        ~tool_tbl
        ~parallel_tool_calls:true
        ~post_stream
        ()
    in
    [%test_eq: int] 1 (List.count history ~f:is_notification);
    [%test_eq: int] 2 !requests;
    print_s [%sexp (List.map history ~f:F.entry_kind : string list)]);
  [%expect
    {| (input function-call function-call function-output function-output input) |}]
;;
