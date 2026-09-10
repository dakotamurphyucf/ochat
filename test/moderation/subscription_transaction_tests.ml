open Core
module F = Moderator_invocation_fixtures
module M = Chat_response.Moderator_manager
module O = Chat_response.Subscription_operations
module P = Agent_protocol
module L = Chatml.Chatml_lang

let body =
  {|
let* id = Subscription.create("watch", `None, `Next_turn) in
let* () = Task.catch(
  (let* failed = Subscription.fail(id, 0,
     { code = "watch.failed"; message = "discard me"; retryable = false; details = `Null }) in
   let* repeated = Subscription.cancel(id, 0, "same retained winner") in
   Task.fail("discard both updates")),
  fun message -> Task.pure(())) in
let* finished = Subscription.complete(id, 0, `String("ready")) in
let* repeated = Subscription.cancel(id, 0, "too late") in
let* current = Subscription.get(id) in
let* () = Invocation.resolve(p.context.invocation_id, `Pending(`Subscription(id), current)) in
Task.pure(state + 1)
|}
;;

(* An in-memory host with an undo log. This qualifies the real compiler,
   evaluator and manager transaction boundary, not actor persistence/authority. *)
type staged =
  { ticket : int
  ; value : P.Subscription.t
  }

type failure =
  | Prepare
  | Save
  | Neither

let%expect_test
    "subscription receipts rollback independently and save precedes acknowledgement"
  =
  Eio_main.run (fun env ->
    List.iter [ Prepare; Save; Neither ] ~f:(fun failure ->
      let manager, _, make = F.setup env body in
      let invocation = make () in
      let id = P.Id.Subscription.create_with F.generator in
      let staged = ref [] in
      let selected = ref [] in
      let durable = ref None in
      let issued = ref 0 in
      let trace = ref [] in
      let log value = trace := !trace @ [ value ] in
      let find requested =
        match P.Id.Subscription.equal requested id, !staged, !durable with
        | true, entry :: _, _ -> Ok entry.value
        | true, [], Some value -> Ok value
        | _ -> Error "not owned"
      in
      let stage value =
        let ticket = !issued in
        incr issued;
        staged := { ticket; value } :: !staged;
        ticket
      in
      let handlers : O.handlers =
        { create =
            (fun ~kind ~lifetime_ms ~wake ->
              assert (Option.is_none lifetime_ms);
              let created_at = invocation.context.created_at in
              let deadline =
                P.Timestamp.to_time_ns created_at
                |> fun time ->
                Time_ns.add time (Time_ns.Span.of_int_sec 3600) |> P.Timestamp.of_time_ns
              in
              let value =
                P.Subscription.create
                  { id
                  ; session_id = invocation.context.session_id
                  ; generation = 0
                  ; invocation_id = invocation.context.id
                  ; source = None
                  ; parent_job = None
                  ; kind
                  ; created_at
                  ; deadline
                  ; completion_schema = Some `True
                  ; wake
                  ; ingress_capability = None
                  }
                |> F.protocol
              in
              Ok (stage value, id))
        ; get = find
        ; finish =
            (fun ~id ~expected_epoch completion ->
              let open Result.Let_syntax in
              let%map previous = find id in
              let value, _ =
                P.Subscription.finish
                  previous
                  ~expected_epoch
                  ~now:invocation.context.created_at
                  completion
                |> F.protocol
              in
              stage value, value)
        ; arm =
            (fun ~id:_ ~expected_epoch:_ ~timer_id:_ ~job_id:_ ->
              Error "arming not supported by this fixture")
        ; rollback =
            (fun ticket ->
              match !staged with
              | entry :: rest when Int.equal entry.ticket ticket ->
                log ("undo " ^ Int.to_string ticket);
                staged := rest
              | _ -> failwith "rollback was not newest first")
        }
      in
      let subscriptions : O.transaction =
        { handlers
        ; prepare =
            (fun receipts ->
              [%test_eq: int list] [ 0; 3; 4 ] receipts;
              [%test_eq: int list]
                receipts
                (List.rev_map !staged ~f:(fun entry -> entry.ticket));
              selected := receipts;
              log "select";
              match failure with
              | Prepare -> Error "selection rejected"
              | Save | Neither ->
                Ok
                  (fun () ->
                    assert (Option.is_some !durable);
                    log "acknowledge";
                    staged := []))
        }
      in
      let invoke ?subscriptions () =
        M.handle_invocation_entries
          ?subscriptions
          manager
          ~invocation
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~now_ms:0
          ~validate_work:(function
            | P.Invocation.Subscription requested -> Result.map (find requested) ~f:ignore
            | _ -> Error "not a subscription")
          ~prepare_resolution:(fun ~resolved ~outcome:_ ~snapshot:_ ->
            (match resolved.status with
             | Resolved (Pending (Subscription requested, json)) ->
               let value = find requested |> F.ok in
               assert (Jsonaf.exactly_equal json (P.Subscription.to_json value));
               assert (
                 Option.equal
                   P.Completion.equal
                   (Some (Succeeded (`String "ready")))
                   value.result)
             | _ -> failwith "expected owned Pending with terminal status");
            log "proposal";
            Ok
              { M.persist =
                  (fun () ->
                    [%test_eq: int list] [ 0; 3; 4 ] !selected;
                    log "persist";
                    match failure with
                    | Save -> Error "save rejected"
                    | Prepare -> failwith "persisted after failed preparation"
                    | Neither ->
                      durable := Some (find id |> F.ok);
                      Ok ())
              ; install = (fun () -> log "install")
              })
      in
      let result = invoke ~subscriptions () in
      (match failure, result with
       | Neither, Ok _ -> ()
       | (Prepare | Save), Error message ->
         print_endline message;
         (* Whole-handler cleanup belongs to the host, including selected state. *)
         staged := [];
         selected := []
       | _ -> failwith "unexpected commit result");
      print_s [%sexp (!trace : string list)];
      let snapshot = F.state manager in
      print_s [%sexp (Option.is_some !durable : bool)];
      (* Lexical handlers must be removed even after preparation/save errors. *)
      F.expect "subscription transaction is not installed" (invoke ());
      assert (Int.equal 0 (Session.Snapshot.compare snapshot (F.state manager)))));
  [%expect
    {|
    selection rejected
    ("undo 2" "undo 1" proposal select)
    false
    save rejected
    ("undo 2" "undo 1" proposal select persist)
    false
    ("undo 2" "undo 1" proposal select persist install acknowledge)
    true
    |}]
;;

let%expect_test "subscription authority is absent from one-off and standalone surfaces" =
  let source = "let main input = Subscription.create(\"watch\", `None, `No_wake)" in
  Chatml_host_runtime.compile_script
    ~surface:Chatml.Chatml_extension_surface.moderator_v1
    ~source
    ()
  |> F.ok
  |> ignore;
  List.iter
    [ Chatml.Chatml_extension_surface.one_off_v1
    ; Chatml.Chatml_extension_surface.tool_v1
    ]
    ~f:(fun surface ->
      match Chatml_host_runtime.compile_script ~surface ~source () with
      | Error _ -> print_endline "subscription module unavailable"
      | Ok _ -> failwith "ordinary tool gained moderator subscription surface");
  [%expect
    {|
    subscription module unavailable
    subscription module unavailable
    |}]
;;

let%expect_test
    "ordinary and queued events save subscription updates with their queue checkpoint"
  =
  Eio_main.run (fun env ->
    let id = P.Id.Subscription.create_with F.generator in
    let quoted_id = "\"" ^ P.Id.Subscription.to_string id ^ "\"" in
    let events =
      "| `Turn_end ->\n"
      ^ "let* value = Subscription.complete("
      ^ quoted_id
      ^ ", 0, `String(\"done\")) in\n"
      ^ "let* () = Runtime.emit(`String(\"follow-up\")) in Task.pure(state + 1)\n"
      ^ "| `Internal_event(payload) ->\n"
      ^ "let* value = Subscription.cancel("
      ^ quoted_id
      ^ ", 0, \"already done\") in\n"
      ^ "Task.pure(state + 1)\n| _ -> Task.pure(state)"
    in
    let manager, _, make =
      F.setup
        env
        ~events
        "let* () = Invocation.resolve(p.context.invocation_id, `Complete(`Null)) in \
         Task.pure(state)"
    in
    let invocation = make () in
    let created_at = invocation.context.created_at in
    let deadline =
      P.Timestamp.to_time_ns created_at
      |> fun time ->
      Time_ns.add time (Time_ns.Span.of_int_sec 3600) |> P.Timestamp.of_time_ns
    in
    let initial =
      P.Subscription.create
        { id
        ; session_id = invocation.context.session_id
        ; generation = 0
        ; invocation_id = invocation.context.id
        ; source = None
        ; parent_job = None
        ; kind = "watch"
        ; created_at
        ; deadline
        ; completion_schema = None
        ; wake = Next_turn
        ; ingress_capability = None
        }
      |> F.protocol
    in
    let durable = ref initial in
    let staged = ref None in
    let reject_save = ref false in
    let selected = ref false in
    let acknowledgements = ref 0 in
    let handlers : O.handlers =
      { create = (fun ~kind:_ ~lifetime_ms:_ ~wake:_ -> Error "no originating invocation")
      ; get =
          (fun requested ->
            assert (P.Id.Subscription.equal requested id);
            Ok (Option.value !staged ~default:!durable))
      ; finish =
          (fun ~id:requested ~expected_epoch result ->
            assert (P.Id.Subscription.equal requested id);
            let value, _ =
              P.Subscription.finish !durable ~expected_epoch ~now:created_at result
              |> F.protocol
            in
            staged := Some value;
            Ok (0, value))
      ; arm =
          (fun ~id:_ ~expected_epoch:_ ~timer_id:_ ~job_id:_ ->
            Error "arming not supported by this fixture")
      ; rollback = (fun _ -> staged := None)
      }
    in
    let subscriptions : O.transaction =
      { handlers
      ; prepare =
          (fun receipts ->
            [%test_eq: int list] [ 0 ] receipts;
            selected := true;
            Ok
              (fun () ->
                incr acknowledgements;
                staged := None;
                selected := false))
      }
    in
    let prepare_event ~outcome:_ ~snapshot:_ =
      Ok
        { M.persist =
            (fun () ->
              assert !selected;
              match !reject_save with
              | true -> Error "queue save rejected"
              | false ->
                durable := Option.value_exn !staged;
                Ok ())
        ; install = ignore
        }
    in
    let on_tool_call ~name:_ ~args:_ = Error "no native calls in this fixture" in
    M.handle_event_entries_transactional
      ~subscriptions
      manager
      ~session_id:(P.Id.Session.to_string invocation.context.session_id)
      ~now_ms:0
      ~history:[]
      ~available_tools:[]
      ~session_meta:`Null
      ~event:Turn_end
      ~authorize:(fun () -> Ok ())
      ~on_tool_call
      ~prepare_event
    |> F.ok
    |> ignore;
    assert (
      Option.equal P.Completion.equal (Some (Succeeded (`String "done"))) !durable.result);
    let before = M.identity_snapshot manager |> F.ok in
    [%test_eq: int] 1 (List.length before.queued_internal_events);
    let consume () =
      M.handle_next_event_entries_transactional
        ~subscriptions
        manager
        ~session_id:(P.Id.Session.to_string invocation.context.session_id)
        ~now_ms:0
        ~history:[]
        ~available_tools:[]
        ~session_meta:`Null
        ~authorize:(fun ~event:_ -> Ok ())
        ~on_tool_call
        ~prepare_event
    in
    reject_save := true;
    F.expect "queue save rejected" (consume ());
    staged := None;
    selected := false;
    let failed = M.identity_snapshot manager |> F.ok in
    assert (
      Int.equal 0 (Session.Snapshot.compare before.current_state failed.current_state));
    [%test_eq: int] 1 (List.length failed.queued_internal_events);
    [%test_eq: int] 1 !acknowledgements;
    reject_save := false;
    assert (Option.is_some (consume () |> F.ok));
    assert (Option.is_none (consume () |> F.ok));
    let after = M.identity_snapshot manager |> F.ok in
    [%test_eq: int] 0 (List.length after.queued_internal_events);
    [%test_eq: int] 2 !acknowledgements;
    print_s
      [%sexp
        ((before.current_state, failed.current_state, after.current_state)
         : Session.Snapshot.t * Session.Snapshot.t * Session.Snapshot.t)];
    print_s [%sexp (!durable.result : P.Completion.t option)]);
  [%expect
    {|
    ((Int 1) (Int 1) (Int 2))
    ((Succeeded (String done)))
    |}]
;;
