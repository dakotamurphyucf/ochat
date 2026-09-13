open Core
module F = Moderator_invocation_fixtures
module M = Chat_response.Moderator_manager
module N = Chat_response.Notification_operations
module P = Agent_protocol

let body =
  {|
let reference = { key = "report"; invocation_id = `Some(p.context.invocation_id); work = `None } in
let* () = Task.catch(
  (let* discarded = Notification.publish(reference, `Cancelled("discard"), `No_wake) in
   Task.fail("undo notification")),
  fun error -> Task.pure(())) in
let* id = Notification.publish(reference,
  `Failed({ code = "report.failed"; message = "retry later"; retryable = true; details = `Null }),
  `Next_turn) in
let* current = Notification.get(id) in
let* () = Invocation.resolve(p.context.invocation_id, `Complete(current)) in
Task.pure(state + 1)
|}
;;

type failure =
  | Prepare
  | Save
  | Neither
[@@deriving sexp_of]

let%expect_test
    "notification intent rollback and durable acknowledgement share the moderator \
     transaction"
  =
  Eio_main.run (fun env ->
    List.iter [ Prepare; Save; Neither ] ~f:(fun failure ->
      let manager, prepared, make = F.setup env body in
      let invocation = make () in
      let script = Chat_response.Extension_compiler.script prepared in
      let staged = ref []
      and selected = ref []
      and durable = ref [] in
      let next = ref 0
      and trace = ref [] in
      let log item = trace := !trace @ [ item ] in
      let find id =
        List.find_map !staged ~f:(fun (_, value) ->
          Option.some_if (P.Id.Delivery.equal id value.P.Delivery.context.id) value)
        |> Result.of_option ~error:"notification is not scoped"
      in
      let handlers : N.handlers =
        { publish =
            (fun ~correlation ~completion ~wake ->
              assert (String.equal correlation.key "report");
              assert (
                Option.equal
                  P.Id.Invocation.equal
                  correlation.invocation_id
                  (Some invocation.context.id));
              assert (Option.is_none correlation.work);
              let delivery =
                P.Delivery.create
                  { id = P.Id.Delivery.create_with F.generator
                  ; session_id = invocation.context.session_id
                  ; generation = invocation.context.generation
                  ; invocation_id = correlation.invocation_id
                  ; work = correlation.work
                  ; correlation = correlation.key
                  ; source = Moderator
                  ; completion
                  ; wake
                  ; created_at = invocation.context.created_at
                  ; ownership =
                      Some
                        { source =
                            { script_id = script.id
                            ; source_sha256 = script.source_sha256
                            }
                        ; creator = Invocation invocation.context.id
                        }
                  }
                |> F.protocol
              in
              let receipt = !next in
              incr next;
              staged := (receipt, delivery) :: !staged;
              Ok (receipt, delivery))
        ; get = find
        ; rollback =
            (fun receipt ->
              match !staged with
              | (actual, _) :: rest when Int.equal receipt actual ->
                staged := rest;
                log ("undo " ^ Int.to_string receipt)
              | _ -> failwith "notification rollback order changed")
        }
      in
      let notifications : N.transaction =
        { handlers
        ; prepare =
            (fun receipts ->
              [%test_eq: int list] [ 1 ] receipts;
              selected := receipts;
              log "select";
              match failure with
              | Prepare -> Error "selection rejected"
              | Save | Neither ->
                Ok
                  (fun () ->
                    assert (not (List.is_empty !durable));
                    log "acknowledge";
                    staged := []))
        }
      in
      let invoke ?notifications () =
        M.handle_invocation_entries
          ?notifications
          manager
          ~invocation
          ~history:[]
          ~available_tools:[]
          ~session_meta:`Null
          ~now_ms:0
          ~validate_work:(fun _ -> Error "no background work")
          ~prepare_resolution:(fun ~resolved ~outcome:_ ~snapshot:_ ->
            let delivery = snd (List.hd_exn !staged) in
            (match resolved.status with
             | Resolved (Complete json) ->
               let decoded = P.Delivery.of_json json |> F.protocol in
               assert (
                 Jsonaf.exactly_equal
                   (P.Delivery.to_json decoded)
                   (P.Delivery.to_json delivery));
               assert (Option.is_some decoded.context.ownership);
               (match decoded.context.completion with
                | Failed { code = "report.failed"; _ } -> ()
                | _ -> failwith "completion lost")
             | _ -> failwith "notification ID or status was not returned");
            log "proposal";
            Ok
              { M.persist =
                  (fun () ->
                    [%test_eq: int list] [ 1 ] !selected;
                    log "persist";
                    match failure with
                    | Save -> Error "save rejected"
                    | Prepare -> failwith "saved after rejection"
                    | Neither ->
                      durable := [ delivery ];
                      Ok ())
              ; install = (fun () -> log "install")
              })
      in
      (match failure, invoke ~notifications () with
       | Neither, Ok _ -> ()
       | (Prepare | Save), Error message ->
         print_endline message;
         staged := [];
         selected := []
       | _ -> failwith "unexpected notification transaction outcome");
      print_s
        [%sexp (failure : failure), (!trace : string list), (List.length !durable : int)];
      let snapshot = F.state manager in
      F.expect "notification transaction is not installed" (invoke ());
      assert (Int.equal 0 (Session.Snapshot.compare snapshot (F.state manager)))));
  [%expect
    {|
    selection rejected
    (Prepare ("undo 0" proposal select) 0)
    save rejected
    (Save ("undo 0" proposal select persist) 0)
    (Neither ("undo 0" proposal select persist install acknowledge) 1)
    |}]
;;

let%expect_test "notification publication authority is excluded from computation surfaces"
  =
  let source =
    {|let main input = Notification.publish(
    { key = "report"; invocation_id = `None; work = `None }, `Succeeded(input), `No_wake)|}
  in
  let compile surface = Chatml_host_runtime.compile_script ~surface ~source () in
  compile Chatml.Chatml_extension_surface.moderator_v1 |> F.ok |> ignore;
  List.iter
    [ Chatml.Chatml_extension_surface.one_off_v1
    ; Chatml.Chatml_extension_surface.tool_v1
    ]
    ~f:(fun surface -> assert (Result.is_error (compile surface)));
  print_endline "only the moderator surface can compile notification publication";
  [%expect {| only the moderator surface can compile notification publication |}]
;;

let%expect_test
    "notification transactions reach ordinary, queued and observation handlers"
  =
  Eio_main.run (fun env ->
    List.iter [ `Ordinary; `Queued; `Observation ] ~f:(fun mode ->
      let publish =
        {|let* id = Notification.publish(
        { key = "event"; invocation_id = `None; work = `None },
        `Succeeded(`String("ready")), `No_wake) in Task.pure(state + 1)|}
      in
      let events =
        "| `Turn_end -> "
        ^ publish
        ^ "\n| `Internal_event(payload) -> "
        ^ publish
        ^ "\n| `Tool_observed(p) -> "
        ^ publish
        ^ "\n| _ -> Task.pure(state)"
      in
      let manager, prepared, make = F.setup env ~events "Task.pure(state)" in
      let context = (make ()).context in
      let script = Chat_response.Extension_compiler.script prepared in
      let source : P.Invocation.observer =
        { script_id = script.id; source_sha256 = script.source_sha256 }
      in
      let trace = ref [] in
      let log value = trace := !trace @ [ value ] in
      let notifications : N.transaction =
        { handlers =
            { publish =
                (fun ~correlation ~completion ~wake ->
                  assert (String.equal correlation.key "event");
                  assert (
                    Option.is_none correlation.invocation_id
                    && Option.is_none correlation.work);
                  log "stage";
                  let delivery =
                    P.Delivery.create
                      { id = P.Id.Delivery.create_with F.generator
                      ; session_id = context.session_id
                      ; generation = context.generation
                      ; invocation_id = None
                      ; work = None
                      ; correlation = correlation.key
                      ; source = Moderator
                      ; completion
                      ; wake
                      ; created_at = context.created_at
                      ; ownership = Some { source; creator = Invocation context.id }
                      }
                    |> F.protocol
                  in
                  Ok (0, delivery))
            ; get = (fun _ -> Error "unused")
            ; rollback = (fun _ -> failwith "unexpected event rollback")
            }
        ; prepare =
            (fun receipts ->
              [%test_eq: int list] [ 0 ] receipts;
              log "select";
              Ok (fun () -> log "acknowledge"))
        }
      in
      let prepare snapshot =
        [%test_eq: int]
          0
          (Session.Snapshot.compare
             snapshot.Session.Moderator_state.Identity_snapshot.current_state
             (Int 1));
        Ok
          { M.persist =
              (fun () ->
                log "persist";
                Ok ())
          ; install = (fun () -> log "install")
          }
      in
      let prepare_event ~outcome:_ ~snapshot = prepare snapshot in
      let on_tool_call ~name:_ ~args:_ = Error "unexpected native call" in
      (match mode with
       | `Ordinary ->
         M.handle_event_entries_transactional
           ~notifications
           manager
           ~session_id:(P.Id.Session.to_string context.session_id)
           ~now_ms:0
           ~history:[]
           ~available_tools:[]
           ~session_meta:`Null
           ~event:Turn_end
           ~authorize:(fun () -> Ok ())
           ~on_tool_call
           ~prepare_event
         |> F.ok
         |> ignore
       | `Queued ->
         M.enqueue_internal_event_entries
           manager
           ~event:
             (Chatml.Chatml_lang.VVariant
                ("Internal_event", [ VVariant ("String", [ VString "tick" ]) ]))
           ~prepare:(fun ~before:_ ~snapshot:_ -> Ok ())
         |> F.ok
         |> ignore;
         M.handle_next_event_entries_transactional
           ~notifications
           manager
           ~session_id:(P.Id.Session.to_string context.session_id)
           ~now_ms:0
           ~history:[]
           ~available_tools:[]
           ~session_meta:`Null
           ~authorize:(fun ~event:_ -> Ok ())
           ~on_tool_call
           ~prepare_event
         |> F.ok
         |> Option.value_exn
         |> ignore;
         assert (
           List.is_empty (M.identity_snapshot manager |> F.ok).queued_internal_events)
       | `Observation ->
         let invocation =
           P.Invocation.create
             ~observer:source
             { context with
               origin = Moderator
             ; provider_call_id = None
             ; parent_invocation = Some (P.Id.Invocation.create_with F.generator)
             }
           |> F.protocol
           |> P.Invocation.dispatch
           |> F.protocol
         in
         let invocation =
           P.Invocation.resolve
             invocation
             ~session_id:context.session_id
             ~generation:context.generation
             (Complete `Null)
           |> F.protocol
           |> P.Invocation.claim_observation
           |> F.protocol
         in
         M.handle_observation_entries
           ~notifications
           manager
           ~invocation
           ~history:[]
           ~available_tools:[]
           ~session_meta:`Null
           ~now_ms:0
           ~prepare_observation:(fun ~observed:_ ~outcome:_ ~snapshot -> prepare snapshot)
         |> F.ok
         |> ignore);
      print_s
        [%sexp (mode : [ `Ordinary | `Queued | `Observation ]), (!trace : string list)]));
  [%expect
    {|
    (Ordinary (stage select persist install acknowledge))
    (Queued (stage select persist install acknowledge))
    (Observation (stage select persist install acknowledge))
    |}]
;;
