open Core
module Helpers = Ownership_tests
module L = Chatml.Chatml_lang
module Host = Chatml_host_runtime
module Surface = Chatml.Chatml_builtin_surface
module Captured = Chatml_source

let context = L.VRecord (String.Map.singleton "phase" (L.VString "turn_start"))
let config = Host.default_runtime_config ~surface:Surface.ui_moderator_surface ()
let event name = L.VVariant (name, [])

let instantiate ?control text =
  let source = Helpers.capture "session.chatml" text in
  let compiled =
    Host.compile_captured_script source ~surface:Surface.ui_moderator_surface ()
    |> Result.map_error ~f:(fun error -> error.Host.formatted)
    |> Result.ok_or_failwith
  in
  let session =
    Host.instantiate_session_detailed
      ?control
      config
      compiled
      ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
    |> Helpers.unwrap
  in
  source, session
;;

let assert_clean session =
  assert (Option.is_none (Host.current_phase session));
  assert (Option.is_none (Host.pending_ui_request session));
  assert (List.is_empty (Host.pending_local_effects session));
  assert (List.is_empty (Host.committed_local_effects session))
;;

let%expect_test "persistent events retain their captured revision and committed state" =
  let source, session =
    instantiate
      {|let initial_state = 0
let on_event ctx state event =
  match event with
  | `Continue -> Task.pure(state + 1)
  | `Fail -> Task.pure(1 / (state - 1))|}
  in
  Host.handle_event_detailed session ~context ~event:(event "Continue") |> Helpers.unwrap;
  let newer = Helpers.capture "session.chatml" "let initial_state = 123" in
  assert (Captured.Id.equal (Captured.id source) (Captured.id newer));
  assert (not (Captured.Digest.equal (Captured.digest source) (Captured.digest newer)));
  (match Host.handle_event_detailed session ~context ~event:(event "Fail") with
   | Ok () -> assert false
   | Error diagnostic ->
     let location = Helpers.require_owner source diagnostic in
     assert (
       String.equal (Captured.read source location |> Or_error.ok_exn) "1 / (state - 1)"));
  assert (L.equal_value (Host.current_state session) (L.VInt 1));
  assert_clean session;
  [%expect {| |}]
;;

let%expect_test
    "resumed callback failure retains source and discards only uncommitted work"
  =
  let source, session =
    instantiate
      {|let initial_state = 0
let on_event ctx state event =
  Task.bind(Turn.prepend_system("buffered"), fun ignored ->
  Task.bind(Runtime.emit(`Buffered), fun ignored ->
  Task.bind(Approval.ask_text("continue?"), fun answer ->
  Task.pure(1 / state))))|}
  in
  Host.handle_event_detailed session ~context ~event:L.VUnit |> Helpers.unwrap;
  assert (Option.is_some (Host.pending_ui_request session));
  assert (List.is_empty (Host.committed_local_effects session));
  Host.enqueue_internal_event session (event "Host") |> Result.ok_or_failwith;
  (match Host.resume_ui_request_detailed session ~response:"continue" with
   | Ok () -> assert false
   | Error diagnostic ->
     let location = Helpers.require_owner source diagnostic in
     assert (String.equal (Captured.read source location |> Or_error.ok_exn) "1 / state"));
  assert (L.equal_value (Host.current_state session) (L.VInt 0));
  assert (List.equal L.equal_value (Host.queued_events session) [ event "Host" ]);
  assert_clean session;
  [%expect {| |}]
;;

exception Interrupted

let%expect_test "event and resume control exceptions propagate and release active flags" =
  List.iter [ false; true ] ~f:(fun resume ->
    let armed = ref false in
    let control : L.execution_control =
      { checkpoint = (fun () -> ())
      ; diagnostic_checkpoint = (fun () -> ())
      ; allocate = (fun _ -> ())
      ; before_builtin = (fun ~name:_ _ -> ())
      ; check_value = (fun _ -> ())
      ; before_json_import = (fun _ -> ())
      ; before_json_export = (fun _ -> ())
      ; before_effect =
          (fun ~name ~spawned:_ ->
            if !armed && String.equal name "Log.info" then raise Interrupted)
      ; after_effect = (fun _ -> ())
      }
    in
    let _, session =
      instantiate
        ~control
        {|let initial_state = 0
let finish state = Task.bind(Log.info("checkpoint"), fun ignored -> Task.pure(state + 1))
let on_event ctx state event =
  Task.bind(Turn.prepend_system("buffered"), fun ignored ->
  match event with
  | `Direct -> finish(state)
  | `Approval -> Task.bind(Approval.ask_text("continue?"), fun answer -> finish(state)))|}
    in
    if resume
    then
      Host.handle_event_detailed session ~context ~event:(event "Approval")
      |> Helpers.unwrap;
    Host.enqueue_internal_event session (event "Host") |> Result.ok_or_failwith;
    armed := true;
    (match
       if resume
       then Host.resume_ui_request_detailed session ~response:"continue"
       else Host.handle_event_detailed session ~context ~event:(event "Direct")
     with
     | Ok () | Error _ -> failwith "control exception was swallowed"
     | exception Interrupted -> ());
    assert_clean session;
    assert (L.equal_value (Host.current_state session) (L.VInt 0));
    assert (List.equal L.equal_value (Host.queued_events session) [ event "Host" ]);
    armed := false;
    Host.handle_event_detailed session ~context ~event:(event "Approval")
    |> Helpers.unwrap;
    assert (Option.is_some (Host.pending_ui_request session)));
  [%expect {| |}]
;;
