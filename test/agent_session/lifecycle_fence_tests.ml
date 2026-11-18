open! Core
open Fixtures
module A = Agent_session.Session_actor
module P = Agent_protocol

let user_entry actor text =
  let ids =
    Agent_session.History_id_source.create
      ~namespace:(P.Id.Session.to_string session_id)
      ~block_size:1
      ~reserve:(fun ~count -> A.reserve_history_block actor ~count)
    |> protocol_ok
  in
  let id = Agent_session.History_id_source.allocate ids |> protocol_ok in
  Agent_session.History_codec.user_text ~id text
  |> Agent_session.History_codec.to_protocol
;;

let conflict = function
  | Error { P.Error.code = Conflict; _ } -> true
  | Error failure -> raise_s [%sexp (failure : P.Error.t)]
  | Ok _ -> false
;;

let%expect_test
    "lifecycle exclusion guards canonical edits and admission and can abort before \
     retirement"
  =
  Job_fixtures.with_actor (fun _env _sw actor writer _backend ->
    let entry = user_entry actor "retained input" in
    let original = A.state actor |> protocol_ok in
    let stale =
      A.begin_lifecycle
        actor
        ~attachment_id:(Some writer.id)
        ~expected_generation:original.identity.generation
        ~expected_revision:Int64.(original.counters.revision + 1L)
    in
    let fence =
      A.begin_lifecycle
        actor
        ~attachment_id:(Some writer.id)
        ~expected_generation:original.identity.generation
        ~expected_revision:original.counters.revision
      |> protocol_ok
    in
    let patch =
      P.Session_metadata.Patch.create
        ~name:(Set "fenced")
        ~set_labels:[]
        ~remove_labels:[]
      |> protocol_ok
    in
    let edit_blocked =
      A.update_metadata
        actor
        ~attachment_id:writer.id
        ~expected_metadata_revision:original.identity.metadata_revision
        ~patch
        ()
      |> conflict
    in
    let history_blocked =
      A.append_history actor ~attachment_id:writer.id [ entry ] |> conflict
    in
    let start_blocked = A.start actor ~attachment_id:writer.id |> conflict in
    let observed = A.lifecycle_state actor fence |> protocol_ok in
    print_s
      [%sexp
        (( conflict stale
         , edit_blocked
         , history_blocked
         , start_blocked
         , Int64.equal original.counters.revision observed.counters.revision )
         : bool * bool * bool * bool * bool)];
    A.abort_lifecycle actor fence |> protocol_ok;
    A.append_history actor ~attachment_id:writer.id [ entry ] |> protocol_ok |> ignore;
    let current = A.state actor |> protocol_ok in
    print_s [%sexp (List.length current.conversation.canonical_history : int)]);
  [%expect
    {|
    (true true true true true)
    1
    |}]
;;

let%expect_test
    "permanent lifecycle retirement preserves reads and actual joins and cannot reopen"
  =
  Job_fixtures.with_actor (fun _env _sw actor writer _backend ->
    let state = A.state actor |> protocol_ok in
    let fence =
      A.begin_lifecycle
        actor
        ~attachment_id:(Some writer.id)
        ~expected_generation:state.identity.generation
        ~expected_revision:state.counters.revision
      |> protocol_ok
    in
    A.retire_lifecycle actor fence |> protocol_ok;
    let join = A.retire_runtime_worker actor ~closing:true |> protocol_ok in
    A.Runtime_retirement.await join |> protocol_ok;
    A.set_runtime_worker actor ~worker:None ~inference:None |> protocol_ok;
    let abort_blocked = A.abort_lifecycle actor fence |> conflict in
    let start_blocked = A.start actor ~attachment_id:writer.id |> conflict in
    let second_blocked =
      A.begin_lifecycle
        actor
        ~attachment_id:None
        ~expected_generation:state.identity.generation
        ~expected_revision:state.counters.revision
      |> conflict
    in
    let observed = A.lifecycle_state actor fence |> protocol_ok in
    print_s
      [%sexp
        (( abort_blocked
         , start_blocked
         , second_blocked
         , A.Runtime_retirement.is_finished join
         , P.Id.Session.equal observed.identity.session_id state.identity.session_id )
         : bool * bool * bool * bool * bool)]);
  [%expect {| (true true true true true) |}]
;;

let%expect_test "resource observations follow accepted Actor state and local fences" =
  let module O = A.Resource_observation in
  let module Selection = Inference.Selection in
  let unresolved =
    Selection.unresolved ~limits:document_limits
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let target =
    match Selection.view (inference_selection ()) with
    | Captured target -> target
    | Unresolved -> failwith "expected a captured fixture target"
  in
  let write_entered, write_entered_u = Eio.Promise.create () in
  let release_write, release_write_u = Eio.Promise.create () in
  let gate_write = ref false in
  Job_fixtures.with_actor
    ~start:false
    ~prepare_state:(fun state ->
      { state with spec = { state.spec with inference_target = unresolved } })
    ~reject_save:(fun _ ->
      if !gate_write
      then (
        gate_write := false;
        Eio.Promise.resolve write_entered_u ();
        Eio.Promise.await release_write);
      false)
    (fun env _sw actor writer _backend ->
       let observe () = A.observe_resource_state actor |> protocol_ok in
       let observation = observe () in
       let original = O.state observation in
       let patch =
         P.Session_metadata.Patch.create
           ~name:(Set "renamed")
           ~set_labels:[ "tag", "current" ]
           ~remove_labels:[]
         |> protocol_ok
       in
       A.update_metadata
         actor
         ~attachment_id:writer.id
         ~expected_metadata_revision:original.identity.metadata_revision
         ~patch
         ()
       |> protocol_ok
       |> ignore;
       let renamed = O.check_current observation |> protocol_ok in
       [%test_eq: string option] (Some "renamed") renamed.identity.display_name;
       [%test_eq: string option] (Some "actor") original.identity.display_name;
       assert (phys_equal (O.state observation) original);
       assert (Int64.(renamed.counters.revision > original.counters.revision));
       gate_write := true;
       let () =
         Eio.Fiber.both
           (fun () ->
              A.capture_inference_target actor ~target ~limits:document_limits
              |> protocol_ok)
           (fun () ->
              Eio.Promise.await write_entered;
              Exn.protect
                ~finally:(fun () -> Eio.Promise.resolve release_write_u ())
                ~f:(fun () ->
                  assert (phys_equal (O.check_current observation |> protocol_ok) renamed)))
       in
       assert (conflict (O.check_current observation));
       A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
       let running = observe () in
       A.stop actor ~attachment_id:writer.id ~mode:Graceful |> protocol_ok |> ignore;
       assert (conflict (O.check_current running));
       let stopped = observe () in
       let state = O.check_current stopped |> protocol_ok in
       [%test_eq: P.Session.desired_state] Stopped state.lifecycle.desired;
       [%test_eq: int64] (Int64.succ (O.state running).stop_epoch) state.stop_epoch;
       let fence =
         A.begin_lifecycle
           actor
           ~attachment_id:(Some writer.id)
           ~expected_generation:state.identity.generation
           ~expected_revision:state.counters.revision
         |> protocol_ok
       in
       assert (conflict (O.check_current stopped));
       assert (conflict (A.observe_resource_state actor));
       A.abort_lifecycle actor fence |> protocol_ok;
       let before_retirement = observe () in
       let join = A.retire_runtime_worker actor ~closing:true |> protocol_ok in
       A.Runtime_retirement.await join |> protocol_ok;
       assert (conflict (O.check_current before_retirement));
       assert (conflict (A.observe_resource_state actor));
       A.shutdown actor;
       let closed = function
         | Error (failure : P.Error.t) ->
           [%test_eq: P.Error.code] Server_shutting_down failure.code
         | Ok _ -> failwith "closed Actor returned a resource observation"
       in
       closed (O.check_current before_retirement);
       closed (A.observe_resource_state actor);
       Eio.Domain_manager.run (Eio.Stdenv.domain_mgr env) (fun () ->
         assert (conflict (O.check_current before_retirement)));
       print_endline "accepted facts, original snapshot and local fences verified");
  [%expect {| accepted facts, original snapshot and local fences verified |}]
;;
