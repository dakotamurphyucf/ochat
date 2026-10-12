open! Core
open Fixtures
open Agent_server_test_support
module P = Agent_protocol
module C = Agent_client
module M = Managed_completion_cli

let select host connection session =
  let fields = snapshot connection session.P.Session.id in
  let lifecycle = Option.value_exn fields.lifecycle in
  Agent_server.Embedded.session_script_selection
    host
    ~expected:(P.Session_lifecycle.Observation.expected lifecycle)
    ~checkpoint:Eio.Fiber.check
  |> protocol_ok
;;

let finished_prompt =
  {|<developer>Managed completion definition.</developer>
<script id="complete" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.bind(Run.finish(`Object([
  { key = "kind"; value = `String("finish") },
  { key = "terminal"; value = `Object([{ key = "kind"; value = `String("completed") }]) },
  { key = "relinquish"; value = `Array([]) }])), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)
</script>|}
;;

let%expect_test "actual managed run terminal, canonical transcript and original receipt" =
  let requests = ref 0 in
  with_host
    ~remove_source:true
    ~prompt:finished_prompt
    ~requests
    ~f:(fun ~env ~host connection ->
      List.iteri [ P.Run.Mode.Single_turn; Workflow ] ~f:(fun index mode ->
        let session = create connection ~key:(sprintf "managed-%d" index) in
        let selection = select host connection session in
        (match selection.moderator with
         | Some (Standard Moderator_v1) -> ()
         | _ -> failwith "actual retained moderator role was not selected");
        [%test_eq: int] !requests 0;
        let views = ref 0 in
        let completed =
          run
            connection
            ~env
            ~session
            ~request:(Authored_start mode)
            ~on_view:(fun _ ->
              Int.incr views;
              Eio.Fiber.yield ())
            ()
          |> checked
        in
        assert (!views > 0);
        let admission, terminal =
          match completed.evidence with
          | M.Evidence.Managed_run { admission; terminal } -> admission, terminal
          | Foreground _ -> failwith "managed admission became a foreground turn"
        in
        assert (P.Run.Mode.equal terminal.run.mode mode);
        (match terminal.run.lifecycle with
         | Terminal (Completed _) -> ()
         | _ -> failwith "actual workflow did not finish");
        assert (Option.is_some terminal.terminal_receipt);
        let current = snapshot connection session.id in
        let selected = current.session in
        let observed =
          run connection ~env ~session:selected ~request:(Observe_run admission) ()
          |> checked
        in
        (match observed.evidence with
         | Managed_run original ->
           assert (P.Run_receipt.equal original.admission admission)
         | Foreground _ -> failwith "receipt observation changed execution branch");
        [%test_eq: string] completed.chatmd observed.chatmd;
        let query =
          P.Run_query.Request.create
            ~session:terminal.run.session
            ~page:(P.Page.Request.create ~limit:10 () |> protocol_ok)
          |> protocol_ok
        in
        let runs =
          C.Run_views.create
            connection
            ~server_id:(P.Session_ref.server_id terminal.run.session)
          |> fun views -> C.Run_views.page views query |> protocol_ok
        in
        [%test_eq: int] (List.length runs.items) 1;
        print_s
          [%message
            (mode : P.Run.Mode.t)
              ~canonical_definition:
                (String.is_substring
                   completed.chatmd
                   ~substring:"Managed completion definition."
                 : bool)];
        if index = 0
        then (
          let failing = create connection ~key:"admission-callback-failure" in
          let original = Failure "original admitted receipt callback" in
          try
            run
              connection
              ~env
              ~session:failing
              ~request:(Authored_start Single_turn)
              ~on_admission:(fun _ -> raise original)
              ()
            |> ignore;
            failwith "admission callback failure disappeared"
          with
          | exn -> assert (phys_equal exn original))));
  [%test_eq: int] !requests 0;
  [%expect
    {|
    ((mode Single_turn) (canonical_definition true))
    ((mode Workflow) (canonical_definition true))
    |}]
;;

let turn_prompt =
  {|<developer>Real workflow waits after a foreground turn.</developer>
<script id="turn" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> match event with
| `Session_start -> Task.bind(Run.continue(), fun ignored -> Task.pure(state))
| _ -> Task.pure(state)
</script>|}
;;

let%expect_test "failed authored callback has actual run terminal custody and receipt" =
  let requests = ref 0 in
  let prompt =
    {|<developer>Failing managed callback.</developer>
<script id="failure" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event ctx state event = Task.fail("actual callback failure")
</script>|}
  in
  with_host ~remove_source:false ~prompt ~requests ~f:(fun ~env ~host:_ connection ->
    let session = create connection ~key:"failed-callback" in
    let completed =
      run connection ~env ~session ~request:(Authored_start Workflow) () |> checked
    in
    let admission, terminal =
      match completed.evidence with
      | Managed_run { admission; terminal } -> admission, terminal
      | Foreground _ -> failwith "failed callback lost managed admission"
    in
    (match terminal.run.lifecycle with
     | Terminal (Failed None) -> ()
     | _ -> failwith "failed callback did not retire its actual run");
    let receipt = Option.value_exn terminal.terminal_receipt in
    assert (P.Id.Run.equal admission.run_id receipt.run_id);
    assert (P.Run_source.equal admission.source receipt.source);
    assert (P.Id.Principal.equal admission.principal_id receipt.principal_id);
    let truthful_callback =
      let fields = snapshot connection session.id in
      List.exists fields.extension_status ~f:(fun status ->
        P.Extension_status.equal_kind status.kind Moderator_execution
        && String.equal status.state "failed")
    in
    assert truthful_callback;
    print_s [%message (truthful_callback : bool) ~terminal_receipt:(true : bool)]);
  [%test_eq: int] !requests 0;
  [%expect {| ((truthful_callback true) (terminal_receipt true)) |}]
;;

let%test_unit
    "Workflow does not finish at its first real operation and yielding callbacks wake"
  =
  let requests = ref 0 in
  with_host
    ~remove_source:false
    ~prompt:turn_prompt
    ~requests
    ~f:(fun ~env ~host:_ connection ->
      let single = create connection ~key:"single-startup" in
      let completed =
        run connection ~env ~session:single ~request:(Authored_start Single_turn) ()
        |> checked
      in
      (match completed.evidence with
       | Managed_run { terminal; _ } ->
         assert (P.Run.Mode.equal terminal.run.mode Single_turn);
         assert (P.Run.Lifecycle.equal terminal.run.lifecycle (Terminal (Completed None)));
         [%test_eq: int] (List.length terminal.run.owned_work) 1
       | Foreground _ -> failwith "single-turn startup lost its managed owner");
      [%test_eq: int] !requests 1;
      let workflow = create connection ~key:"workflow" in
      let reason = Failure "workflow presentation cancellation" in
      let saw_terminal_operation = ref false in
      let admission_seen = ref None in
      (try
         Eio.Cancel.sub (fun cancel ->
           run
             connection
             ~env
             ~session:workflow
             ~request:(Authored_start Workflow)
             ~on_admission:(fun receipt -> admission_seen := Some receipt)
             ~on_view:(fun projection ->
               Eio.Fiber.yield ();
               match C.Projection.terminal_operation projection with
               | Some { state = Completed; _ } ->
                 assert (Option.is_some !admission_seen);
                 saw_terminal_operation := true;
                 Eio.Cancel.cancel cancel reason
               | _ -> ())
             ()
           |> checked
           |> ignore);
         failwith "workflow incorrectly finished without authored completion"
       with
       | Eio.Cancel.Cancelled actual when phys_equal actual reason -> ());
      assert !saw_terminal_operation;
      assert (Option.is_some !admission_seen);
      let initialized = C.Connection.initialization connection |> Option.value_exn in
      let query =
        P.Run_query.Request.create
          ~session:
            (P.Session_ref.create
               ~server_id:initialized.server_id
               ~session_id:workflow.id)
          ~page:(P.Page.Request.create ~limit:10 () |> protocol_ok)
        |> protocol_ok
      in
      let views = C.Run_views.create connection ~server_id:initialized.server_id in
      let page = C.Run_views.page views query |> protocol_ok in
      (match (List.hd_exn page.items).run.lifecycle with
       | Active | Waiting _ | Admitted -> ()
       | Terminal _ -> failwith "closing presentation changed workflow completion");
      let actual = List.hd_exn page.items in
      assert (Option.equal P.Run_receipt.equal actual.admission_receipt !admission_seen);
      [%test_eq: int] !requests 2)
;;

let%test_unit "ordinary captured source executes one foreground turn, no synthetic Run" =
  let requests = ref 0 in
  with_host
    ~remove_source:false
    ~prompt:"<developer>Ordinary captured definition.</developer>"
    ~requests
    ~f:(fun ~env ~host connection ->
      let session = create connection ~key:"ordinary" in
      let selection = select host connection session in
      assert (Option.is_none selection.moderator);
      [%test_eq: int] !requests 0;
      let completed =
        run connection ~env ~session ~request:(Foreground_continue Start_stopped) ()
        |> checked
      in
      (match completed.evidence with
       | Foreground { operation = { state = Completed; _ }; _ } -> ()
       | _ -> failwith "ordinary completion became a synthetic managed run");
      [%test_eq: int] !requests 1;
      assert (
        String.is_substring completed.chatmd ~substring:"Ordinary captured definition.");
      let selected = create connection ~key:"intervening-attachment" in
      let expected =
        (snapshot connection selected.id).lifecycle
        |> Option.value_exn
        |> P.Session_lifecycle.Observation.expected
      in
      let polls = ref 0 in
      Agent_server.Embedded.session_script_selection host ~expected ~checkpoint:(fun () ->
        Int.incr polls)
      |> protocol_ok
      |> ignore;
      let reason = Failure "actual source query cancellation" in
      (try
         Eio.Cancel.sub (fun cancel ->
           Agent_server.Embedded.session_script_selection
             host
             ~expected
             ~checkpoint:(fun () -> Eio.Cancel.cancel cancel reason)
           |> protocol_ok
           |> ignore);
         failwith "source query delivered after caller cancellation"
       with
       | Eio.Cancel.Cancelled actual when phys_equal actual reason -> ());
      let intervening = ref None in
      let seen = ref 0 in
      let checkpoint () =
        Int.incr seen;
        if Int.equal !seen !polls
        then (
          let request : P.Session.Attach_request.t =
            { session_id = selected.id
            ; requested_mode = Read_only
            ; subscribe = false
            ; after_sequence = None
            ; reclaim_token = None
            ; idempotency_key = P.Idempotency_key.of_string "other-reader" |> protocol_ok
            }
          in
          match
            C.Connection.request connection (Session_attach request) |> protocol_ok
          with
          | Session_attach response -> intervening := Some response.attachment.id
          | _ -> failwith "actual attachment returned another result")
      in
      (match
         Agent_server.Embedded.session_script_selection host ~expected ~checkpoint
       with
       | Error { code = Conflict; _ } -> ()
       | _ -> failwith "final borrowed checkpoint mutation escaped source observation");
      assert (Option.is_some !intervening);
      (match
         run
           connection
           ~env
           ~session:selected
           ~request:(Foreground_continue Start_stopped)
           ()
       with
       | Error (Protocol { code = Conflict; _ }) -> ()
       | _ -> failwith "intervening actual attachment was accepted as unchanged selection");
      assert (Option.is_some !intervening);
      [%test_eq: int] !requests 1)
;;

let%test_unit "real single-turn user submission uses its original managed admission" =
  let requests = ref 0 in
  let prompt =
    {|<developer>Managed single-turn definition.</developer>
<script id="passive" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event = fun ctx state event -> Task.pure(state)
</script>|}
  in
  with_host ~remove_source:false ~prompt ~requests ~f:(fun ~env ~host:_ connection ->
    let selected = create connection ~key:"single-submission" in
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Eio.Switch.run (fun sw ->
        let changed = Eio.Condition.create () in
        let handle =
          C.Session_handle.attach
            ~sw
            ~clock:(Eio.Stdenv.clock env)
            ~connection
            ~session_id:selected.id
            ~mode:Read_write
            ~on_update:(fun _ -> Eio.Condition.broadcast changed)
            ()
          |> protocol_ok
        in
        C.Session_handle.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
        Eio.Condition.loop_no_mutex changed (fun () ->
          let fields =
            C.Session_handle.projection handle
            |> C.Projection.snapshot
            |> P.Public.Snapshot.fields
          in
          Option.some_if
            (List.exists fields.extension_status ~f:(fun status ->
               P.Extension_status.equal_kind status.kind Moderator_execution
               && String.equal status.state "completed"))
            ());
        C.Session_handle.close handle));
    let session = (snapshot connection selected.id).session in
    let views = ref 0 in
    let completed =
      run
        connection
        ~env
        ~session
        ~request:
          (User_submission
             { mode = Single_turn
             ; content =
                 { kind = Plain_text
                 ; text = "An actual admitted user message."
                 ; attachments = []
                 }
             ; activation = Require_running
             })
        ~on_view:(fun _ ->
          Int.incr views;
          Eio.Fiber.yield ())
        ()
      |> checked
    in
    (match completed.evidence with
     | Managed_run
         { terminal =
             { run = { mode = Single_turn; lifecycle = Terminal (Completed _); _ }; _ }
         ; _
         } -> ()
     | _ -> failwith "actual single-turn submission did not complete its Run");
    [%test_eq: int] !requests 1;
    assert (!views > 0);
    assert (
      String.is_substring completed.chatmd ~substring:"An actual admitted user message."))
;;

let%test_unit "same reader rejects closure and overshoot before execution" =
  let requests = ref 0 in
  with_host
    ~remove_source:false
    ~prompt:finished_prompt
    ~requests
    ~f:(fun ~env ~host:_ connection ->
      List.iteri [ true; false ] ~f:(fun index close ->
        let session = create connection ~key:(sprintf "position-boundary-%d" index) in
        let prepare handle selected =
          if close
          then C.Session_handle.close handle
          else (
            C.Session_handle.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
            C.Session_handle.stop handle ~mode:Cancel |> protocol_ok |> ignore);
          Ok (M.Request.Authored_start Single_turn, selected)
        in
        match
          run connection ~env ~session ~request:(Authored_start Single_turn) ~prepare ()
        with
        | Error (Protocol { code; _ })
          when P.Error.equal_code code (if close then Interrupted else Conflict) -> ()
        | Error (Protocol failure) ->
          raise_s
            [%sexp "unexpected boundary error", (index : int), (failure : P.Error.t)]
        | Ok _ | Error _ -> failwith "changed/closed reader authorized execution");
      [%test_eq: int] !requests 0)
;;

let%test_unit "activation preserves callback failure and rejects revoked writer" =
  let requests = ref 0 in
  let prompt =
    {|<developer>Actual retained activation boundary.</developer>
<script id="passive" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = []
let on_event ctx state event = Task.pure(state)
</script>|}
  in
  with_host ~remove_source:false ~prompt ~requests ~f:(fun ~env ~host connection ->
    let session = create connection ~key:"activation-owner-race" in
    let identity = Failure "original activation callback" in
    let prepare handle selected =
      let started =
        C.Session_handle.start handle ~queue_if_limited:false |> protocol_ok
      in
      let polls = ref 0 in
      let checkpoint () =
        incr polls;
        if !polls = 2 then raise identity
      in
      (try
         ignore
           (Agent_server.Embedded.complete_script_activation
              host
              ~started
              ~attachment_id:(C.Session_handle.attachment handle).id
              ~checkpoint);
         failwith "activation callback failure disappeared"
       with
       | exn -> assert (phys_equal exn identity));
      let detached = ref false in
      let polls = ref 0 in
      let checkpoint () =
        incr polls;
        if !polls = 2
        then (
          C.Session_handle.detach handle |> protocol_ok;
          detached := true)
      in
      (match
         Agent_server.Embedded.complete_script_activation
           host
           ~started
           ~attachment_id:(C.Session_handle.attachment handle).id
           ~checkpoint
       with
       | Error { code = Invalid_request; _ } -> ()
       | Error failure ->
         raise_s [%sexp "unexpected activation error", (failure : P.Error.t)]
       | Ok _ -> failwith "revoked attachment passed activation admission");
      assert !detached;
      Error
        (M.Error.Protocol
           (P.Error.create Conflict ~message:"intentional owner race" ~retryable:false ()))
    in
    (match
       run connection ~env ~session ~request:(Authored_start Single_turn) ~prepare ()
     with
     | Error (Protocol { code = Conflict; _ }) -> ()
     | Ok _ | Error _ -> failwith "competing activation authorized run");
    [%test_eq: int] !requests 0)
;;

let%test_unit "lost actual admission reply retains original key and reconciles its run" =
  let requests = ref 0 in
  with_host
    ~remove_source:false
    ~prompt:finished_prompt
    ~requests
    ~f:(fun ~env ~host:_ original ->
      let dropped = ref None in
      let calls = ref 0 in
      let transport =
        C.Transport.create
          ~request:(fun command ->
            let result = C.Connection.request original command in
            match command, result, !dropped with
            | P.Command.Session_run_start request, Ok _, None ->
              incr calls;
              dropped := Some request.key;
              Error
                (P.Error.create
                   Interrupted
                   ~message:"actual committed reply dropped"
                   ~retryable:false
                   ())
            | Session_run_start _, _, Some _ -> failwith "original admission was repeated"
            | _ -> result)
          ~next_notification:(fun () -> C.Connection.next_notification original)
          ~close:ignore
      in
      let connection = C.Connection.create transport in
      Exn.protect
        ~finally:(fun () -> C.Connection.close connection)
        ~f:(fun () ->
          C.Session_handle.initialize
            connection
            ~implementation_name:"original-receipt-fixture"
            ~implementation_version:"1"
          |> protocol_ok
          |> ignore;
          let session = create connection ~key:"lost-actual-admission" in
          let received = ref false in
          (match
             run
               connection
               ~env
               ~session
               ~request:(Authored_start Workflow)
               ~on_admission:(fun _ -> received := true)
               ()
           with
           | Error (Protocol { code = Interrupted; _ }) -> ()
           | Ok _ | Error _ -> failwith "lost actual receipt did not preserve uncertainty");
          assert (not !received);
          let pending = C.Connection.pending_commands connection |> List.hd_exn in
          [%test_eq: int] (List.length (C.Connection.pending_commands connection)) 1;
          let identity = C.Connection.pending_identity pending in
          [%test_eq: string] identity.method_name "session.run.start";
          assert (Option.equal P.Idempotency_key.equal identity.idempotency_key !dropped);
          (match C.Connection.reconcile connection pending |> protocol_ok with
           | Committed (Accepted_run { session_id; receipt }) ->
             assert (P.Id.Session.equal session_id session.id);
             assert (
               Option.value_map
                 !dropped
                 ~default:false
                 ~f:(P.Idempotency_key.equal receipt.key));
             let initialized =
               C.Connection.initialization connection |> Option.value_exn
             in
             let outcome =
               C.Run_views.lookup
                 (C.Run_views.create connection ~server_id:initialized.server_id)
                 { session =
                     P.Session_ref.create ~server_id:initialized.server_id ~session_id
                 ; run_id = receipt.run_id
                 }
               |> protocol_ok
             in
             (match outcome with
              | Available actual ->
                assert (
                  Option.equal P.Run_receipt.equal actual.admission_receipt (Some receipt))
              | Unavailable _ -> failwith "original admitted run unavailable")
           | _ -> failwith "original admission lost its actual run receipt");
          [%test_eq: int] !calls 1;
          assert (List.is_empty (C.Connection.pending_commands connection));
          [%test_eq: int] !requests 0))
;;
