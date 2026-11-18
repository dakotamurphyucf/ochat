open! Core
module P = Agent_protocol
module C = Agent_client
module W = P.Goal_control
module Q = P.Goal_query
module H = C.Session_handle
module Fixture = Support.Config_fixture

let prompt =
  {|<developer>Offline Goal operator conformance.</developer>
<tool name="goal_get"/>
<script id="conformance_goal" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let field value key = match Json.get_field(value, key) with
| `Some(found) -> found
| `None -> `Null
let goal_row read = match field(read, "goals") with
| `Array(rows) -> if Array.length(rows) == 1 then rows[0] else `Null
| _ -> `Null
let on_event ctx state event = match event with
| `Session_start ->
  let* read = Tool.call("goal_get", `Object([{key = "goal_id"; value = `Null}])) in
  (match read with
  | `Error(message) -> Task.fail(message)
  | `Ok(view) ->
    let run = field(goal_row(view), "current_run") in
    let* id = Schedule.after_ms(600000, `String("goal-operator conformance wait")) in
    let* schedule = Schedule.get(id) in
    let owner = field(schedule, "ownership") in
    let wake = `Object([
      {key = "run_id"; value = field(run, "run_id")},
      {key = "source"; value = field(view, "source")},
      {key = "occurrence"; value = `Object([
        {key = "kind"; value = `String("delivered_timer")},
        {key = "schedule_id"; value = `String(id)},
        {key = "delivery_count"; value = `Number(1.0)},
        {key = "creator"; value = `Object([
          {key = "kind"; value = field(owner, "creator_type")},
          {key = "id"; value = field(owner, "creator_id")}
        ])}
      ])}
    ]) in
    let* ignored = Run.wait(wake) in
    Task.pure(state + 1))
| _ -> Task.pure(state)
</script>|}
;;

let fail message = raise_s [%sexp "Goal operator conformance", (message : string)]
let check condition message = if not condition then fail message

let checked = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "Goal operator protocol error", (error : P.Error.t)]
;;

let reject (result : (_, P.Error.t) Result.t) expected =
  match result with
  | Error error when P.Error.equal_code error.code expected -> ()
  | Error error ->
    raise_s
      [%sexp "unexpected Goal rejection", (expected : P.Error.code), (error : P.Error.t)]
  | Ok _ -> fail "Goal control unexpectedly succeeded"
;;

let key value = P.Idempotency_key.of_string ("goal-conformance:" ^ value) |> checked
let goal_id = P.Id.Goal.of_string "goal_transport_conformance" |> checked

module Dropped_reply = struct
  type t =
    { connection : C.Connection.t
    ; drop_next : string option ref
    ; dropped : P.Method_result.t option ref
    }

  let create physical =
    let drop_next = ref None in
    let dropped = ref None in
    let request command =
      match C.Connection.request physical command with
      | Ok (P.Public.Result.Non_history result)
        when Option.exists !drop_next ~f:(String.equal (P.Command.method_name command)) ->
        (* The real socket, dispatcher and writer have produced a correlated
           acknowledgement. Lose it before the typed caller can observe it. *)
        drop_next := None;
        dropped := Some (P.Public.Result.Non_history.value result);
        C.Connection.close physical;
        Error
          (P.Error.create
             Interrupted
             ~message:"test discarded a reply"
             ~retryable:true
             ())
      | result -> result
    in
    let connection =
      C.Transport.create
        ~request
        ~next_notification:(fun () -> C.Connection.next_notification physical)
        ~close:(fun () -> C.Connection.close physical)
      |> C.Connection.create
    in
    { connection; drop_next; dropped }
  ;;

  let lose t ~method_ f =
    t.drop_next := Some method_;
    t.dropped := None;
    reject (f t.connection) Interrupted;
    Option.value_or_thunk !(t.dropped) ~default:(fun () -> fail "no real reply was lost")
  ;;
end

let initialize connection =
  let initialized = Support.Unix_driver.initialize connection |> checked in
  check
    (List.mem initialized.enabled_features Q.Features.inspect ~equal:String.equal
     && List.mem initialized.enabled_features W.Features.control ~equal:String.equal)
    "host did not negotiate installed Goal inspection and controls";
  initialized
;;

let with_handle ~sw ~clock connection ~session_id ~mode f =
  let handle =
    H.attach ~sw ~clock ~connection ~session_id ~mode ~subscribe:false () |> checked
  in
  Exn.protect
    ~finally:(fun () -> H.close handle)
    ~f:(fun () -> f handle (H.goals handle |> checked))
;;

let view = function
  | Q.Result.Available view -> view
  | Unavailable _ -> fail "Goal view was unavailable"
;;

let one controls =
  let basis = C.Goal_controls.get controls goal_id |> checked |> view in
  match basis.goals with
  | [ row ] -> basis, row
  | _ -> fail "Goal lookup did not return the actual Goal"
;;

let source (basis : Q.View.t) =
  Option.value_or_thunk basis.source ~default:(fun () ->
    fail "Goal source is unavailable")
;;

let command (basis : Q.View.t) ?expected_goal_revision ~source ~key mutation =
  P.Goal_command.create
    ~session_id:(P.Session_ref.session_id basis.session)
    ~generation:basis.generation
    ~source
    ~expected_catalog_revision:basis.catalog_revision
    ~expected_goal_revision
    ~key
    ~mutation
  |> checked
;;

let start (basis : Q.View.t) (row : Q.View.row) ~source ~key =
  W.Start.create
    ~generation:basis.generation
    ~expected_revision:basis.session_revision
    ~source
    ~goal_id
    ~expected_goal_revision:row.goal.revision
    ~expected_catalog_revision:basis.catalog_revision
    ~input:Authored_start
    ~key
  |> checked
;;

let resume (basis : Q.View.t) (row : Q.View.row) ~run_id ~run_revision ~key =
  P.Goal_resume_request.create
    ~session_id:(P.Session_ref.session_id basis.session)
    ~generation:basis.generation
    ~goal_id
    ~run_id
    ~source:(source basis)
    ~expected_goal_revision:row.goal.revision
    ~expected_catalog_revision:basis.catalog_revision
    ~expected_run_revision:run_revision
    ~key
  |> checked
;;

let run_revision (row : Q.View.row) run_id =
  match row.run with
  | Observed { run_id = actual; revision; status = Waiting } ->
    check (P.Id.Run.equal actual run_id) "Goal points to another Run";
    revision
  | No_episode | Unavailable | Observed _ ->
    raise_s
      [%sexp
        "Goal Run did not remain in its real passive wait"
      , (row.run : Q.Run_observation.t)]
;;

let check_effects connection ~session (row : Q.View.row) =
  let page = P.Page.Request.create ~limit:10 () |> checked in
  let runs = C.Run_views.create connection ~server_id:(P.Session_ref.server_id session) in
  let request = P.Run_query.Request.create ~session ~page |> checked in
  let runs = C.Run_views.page runs request |> checked in
  check (List.length runs.items = 1) "Goal replay duplicated Run admission";
  (match
     C.Connection.request_without_history
       connection
       (Schedule_list
          { session_id = P.Session_ref.session_id session; page; status = None })
     |> checked
   with
   | Schedule_list page ->
     check (List.length page.items = 1) "Goal replay duplicated timer"
   | _ -> fail "expected schedule list");
  match row.accounting with
  | Some accounting ->
    check
      (Int64.equal accounting.admitted.attempts 0L
       && Int64.equal accounting.pending.attempts 0L)
      "offline Goal workflow unexpectedly admitted or reserved model work"
  | None -> fail "new Goal lacked its canonical accounting"
;;

let check_catalog_capacity controls original_create original_receipt =
  let list () = C.Goal_controls.list controls |> checked |> view in
  let additional_goal number =
    P.Id.Goal.of_string (sprintf "goal_transport_capacity_%d" number) |> checked
  in
  let objective = P.Goal.Objective.of_string "Check catalog capacity" |> checked in
  let create basis number =
    command
      basis
      ~source:(Some (source basis))
      ~key:(key (sprintf "capacity-%d" number))
      (Create { id = additional_goal number; objective; budget = P.Goal.Budget.unlimited })
  in
  List.init 15 ~f:Fn.id
  |> List.iter ~f:(fun number ->
    let basis = list () in
    check basis.can_create "the catalog declined advice before reaching capacity";
    ignore
      (C.Goal_controls.command controls (create basis number) |> checked
       : P.Goal_command_receipt.t));
  let full = list () in
  check
    (List.length full.goals = 16 && not full.can_create)
    "the full catalog still advised a new Goal";
  let existing =
    List.find_exn full.goals ~f:(fun row ->
      P.Id.Goal.equal row.goal.id (additional_goal 14))
  in
  check
    (List.mem existing.actions Record_progress ~equal:Q.Action.equal)
    "a full Goal catalog suppressed edits to an existing Goal";
  reject (C.Goal_controls.command controls (create full 15)) Resource_limit;
  let replayed = C.Goal_controls.command controls original_create |> checked in
  check
    (P.Goal_command_receipt.equal original_receipt replayed)
    "a full Goal catalog lost the original Create receipt";
  check
    (Int64.equal full.catalog_revision (list ()).catalog_revision)
    "a rejected or retained Create advanced the catalog"
;;

let check ~sw ~env fixture ~spec =
  let clock = Eio.Stdenv.clock env in
  let socket () =
    Support.Unix_driver.connect ~sw ~env ~socket_path:(Fixture.unix_socket fixture)
  in
  let first = socket () |> Dropped_reply.create in
  let initialized = initialize first.connection in
  let created =
    H.create
      ~sw
      ~clock
      ~connection:first.connection
      ~spec
      ~mode:Read_write
      ~subscribe:false
      ()
    |> checked
  in
  let session_id = H.session_id created in
  let session = P.Session_ref.create ~server_id:initialized.server_id ~session_id in
  let controls = H.goals created |> checked in
  let initial = C.Goal_controls.list controls |> checked |> view in
  check
    (initial.can_create && List.is_empty initial.goals)
    "new writer cannot create a Goal";
  let create =
    let objective =
      P.Goal.Objective.of_string "Complete offline operator qualification" |> checked
    in
    command
      initial
      ~source:(Some (source initial))
      ~key:(key "create")
      (Create { id = goal_id; objective; budget = P.Goal.Budget.unlimited })
  in
  let foreign =
    Agent_transport_http.Client.connect
      ~sw
      ~env
      ~uri:(Uri.of_string (sprintf "http://127.0.0.1:%d" (Fixture.http_port fixture)))
      ~bearer_token:(Some (Fixture.public_token fixture))
      ~notification_capacity:64
    |> checked
  in
  let foreign_identity = initialize foreign in
  check
    (not (P.Id.Principal.equal foreign_identity.principal.id initialized.principal.id))
    "HTTP test principal is not distinct from the Unix creator";
  let original_writer = (H.attachment created).id in
  let stolen connection id =
    C.Goal_controls.create connection ~session ~attachment:(fun () -> id) ()
  in
  reject
    (C.Goal_controls.command (stolen foreign original_writer) create)
    Permission_denied;
  C.Connection.close foreign;
  let other = socket () in
  ignore (initialize other : P.Initialize.Response.t);
  reject (C.Goal_controls.command (stolen other original_writer) create) Permission_denied;
  let other_session =
    H.create ~sw ~clock ~connection:other ~spec ~mode:Read_write ~subscribe:false ()
    |> checked
  in
  reject
    (C.Goal_controls.command (stolen other (H.attachment other_session).id) create)
    Permission_denied;
  H.close other_session;
  C.Connection.close other;
  let created_receipt = C.Goal_controls.command controls create |> checked in
  let basis, row = one controls in
  check
    (Int64.equal row.goal.revision created_receipt.goal_revision)
    "Goal creation receipt did not persist";
  let progress text = P.Goal.Progress.of_string text |> checked in
  let edit ~key text =
    command
      basis
      ~expected_goal_revision:row.goal.revision
      ~source:(Some (source basis))
      ~key
      (Record_progress (goal_id, progress text))
  in
  let edit_command = edit ~key:(key "edit") "durable operator progress" in
  let edit_receipt = C.Goal_controls.command controls edit_command |> checked in
  reject
    (C.Goal_controls.command
       controls
       (edit ~key:(key "edit") "changed request with old key"))
    Conflict;
  reject
    (C.Goal_controls.command controls (edit ~key:(key "stale") "stale basis"))
    Conflict;
  let basis, row = one controls in
  check
    (Int64.equal row.goal.revision edit_receipt.goal_revision
     && P.Goal.Progress.equal row.goal.progress (progress "durable operator progress"))
    "rejected writes changed the committed Goal edit";
  let reader = socket () in
  ignore (initialize reader : P.Initialize.Response.t);
  with_handle ~sw ~clock reader ~session_id ~mode:Read_only (fun _ read_only ->
    reject (C.Goal_controls.command read_only create) Permission_denied;
    let basis, row = one read_only in
    let intent = start basis row ~source:(source basis) ~key:(key "readonly-start") in
    reject (C.Goal_controls.start read_only intent) Permission_denied);
  C.Connection.close reader;
  (* Acquire the Start fence after all attach/detach mutations. *)
  let basis, row = one controls in
  let current_source = source basis in
  let wrong_source =
    P.Run_source.create
      ~observer:current_source.observer
      ~generation:current_source.generation
      ~installation_epoch:(Int64.succ current_source.installation_epoch)
    |> checked
  in
  reject
    (C.Goal_controls.start
       controls
       (start basis row ~source:wrong_source ~key:(key "wrong-source")))
    Conflict;
  let start_intent = start basis row ~source:current_source ~key:(key "start") in
  let admitted =
    match
      Dropped_reply.lose first ~method_:"session.goal.start" (fun _ ->
        C.Goal_controls.start controls start_intent)
    with
    | Goal_control (Start (Activated admission)) -> admission.receipt
    | Goal_control (Start (Activation_failed { error; _ } | Admission_uncertain error)) ->
      raise_s [%sexp "Goal startup did not activate", (error : P.Error.t)]
    | result -> raise_s [%sexp "unexpected Goal Start", (result : P.Method_result.t)]
  in
  H.close created;
  let second = socket () |> Dropped_reply.create in
  let reconnected = initialize second.connection in
  check
    (P.Id.Principal.equal reconnected.principal.id initialized.principal.id)
    "socket principal changed on reconnect";
  let resume_intent, resumed_receipt, waiting_revision, after_resume_catalog =
    with_handle
      ~sw
      ~clock
      second.connection
      ~session_id
      ~mode:Read_write
      (fun writer controls ->
         check
           (not (P.Id.Attachment.equal original_writer (H.attachment writer).id))
           "reconnect reused the old writer";
         (match C.Goal_controls.start controls start_intent |> checked with
          | Retained admission ->
            check
              (P.Run_receipt.equal admitted admission.receipt)
              "Start replay changed the original admission"
          | _ -> fail "Start replay activated another episode");
         let _, row = one controls in
         let waiting_revision =
           Eio.Time.with_timeout_exn clock 10. (fun () ->
             let rec await row =
               match row.Q.View.run with
               | Observed { status = Waiting; _ } -> run_revision row admitted.run_id
               | Observed { status = Admitted | Active; _ } ->
                 Eio.Fiber.yield ();
                 await (snd (one controls))
               | Observed _ | No_episode | Unavailable ->
                 fail "Goal's offline callback did not preserve its Run"
             in
             await row)
         in
         let basis, row = one controls in
         let hold =
           command
             basis
             ~expected_goal_revision:row.goal.revision
             ~source:None
             ~key:(key "hold")
             (Hold (goal_id, Operator_pause))
         in
         ignore
           (C.Goal_controls.command controls hold |> checked : P.Goal_command_receipt.t);
         let basis, row = one controls in
         (match row.goal.state with
          | Open (Held Operator_pause) -> ()
          | _ -> fail "Hold did not preserve a paused Goal");
         check
           (Int64.equal waiting_revision (run_revision row admitted.run_id))
           "Hold cancelled or advanced the waiting Run";
         check_effects second.connection ~session row;
         let stale =
           resume
             basis
             row
             ~run_id:admitted.run_id
             ~run_revision:(Int64.succ waiting_revision)
             ~key:(key "stale-resume")
         in
         (match C.Goal_controls.resume controls stale with
          | (Ok (Rejected error) | Error error)
            when P.Error.equal_code error.code Conflict -> ()
          | _ -> fail "stale Run revision reached Resume acknowledgement");
         let reader = socket () in
         ignore (initialize reader : P.Initialize.Response.t);
         with_handle ~sw ~clock reader ~session_id ~mode:Read_only (fun _ read_only ->
           let basis, row = one read_only in
           let intent =
             resume
               basis
               row
               ~run_id:admitted.run_id
               ~run_revision:waiting_revision
               ~key:(key "readonly-resume")
           in
           reject (C.Goal_controls.resume read_only intent) Permission_denied);
         C.Connection.close reader;
         let basis, row = one controls in
         let intent =
           resume
             basis
             row
             ~run_id:admitted.run_id
             ~run_revision:waiting_revision
             ~key:(key "resume")
         in
         let receipt =
           match
             Dropped_reply.lose second ~method_:"session.goal.resume" (fun _ ->
               C.Goal_controls.resume controls intent)
           with
           | Goal_control (Resume (Resumed { receipt; continuation = Waiting })) ->
             receipt
           | Goal_control
               (Resume (Resume_failed { error; _ } | Rejected error | Uncertain error)) ->
             raise_s
               [%sexp
                 "Goal Resume did not acknowledge the passive continuation"
               , (error : P.Error.t)]
           | result ->
             raise_s [%sexp "unexpected Goal Resume", (result : P.Method_result.t)]
         in
         intent, receipt, waiting_revision, Int64.succ basis.catalog_revision)
  in
  let last = socket () in
  ignore (initialize last : P.Initialize.Response.t);
  with_handle ~sw ~clock last ~session_id ~mode:Read_write (fun _ controls ->
    (match C.Goal_controls.resume controls resume_intent |> checked with
     | Retained receipt ->
       check
         (P.Goal_command_receipt.equal resumed_receipt receipt)
         "Resume replay changed the first acknowledgement"
     | _ -> fail "Resume replay repeated continuation handoff");
    let basis, row = one controls in
    check
      (Int64.equal basis.catalog_revision after_resume_catalog
       && Int64.equal row.goal.revision resumed_receipt.goal_revision
       && Int64.equal (run_revision row admitted.run_id) waiting_revision)
      "Resume replay changed catalog, Goal or Run revision";
    (match row.goal.state with
     | Open Enabled -> ()
     | _ -> fail "the connected client did not observe the acknowledged Resume");
    check
      (List.exists row.execution_blockers ~f:(function
         | Active_run -> true
         | _ -> false)
       && not
            (List.exists row.execution_blockers ~f:(function
               | No_current_run -> true
               | _ -> false)))
      "advice for the enabled waiting Run claimed its current Run was absent";
    check_effects last ~session row;
    let reason = P.Goal.Reason.of_string "operator closed the objective" |> checked in
    let abandon =
      command
        basis
        ~expected_goal_revision:row.goal.revision
        ~source:None
        ~key:(key "abandon")
        (Abandon (goal_id, reason))
    in
    ignore
      (C.Goal_controls.command controls abandon |> checked : P.Goal_command_receipt.t);
    let _, row = one controls in
    (match row.goal.state with
     | Abandoned actual when P.Goal.Reason.equal actual reason -> ()
     | _ -> fail "Abandon did not record the final objective disposition");
    check
      (Int64.equal (run_revision row admitted.run_id) waiting_revision)
      "Abandon rewrote or cancelled the Run";
    check_effects last ~session row;
    check_catalog_capacity controls create created_receipt);
  C.Connection.close last
;;
