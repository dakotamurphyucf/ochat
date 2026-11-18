open! Core
module P = Agent_protocol
module G = P.Goal
module D = Document_schema
module Store = Agent_session.Goal_document

let ok = function
  | Ok value -> value
  | Error (e : P.Error.t) -> failwith e.message
;;

let stored = function
  | Ok value -> value
  | Error e -> failwith (Sexp.to_string_hum (D.Error.sexp_of_t e))
;;

let now = P.Timestamp.of_string "2026-11-03T00:00:00Z" |> ok

let source ?(epoch = 1L) () =
  P.Run_source.create
    ~observer:{ script_id = "goal"; source_sha256 = String.make 64 'a' }
    ~generation:0
    ~installation_epoch:epoch
  |> ok
;;

let session =
  P.Session_ref.create
    ~server_id:(P.Id.Server.of_string "srv_goal" |> ok)
    ~session_id:(P.Id.Session.of_string "ses_goal" |> ok)
;;

let creator = P.Id.Principal.of_string "pri_goal" |> ok

let goal () =
  G.create
    ~id:(P.Id.Goal.of_string "goal_fixture" |> ok)
    ~session
    ~creator
    ~source:(source ())
    ~objective:(G.Objective.of_string "Qualify the artifact" |> ok)
    ~budget:G.Budget.unlimited
  |> ok
;;

let reason = G.Reason.of_string "Evidence collected" |> ok

let run
      ?(id = "run_goal")
      ?(epoch = 1L)
      ?(owned_work = [])
      ?(relinquished_work = [])
      ?(principal_id = creator)
      ?(session = session)
      ?run_source
      revision
      lifecycle
  =
  P.Run.create
    ~id:(P.Id.Run.of_string id |> ok)
    ~session
    ~principal_id
    ~source:(Option.value run_source ~default:(source ~epoch ()))
    ~mode:Workflow
    ~lifecycle
    ~revision
    ~owned_work
    ~relinquished_work
    ~terminal_work:[]
    ~created_at:now
    ~updated_at:now
  |> ok
;;

let receipt ?session_revision ?action (run : P.Run.t) kind =
  P.Run_receipt.create
    ~run_id:run.id
    ~principal_id:run.principal_id
    ~source:run.source
    ~key:
      (P.Idempotency_key.of_string
         (P.Id.Run.to_string run.id ^ Sexp.to_string (P.Run_receipt.Kind.sexp_of_t kind))
       |> ok)
    ~request_sha256:
      (let json =
         match kind with
         | P.Run_receipt.Kind.Admission -> `Null
         | Action ->
           P.Run_action.to_json
             (Option.value
                action
                ~default:
                  (P.Run_action.Finish { terminal = Completed None; relinquish = [] }))
         | Terminal ->
           (match run.lifecycle with
            | Terminal terminal -> P.Run.Terminal.to_json terminal
            | Admitted | Active | Waiting _ -> assert false)
       in
       Jsonaf.to_string json |> Digestif.SHA256.digest_string |> Digestif.SHA256.to_hex)
    ~kind
    ~run_revision:run.revision
    ~session_revision:(Option.value session_revision ~default:run.revision)
    ~committed_at:now
  |> ok
;;

let bound () =
  let admitted = run 0L Admitted in
  let episode =
    G.Episode.create ~run:admitted ~admission:(receipt admitted Admission) |> ok
  in
  G.bind_episode (goal ()) episode ~previous:None |> ok, episode
;;

let requested () =
  let goal, episode = bound () in
  let active = run 1L Active in
  let request =
    G.Achievement_request.create
      ~episode
      ~run:active
      ~receipt:(receipt active Action)
      ~action:(Finish { terminal = Completed None; relinquish = [] })
      ~evidence:reason
    |> ok
  in
  G.request_achievement goal request |> ok, request
;;

let set json name value =
  match json with
  | `Object fields ->
    `Object
      ((name, value)
       :: List.filter fields ~f:(fun (key, _) -> not (String.equal key name)))
  | _ -> failwith "object expected"
;;

let field json name =
  match D.Json.field json ~name with
  | Value value -> value
  | Absent | Null -> failwith name
;;

let%expect_test
    "achievement request waits for actual Completed and adverse evidence stays unachieved"
  =
  let goal, request = requested () in
  let changed_objective = G.Objective.of_string "Different pending objective" |> ok in
  (match G.revise_objective goal changed_objective with
   | Error error -> [%test_eq: P.Error.code] error.code Conflict
   | Ok _ -> assert false);
  let edited =
    G.of_json
      (set
         (set (G.to_json goal) "objective" (G.Objective.to_json changed_objective))
         "revision"
         (`String (Int64.to_string (Int64.succ (G.revision goal)))))
    |> ok
  in
  assert (Result.is_error (G.validate_update goal ~incoming:edited));
  assert (
    Result.is_ok (G.record_progress goal (G.Progress.of_string "Still pending" |> ok)));
  assert (
    match G.state goal with
    | Open _ -> true
    | Closed _ -> false);
  List.iter
    [ P.Run.Terminal.Failed None; Cancelled; Limited; Interrupted ]
    ~f:(fun terminal ->
      let ended = run 2L (Terminal terminal) in
      assert (
        Result.is_error
          (G.Achievement.create request ~run:ended ~receipt:(receipt ended Terminal)));
      let held =
        G.retire_request goal ~run:ended ~receipt:(receipt ended Terminal) |> ok
      in
      assert (
        match G.state held with
        | Open (Held _) -> true
        | Open Enabled | Closed _ -> false);
      let paused = G.hold goal Operator_pause |> ok in
      let settled_pause =
        G.retire_request paused ~run:ended ~receipt:(receipt ended Terminal) |> ok
      in
      assert (
        match G.state settled_pause with
        | Open (Held Operator_pause) -> true
        | _ -> false));
  let completed = run 2L (Terminal (Completed None)) in
  let achievement =
    G.Achievement.create request ~run:completed ~receipt:(receipt completed Terminal)
    |> ok
  in
  let achieved = G.achieve goal achievement |> ok in
  assert (
    match G.state achieved with
    | Closed (Achieved _) -> true
    | Closed (Abandoned _) | Open _ -> false);
  assert (Result.is_error (G.resume achieved));
  assert (Result.is_error (G.achieve achieved achievement));
  assert (G.equal achieved (G.of_json (G.to_json achieved) |> ok));
  print_endline
    "requested is open; four adverse outcomes stay open; actual completion closes once";
  [%expect
    {| requested is open; four adverse outcomes stay open; actual completion closes once |}]
;;

let%expect_test
    "bounded independent command model preserves closed state and accounting identity"
  =
  let commands = [ `Pause; `Resume; `Edit; `Abandon ] in
  let identity = G.id (goal ()) in
  let rec walk depth closed runnable expected_revision goal =
    assert (Int64.equal (G.revision goal) expected_revision);
    assert (P.Id.Goal.equal (G.id goal) identity);
    assert (
      match closed, runnable, G.state goal with
      | true, _, Closed (Abandoned _)
      | false, true, Open Enabled
      | false, false, Open (Held _) -> true
      | _ -> false);
    if depth > 0
    then
      List.iter commands ~f:(fun command ->
        let result =
          match command with
          | `Pause -> G.hold goal Operator_pause
          | `Resume -> G.resume goal
          | `Edit ->
            G.revise_objective goal (G.Objective.of_string "Revised objective" |> ok)
          | `Abandon -> G.abandon goal reason
        in
        match closed, result with
        | true, Error _ -> ()
        | true, Ok _ | false, Error _ -> failwith "reference model disagreement"
        | false, Ok next ->
          walk
            (depth - 1)
            (match command with
             | `Abandon -> true
             | `Pause | `Resume | `Edit -> false)
            (match command with
             | `Pause -> false
             | `Resume -> true
             | `Edit | `Abandon -> runnable)
            (Int64.succ expected_revision)
            next)
  in
  walk 4 false false 0L (goal ());
  print_endline
    "all bounded command sequences preserve closure, revision and goal identity";
  [%expect
    {| all bounded command sequences preserve closure, revision and goal identity |}]
;;

let%expect_test
    "source adoption retains prior achievement evidence and permits a new actual episode"
  =
  let goal, request = requested () in
  let interrupted = run 2L (Terminal Interrupted) in
  let goal =
    G.retire_request goal ~run:interrupted ~receipt:(receipt interrupted Terminal) |> ok
  in
  let goal = G.adopt_source goal (source ~epoch:2L ()) |> ok in
  assert (
    Option.equal G.Achievement_request.equal (G.achievement_request goal) (Some request));
  let next_run = run ~id:"run_next" ~epoch:2L 0L Admitted in
  let next_episode =
    G.Episode.create
      ~run:next_run
      ~admission:(receipt ~session_revision:2L next_run Admission)
    |> ok
  in
  let next =
    G.bind_episode
      goal
      next_episode
      ~previous:(Some (interrupted, receipt interrupted Terminal))
    |> ok
  in
  assert (
    Option.equal G.Achievement_request.equal (G.achievement_request next) (Some request));
  let old_completed = run 3L (Terminal (Completed None)) in
  let old_achievement =
    G.Achievement.create
      request
      ~run:old_completed
      ~receipt:(receipt old_completed Terminal)
    |> ok
  in
  assert (Result.is_error (G.achieve next old_achievement));
  assert (G.equal next (G.of_json (G.to_json next) |> ok));
  print_endline
    "new source and episode retain old receipt; old completion cannot close the new \
     episode";
  [%expect
    {| new source and episode retain old receipt; old completion cannot close the new episode |}]
;;

let%expect_test
    "text, budget, identity and checked revision boundaries are admitted honestly"
  =
  List.iter
    [ ""; " \n"; "\000"; "\255"; String.make ((8 * 1024) + 1) 'a' ]
    ~f:(fun text -> assert (Result.is_error (G.Objective.of_string text)));
  assert (Result.is_ok (G.Progress.of_string ""));
  assert (Result.is_ok (G.Objective.of_string "Unicode λ\nline"));
  assert (
    Result.is_error
      (G.Budget.create
         ~admitted_turns:(Some (-1L))
         ~admitted_attempts:None
         ~observed_tokens:None
         ~deadline:None));
  let valid_id = G.id (goal ()) in
  let make id creator session =
    G.create
      ~id
      ~creator
      ~session
      ~source:(source ())
      ~objective:(G.Objective.of_string "Valid objective" |> ok)
      ~budget:G.Budget.unlimited
  in
  let invalid_goal = P.Id.Goal.t_of_sexp (Sexp.Atom "run_wrong") in
  let invalid_creator = P.Id.Principal.t_of_sexp (Sexp.Atom "ses_wrong") in
  let invalid_session =
    P.Session_ref.create
      ~server_id:(P.Session_ref.server_id session)
      ~session_id:(P.Id.Session.t_of_sexp (Sexp.Atom "srv_wrong"))
  in
  let invalid_server =
    P.Session_ref.create
      ~server_id:(P.Id.Server.t_of_sexp (Sexp.Atom "ses_wrong"))
      ~session_id:(P.Session_ref.session_id session)
  in
  List.iter
    [ make invalid_goal creator session
    ; make valid_id invalid_creator session
    ; make valid_id creator invalid_session
    ; make valid_id creator invalid_server
    ]
    ~f:(fun result -> assert (Result.is_error result));
  let deep =
    List.fold (List.init 10000 ~f:Fn.id) ~init:`Null ~f:(fun json _ -> `Array [ json ])
  in
  assert (Result.is_error (G.of_json (set (G.to_json (goal ())) "deep_unknown" deep)));
  let raw = G.to_json (goal ()) in
  let exhausted =
    G.of_json (set raw "revision" (`String (Int64.to_string Int64.max_value))) |> ok
  in
  assert (Result.is_error (G.resume exhausted));
  assert (Result.is_error (G.of_json (set raw "revision" (`String "01"))));
  assert (Result.is_error (G.of_json (set raw "id" (`String "run_wrong"))));
  let changed =
    G.of_json (set (set raw "id" (`String "goal_other")) "revision" (`String "1")) |> ok
  in
  assert (Result.is_error (G.validate_update (goal ()) ~incoming:changed));
  print_endline
    "invalid text/negative ceilings/noncanonical counters/foreign identity rejected; \
     overflow never wraps";
  [%expect
    {| invalid text/negative ceilings/noncanonical counters/foreign identity rejected; overflow never wraps |}]
;;

let%expect_test
    "universal carrier preserves compatible unknowns and rejects unsafe ownership changes"
  =
  let original =
    Store.create (goal ()) |> stored |> Store.to_document |> stored |> D.Document.json
  in
  let payload = field original "payload" in
  let source = set (field payload "source") "future_source" (`String "retained") in
  let payload =
    set (set payload "source" source) "future_goal" (`Object [ "opaque", `Null ])
  in
  let original =
    set (set original "payload" payload) "future_envelope" (`String "retained")
  in
  let restored =
    D.Document.inspect ~limits:Store.limits original
    |> stored
    |> Store.of_document
    |> stored
  in
  let edited =
    G.record_progress
      (Store.value restored)
      (G.Progress.of_string "Observed evidence" |> ok)
    |> ok
  in
  let encoded =
    Store.update restored edited
    |> stored
    |> Store.to_document
    |> stored
    |> D.Document.json
  in
  assert (D.Json.equal (field encoded "future_envelope") (`String "retained"));
  assert (
    D.Json.equal
      (field (field encoded "payload") "future_goal")
      (`Object [ "opaque", `Null ]));
  assert (
    D.Json.equal
      (field (field (field encoded "payload") "source") "future_source")
      (`String "retained"));
  let absent =
    match payload with
    | `Object fields ->
      `Object (List.filter fields ~f:(fun (name, _) -> not (String.equal name "episode")))
    | _ -> assert false
  in
  assert (Result.is_error (G.of_json absent));
  let held_state = field payload "state" in
  let held_state =
    set
      held_state
      "admission"
      (set (field held_state "admission") "future_hold" (`String "protected"))
  in
  let protected =
    D.Document.inspect
      ~limits:Store.limits
      (set original "payload" (set payload "state" held_state))
    |> stored
    |> Store.of_document
    |> stored
  in
  assert (
    Result.is_error (Store.update protected (G.resume (Store.value protected) |> ok)));
  (* A promised future hold/closure must remain compatible at every pending or
     Enabled carrier admission, not merely at the original Finish request. *)
  List.iter
    [ G.resume (goal ()) |> ok; fst (requested ()) ]
    ~f:(fun goal ->
      let original =
        Store.create goal |> stored |> Store.to_document |> stored |> D.Document.json
      in
      let payload = field original "payload" in
      let state = field payload "state" in
      let protected_state =
        set
          state
          "admission"
          (set (field state "admission") "future_variant" (`String "protected"))
      in
      let incoming =
        D.Document.inspect
          ~limits:Store.limits
          (set original "payload" (set payload "state" protected_state))
        |> stored
      in
      assert (Result.is_error (Store.of_document incoming)));
  print_endline
    "nested/envelope unknowns survive progress edit; required absence and unsafe variant \
     change reject";
  [%expect
    {| nested/envelope unknowns survive progress edit; required absence and unsafe variant change reject |}]
;;

let%expect_test
    "bounded tamper matrix cannot bypass receipt evidence or carrier update invariants"
  =
  let requested_goal, request = requested () in
  let completed = run 2L (Terminal (Completed None)) in
  let achievement =
    G.Achievement.create request ~run:completed ~receipt:(receipt completed Terminal)
    |> ok
  in
  let bad_settlement =
    set
      (field (G.Achievement.to_json achievement) "settlement")
      "receipt"
      (set
         (field (field (G.Achievement.to_json achievement) "settlement") "receipt")
         "request_sha256"
         (`String (String.make 64 '0')))
  in
  assert (
    Result.is_error
      (G.Achievement.of_json
         (set (G.Achievement.to_json achievement) "settlement" bad_settlement)));
  let failed = run 2L (Terminal (Failed None)) in
  let valid_failed =
    G.Episode.Settlement.create
      (G.Achievement_request.episode request)
      ~run:failed
      ~receipt:(receipt failed Terminal)
    |> ok
  in
  assert (
    Result.is_error
      (G.Achievement.of_json
         (set
            (G.Achievement.to_json achievement)
            "settlement"
            (G.Episode.Settlement.to_json valid_failed))));
  let raw = G.to_json requested_goal in
  let next_revision =
    `String (Int64.to_string (Int64.succ (G.revision requested_goal)))
  in
  let candidate json =
    G.of_json (set json "revision" next_revision) |> ok |> Store.create |> stored
  in
  let previous = Store.create requested_goal |> stored in
  let erased =
    raw |> fun j -> set (set j "achievement_request" `Null) "request_settlement" `Null
  in
  let source_enabled =
    set
      (set raw "source" (P.Run_source.to_json (source ~epoch:2L ())))
      "state"
      (`Object
          [ "kind", `String "open"; "admission", `Object [ "kind", `String "enabled" ] ])
  in
  let other_run = run ~id:"run_unproven" 0L Admitted in
  let other_episode =
    G.Episode.create ~run:other_run ~admission:(receipt other_run Admission) |> ok
  in
  List.iter [ erased; source_enabled ] ~f:(fun tampered ->
    assert (Result.is_error (Store.adopt previous ~incoming:(candidate tampered))));
  let plain, _ = bound () in
  let plain_raw = G.to_json plain in
  let replaced_episode =
    G.of_json
      (set
         (set plain_raw "episode" (G.Episode.to_json other_episode))
         "revision"
         (`String (Int64.to_string (Int64.succ (G.revision plain)))))
    |> ok
  in
  assert (
    Result.is_error
      (Store.adopt
         (Store.create plain |> stored)
         ~incoming:(Store.create replaced_episode |> stored)));
  let resolved =
    G.retire_request requested_goal ~run:failed ~receipt:(receipt failed Terminal) |> ok
  in
  let resolved_raw = G.to_json resolved in
  let cleared =
    G.of_json
      (set
         (set resolved_raw "request_settlement" `Null)
         "revision"
         (`String (Int64.to_string (Int64.succ (G.revision resolved)))))
    |> ok
  in
  assert (Result.is_error (Store.update (Store.create resolved |> stored) cleared));
  let enabled = G.resume requested_goal |> ok in
  let settled_enabled =
    G.retire_request enabled ~run:failed ~receipt:(receipt failed Terminal) |> ok
  in
  let enabled_tamper =
    G.of_json
      (set
         (G.to_json settled_enabled)
         "state"
         (`Object
             [ "kind", `String "open"
             ; "admission", `Object [ "kind", `String "enabled" ]
             ]))
    |> ok
  in
  assert (
    Result.is_error
      (Store.adopt
         (Store.create enabled |> stored)
         ~incoming:(Store.create enabled_tamper |> stored)));
  let prior_proof =
    G.Episode.Settlement.create
      (G.Achievement_request.episode request)
      ~run:failed
      ~receipt:(receipt failed Terminal)
    |> ok
  in
  let historical_episode =
    G.of_json
      (set
         (set
            (set (G.to_json plain) "episode" (G.Episode.to_json other_episode))
            "previous_episode_settlement"
            (G.Episode.Settlement.to_json prior_proof))
         "revision"
         (`String (Int64.to_string (Int64.succ (G.revision plain)))))
  in
  assert (Result.is_error historical_episode);
  let request_json = G.Achievement_request.to_json request in
  let earlier_action =
    set
      request_json
      "receipt"
      (set (field request_json "receipt") "run_revision" (`String "0"))
  in
  let settlement_json = G.Episode.Settlement.to_json valid_failed in
  let earlier_terminal =
    set
      settlement_json
      "receipt"
      (set (field settlement_json "receipt") "run_revision" (`String "0"))
  in
  assert (Result.is_error (G.Achievement_request.of_json earlier_action));
  assert (Result.is_error (G.Episode.Settlement.of_json earlier_terminal));
  let b_admitted = run ~id:"run_episode_b" 0L Admitted in
  let b_episode =
    G.Episode.create
      ~run:b_admitted
      ~admission:(receipt ~session_revision:2L b_admitted Admission)
    |> ok
  in
  let b_active = run ~id:"run_episode_b" 1L Active in
  let b_request =
    G.Achievement_request.create
      ~episode:b_episode
      ~run:b_active
      ~receipt:(receipt ~session_revision:3L b_active Action)
      ~action:(Finish { terminal = Completed None; relinquish = [] })
      ~evidence:reason
    |> ok
  in
  let b_failed = run ~id:"run_episode_b" 2L (Terminal (Failed None)) in
  let b_terminal = receipt ~session_revision:4L b_failed Terminal in
  let b_settlement =
    G.Episode.Settlement.create b_episode ~run:b_failed ~receipt:b_terminal |> ok
  in
  let foreign_request =
    G.of_json
      (set
         (set
            (set
               (G.to_json resolved)
               "achievement_request"
               (G.Achievement_request.to_json b_request))
            "request_settlement"
            (G.Episode.Settlement.to_json b_settlement))
         "revision"
         (`String (Int64.to_string (Int64.succ (G.revision resolved)))))
    |> ok
  in
  let resolved_store = Store.create resolved |> stored in
  assert (Result.is_error (Store.update resolved_store foreign_request));
  assert (
    Result.is_error
      (Store.adopt resolved_store ~incoming:(Store.create foreign_request |> stored)));
  let b_bound =
    G.bind_episode resolved b_episode ~previous:(Some (failed, receipt failed Terminal))
    |> ok
  in
  let cancelled = run 2L (Terminal Cancelled) in
  let cancelled_receipt = receipt cancelled Terminal in
  assert (
    Result.is_error
      (G.bind_episode resolved b_episode ~previous:(Some (cancelled, cancelled_receipt))));
  let altered_admission =
    P.Run_receipt.of_json
      (set
         (P.Run_receipt.to_json
            (G.Episode.admission (G.Achievement_request.episode request)))
         "key"
         (`String "alternate_admission"))
    |> ok
  in
  let altered_episode =
    G.Episode.create ~run:(run 0L Admitted) ~admission:altered_admission |> ok
  in
  let contradictory_proofs =
    List.map
      [ G.Achievement_request.episode request; altered_episode ]
      ~f:(fun episode ->
        G.Episode.Settlement.create episode ~run:cancelled ~receipt:cancelled_receipt
        |> ok)
  in
  let b_document =
    Store.create b_bound |> stored |> Store.to_document |> stored |> D.Document.json
  in
  List.iter contradictory_proofs ~f:(fun proof ->
    let contradictory =
      set
        (G.to_json b_bound)
        "previous_episode_settlement"
        (G.Episode.Settlement.to_json proof)
    in
    assert (Result.is_error (G.of_json contradictory));
    let document =
      D.Document.inspect ~limits:Store.limits (set b_document "payload" contradictory)
      |> stored
    in
    let adopted =
      Result.bind (Store.of_document document) ~f:(fun incoming ->
        Store.adopt resolved_store ~incoming)
    in
    assert (Result.is_error adopted));
  let b_requested = G.request_achievement b_bound b_request |> ok in
  assert (Result.is_ok (Store.update (Store.create b_bound |> stored) b_requested));
  let b_settled = G.retire_request b_requested ~run:b_failed ~receipt:b_terminal |> ok in
  assert (G.equal b_settled (G.of_json (G.to_json b_settled) |> ok));
  print_endline
    "tampered terminal digest, Failed-as-achieved, request erasure, enabled adoption, \
     unproved episode, terminal erasure and contradictory same-Run proofs reject";
  [%expect
    {| tampered terminal digest, Failed-as-achieved, request erasure, enabled adoption, unproved episode, terminal erasure and contradictory same-Run proofs reject |}]
;;

let%expect_test "request admission reserves settlement, metadata and carrier capacity" =
  let largest make prefix character =
    let rec loop count previous =
      assert (count < 4096);
      let text = prefix ^ String.make count character in
      match make text with
      | Ok _ -> loop (count + 1) (Some text)
      | Error _ -> Option.value_exn previous
    in
    loop 1 None
  in
  let identity make prefix = make (largest make prefix 'a') |> ok in
  let extreme_session =
    P.Session_ref.create
      ~server_id:(identity P.Id.Server.of_string "srv_")
      ~session_id:(identity P.Id.Session.of_string "ses_")
  in
  let principal_id = identity P.Id.Principal.of_string "pri_" in
  let run_id = largest P.Id.Run.of_string "run_" 'a' in
  let extreme_time =
    P.Timestamp.of_time_ns (Time_ns.of_int63_ns_since_epoch Int63.max_value)
  in
  let make_source script_id generation installation_epoch =
    P.Run_source.create
      ~observer:{ script_id; source_sha256 = String.make 64 'a' }
      ~generation
      ~installation_epoch
  in
  let script_id = largest (fun text -> make_source text 0 1L) "" '\000' in
  let initial_source = make_source script_id 0 1L |> ok in
  let future_source = make_source script_id Int.max_value Int64.max_value |> ok in
  let extreme_run
        ?(id = run_id)
        ?(owned_work = [])
        ?(relinquished_work = [])
        revision
        lifecycle
    =
    run
      ~id
      ~owned_work
      ~relinquished_work
      ~principal_id
      ~session:extreme_session
      ~run_source:initial_source
      revision
      lifecycle
  in
  let extreme_receipt ?action run kind =
    let key = largest P.Idempotency_key.of_string "" 'a' in
    let json =
      receipt ?action ~session_revision:Int64.max_value run kind |> P.Run_receipt.to_json
    in
    P.Run_receipt.of_json
      (set
         (set json "key" (`String key))
         "committed_at"
         (P.Timestamp.to_json extreme_time))
    |> ok
  in
  let admitted = extreme_run 0L Admitted in
  let episode =
    G.Episode.create ~run:admitted ~admission:(extreme_receipt admitted Admission) |> ok
  in
  let bound_goal =
    G.create
      ~id:(identity P.Id.Goal.of_string "goal_")
      ~session:extreme_session
      ~creator:principal_id
      ~source:initial_source
      ~objective:(G.Objective.of_string (String.make G.Objective.max_bytes '"') |> ok)
      ~budget:G.Budget.unlimited
    |> ok
    |> fun goal -> G.bind_episode goal episode ~previous:None |> ok
  in
  let make_request count =
    let work =
      List.init count ~f:(fun index ->
        let id =
          P.Id.Job.of_string ("job_" ^ String.make 80 'a' ^ Int.to_string index) |> ok
        in
        P.Run_work.create ~key:(Retained (Job { id; attempt = 1 })) ~generation:0 |> ok)
    in
    let active = extreme_run ~owned_work:work (Int64.pred Int64.max_value) Active in
    let action = P.Run_action.Finish { terminal = Completed None; relinquish = work } in
    let request =
      G.Achievement_request.create
        ~episode
        ~run:active
        ~receipt:(extreme_receipt ~action active Action)
        ~action
        ~evidence:reason
      |> ok
    in
    request, work
  in
  let large, _ = make_request 1024 in
  assert (Result.is_error (G.request_achievement bound_goal large));
  let no_headroom, _ = make_request 512 in
  assert (
    String.length (Jsonaf.to_string (G.Achievement_request.to_json no_headroom))
    < G.Capacity.max_projection_bytes);
  assert (Result.is_error (G.request_achievement bound_goal no_headroom));
  let near, work = make_request 256 in
  assert (
    String.length (Jsonaf.to_string (G.Achievement_request.to_json near)) > 30 * 1024);
  let requested_goal = G.request_achievement bound_goal near |> ok in
  let with_revision goal revision =
    G.of_json (set (G.to_json goal) "revision" (`String (Int64.to_string revision)))
  in
  let held_edge = with_revision requested_goal (Int64.pred Int64.max_value) |> ok in
  assert (Result.is_error (with_revision requested_goal Int64.max_value));
  assert (Result.is_error (G.resume held_edge));
  assert (Result.is_ok (G.abandon held_edge reason));
  let enabled = G.resume requested_goal |> ok in
  let enabled_edge = with_revision enabled Int64.(max_value - 2L) |> ok in
  assert (Result.is_error (with_revision enabled (Int64.pred Int64.max_value)));
  let held_edge = G.hold enabled_edge Source_review |> ok in
  let adverse_edge =
    extreme_run ~relinquished_work:work Int64.max_value (Terminal Interrupted)
  in
  assert (
    Result.is_ok
      (G.retire_request
         held_edge
         ~run:adverse_edge
         ~receipt:(extreme_receipt adverse_edge Terminal)));
  let reserved = G.reserved_capacity requested_goal |> ok in
  let within_reservation goal =
    let actual = G.Capacity.measure (G.to_json goal) |> ok in
    assert (G.Capacity.encoded_bytes actual <= G.Capacity.encoded_bytes reserved);
    assert (G.Capacity.fields actual <= G.Capacity.fields reserved);
    assert (G.Capacity.nodes actual <= G.Capacity.nodes reserved)
  in
  let unrequested =
    Store.create bound_goal |> stored |> Store.to_document |> stored |> D.Document.json
  in
  let occupied =
    set unrequested "future_envelope" (`String (String.make (170 * 1024) 'a'))
  in
  let occupied =
    D.Document.inspect ~limits:Store.limits occupied
    |> stored
    |> Store.of_document
    |> stored
  in
  assert (Result.is_error (Store.update occupied requested_goal));
  let budget =
    G.Budget.create
      ~admitted_turns:(Some Int64.max_value)
      ~admitted_attempts:(Some Int64.max_value)
      ~observed_tokens:
        (Some
           (G.Budget.Token_stop.create
              ~component:Cache_write_input
              ~ceiling:Int64.max_value
            |> ok))
      ~deadline:(Some extreme_time)
    |> ok
  in
  let sized =
    G.record_progress
      requested_goal
      (G.Progress.of_string (String.make G.Progress.max_bytes '"') |> ok)
    |> ok
    |> fun goal ->
    G.change_budget goal budget
    |> ok
    |> fun goal ->
    G.adopt_source goal future_source
    |> ok
    |> fun goal ->
    G.hold
      goal
      (Needs_input (G.Reason.of_string (String.make G.Reason.max_bytes '"') |> ok))
    |> ok
    |> fun goal ->
    G.of_json
      (set
         (G.to_json goal)
         "revision"
         (`String (Int64.to_string (Int64.pred (Int64.pred Int64.max_value)))))
    |> ok
  in
  List.iter [ requested_goal; sized ] ~f:within_reservation;
  let original =
    Store.create sized |> stored |> Store.to_document |> stored |> D.Document.json
  in
  let with_unknown = set original "future_envelope" (`String (String.make 1024 'a')) in
  let carrier =
    D.Document.inspect ~limits:Store.limits with_unknown
    |> stored
    |> Store.of_document
    |> stored
  in
  let completed =
    extreme_run ~relinquished_work:work Int64.max_value (Terminal (Completed None))
  in
  let proof =
    G.Achievement.create near ~run:completed ~receipt:(extreme_receipt completed Terminal)
    |> ok
  in
  let achieved = G.achieve sized proof |> ok in
  within_reservation achieved;
  let closed =
    Store.update carrier achieved
    |> stored
    |> Store.to_document
    |> stored
    |> D.Document.json
  in
  assert (
    D.Json.equal (field closed "future_envelope") (field with_unknown "future_envelope"));
  assert (G.equal achieved (G.of_json (G.to_json achieved) |> ok));
  assert (
    D.Json.equal
      (field (G.to_json achieved) "state")
      (`Object [ "kind", `String "achieved" ]));
  let failed =
    extreme_run ~relinquished_work:work Int64.max_value (Terminal (Failed None))
  in
  let retired =
    G.retire_request sized ~run:failed ~receipt:(extreme_receipt failed Terminal) |> ok
  in
  within_reservation retired;
  let retired_carrier = Store.update carrier retired |> stored in
  let next_run =
    run
      ~id:(String.chop_suffix_exn run_id ~suffix:"a" ^ "b")
      ~principal_id
      ~session:extreme_session
      ~run_source:future_source
      0L
      Admitted
  in
  let next_episode =
    G.Episode.create ~run:next_run ~admission:(extreme_receipt next_run Admission) |> ok
  in
  let rebound =
    G.bind_episode
      retired
      next_episode
      ~previous:(Some (failed, extreme_receipt failed Terminal))
    |> ok
  in
  within_reservation rebound;
  let rebound_json =
    Store.update retired_carrier rebound
    |> stored
    |> Store.to_document
    |> stored
    |> D.Document.json
  in
  assert (
    D.Json.equal
      (field rebound_json "future_envelope")
      (field with_unknown "future_envelope"));
  assert (G.equal rebound (G.of_json (G.to_json rebound) |> ok));
  let compact =
    Store.create requested_goal
    |> stored
    |> Store.to_document
    |> stored
    |> D.Document.json
  in
  let fill =
    G.Capacity.max_document_bytes - String.length (Jsonaf.to_string compact) - 128
  in
  let full = set compact "future_envelope" (`String (String.make fill 'a')) in
  let full = D.Document.inspect ~limits:Store.limits full |> stored in
  assert (Result.is_error (Store.of_document full));
  print_endline
    "oversize rejects before staging; accepted request reserves escaped metadata, \
     identities and receipt extremes; closes or retires and rebinds with unknowns";
  [%expect
    {| oversize rejects before staging; accepted request reserves escaped metadata, identities and receipt extremes; closes or retires and rebinds with unknowns |}]
;;
