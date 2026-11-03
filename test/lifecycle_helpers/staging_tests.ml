open! Core
module A = Agent_session
module P = Agent_protocol
module H = Chatml_host_runtime
module L = Chatml.Chatml_lang
module M = Chat_response.Moderation

let get = Result.ok_or_failwith

let protocol_get = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let service () =
  let now = P.Timestamp.of_string "2026-11-03T00:00:00Z" |> protocol_get in
  let source =
    P.Run_source.create
      ~observer:{ script_id = "lifecycle"; source_sha256 = String.make 64 'a' }
      ~generation:0
      ~installation_epoch:1L
    |> protocol_get
  in
  let run =
    P.Run.create
      ~id:(P.Id.Run.of_string "run_lifecycle_fixture" |> protocol_get)
      ~session:
        (P.Session_ref.create
           ~server_id:(P.Id.Server.of_string "srv_fixture" |> protocol_get)
           ~session_id:(P.Id.Session.of_string "ses_fixture" |> protocol_get))
      ~principal_id:(P.Id.Principal.of_string "pri_fixture" |> protocol_get)
      ~source
      ~mode:Workflow
      ~lifecycle:Active
      ~revision:1L
      ~owned_work:[]
      ~relinquished_work:[]
      ~terminal_work:[]
      ~created_at:now
      ~updated_at:now
    |> protocol_get
  in
  let scope =
    A.Run_scope.create
      ~run
      ~execution_id:
        (P.Id.Moderator_execution.of_string "mex_lifecycle_fixture" |> protocol_get)
    |> protocol_get
  in
  A.Run_action_service.create ~scope
;;

let instantiate program config =
  H.instantiate_prepared_session_detailed
    config
    program
    ~entrypoints:{ initial_state_name = "initial_state"; on_event_name = "on_event" }
    ()
  |> Result.map_error ~f:(fun diagnostic -> diagnostic.Chatml_diagnostic.message)
  |> get
;;

let context event =
  M.Entry_projection.project_context
    ~session_id:"ses_fixture"
    ~now_ms:0
    ~phase:(M.Event.phase event)
    ~history:[]
    ~available_tools:[]
    ~session_meta:`Null
  |> M.Context.to_value
;;

let%expect_test "two checked components stage one scoped action then an explicit finish" =
  List.iter
    [ ( Chatml.Chatml_compiler_contract.Target.Moderator_v1
      , Chatml.Chatml_extension_surface.moderator_v1 )
    ; Delegated_moderator_v1, Chatml.Chatml_extension_surface.delegated_moderator_v1
    ]
    ~f:(fun (target, surface) ->
      let _, program =
        Support.build ~target ~entry:Support.example ~example_modules:true
      in
      let active = ref None in
      let stages = ref 0 in
      let dynamic = Chat_response.Run_operations.dynamic_handlers (fun () -> !active) in
      let handlers : Chat_response.Run_operations.handlers =
        { stage =
            (fun action ->
              incr stages;
              dynamic.stage action)
        ; stage_goal_finish = dynamic.stage_goal_finish
        ; rollback = dynamic.rollback
        }
      in
      let config =
        Chat_response.Run_operations.install ~handlers (Support.runtime surface)
      in
      let session = instantiate program config in
      [%test_eq: int] !stages 0;
      let initial = H.current_state session in
      let expected =
        L.VRecord
          (String.Map.of_alist_exn
             [ "schema", L.VInt 1
             ; "config", L.VRecord (String.Map.singleton "revision" (L.VInt 1))
             ; "counter", L.VRecord (String.Map.singleton "remaining" (L.VInt 1))
             ; "review", L.VRecord (String.Map.singleton "seen" (L.VBool false))
             ])
      in
      assert (L.equal_value initial expected);
      let require_action owner action expected =
        assert (Option.equal P.Run_action.equal action (Some expected));
        A.Run_action_service.verify owner action |> get
      in
      let callback event expected =
        let owner = service () in
        let transaction = A.Run_action_service.transaction owner in
        active := Some transaction;
        Exn.protect
          ~finally:(fun () ->
            active := None;
            A.Run_action_service.close owner)
          ~f:(fun () ->
            let before = !stages in
            H.handle_event
              session
              ~context:(context event)
              ~event:(M.Event.to_value event)
              ~prepare_transaction:(fun proposed ->
                let receipts, other =
                  Chat_response.Run_operations.split_actions proposed.local_effects |> get
                in
                [%test_eq: int] (List.length receipts) 1;
                [%test_eq: int] (List.length other) 0;
                let action = transaction.prepare receipts |> get in
                require_action owner action expected;
                Ok ignore)
            |> get;
            [%test_eq: int] (!stages - before) 1)
      in
      callback M.Event.Session_start Continue;
      callback M.Event.Turn_end (Finish { terminal = Completed None; relinquish = [] });
      (* A selected action is staging evidence, not a fabricated terminal receipt.
         Existing actor commit/worker-join suites own terminal settlement. *)
      let state = H.current_state session in
      let snapshot = Chatml.Chatml_value_codec.Snapshot.of_value state |> get in
      let restored = Chatml.Chatml_value_codec.Snapshot.to_value snapshot |> get in
      assert (L.equal_value state restored));
  print_endline
    "moderator + delegated: inert import; one continue; one explicit finish; typed \
     snapshot";
  [%expect
    {| moderator + delegated: inert import; one continue; one explicit finish; typed snapshot |}]
;;

let valid_wake () =
  let owner = service () in
  Exn.protect
    ~finally:(fun () -> A.Run_action_service.close owner)
    ~f:(fun () ->
      let scope = A.Run_action_service.scope owner in
      P.Run_wake.create
        ~run_id:(A.Run_scope.run_id scope)
        ~source:(A.Run_scope.source scope)
        ~occurrence:
          (Job_completion
             { job_id = P.Id.Job.of_string "job_lifecycle_fixture" |> protocol_get
             ; attempt = 0
             })
      |> protocol_get
      |> P.Run_wake.to_json)
;;

let json_entry () =
  sprintf
    {|let initial_state = `Null
let on_event = fun ctx state event ->
  let wake = Json.parse(%S) in
  let same = Json.parse(%S) in
  let different = Json.set_field(wake, "run_id", `String("run_other")) in
  let component = fun id value -> {id = id; obligation = `Active; action = `Wait(value)} in
  let identical = match Lifecycle_run.compose([component("left", wake), component("right", same)]) with
    | `Ok(plan) -> true | `Error(error) -> false
  in
  let conflicting = match Lifecycle_run.compose([component("left", wake), component("right", different)]) with
    | `Ok(plan) -> false
    | `Error(error) -> match error with | `Composition(policy) -> true | `Wake_limit_exceeded -> false
  in
  let limited = fun components -> match Lifecycle_run.compose(components) with
    | `Ok(plan) -> false
    | `Error(error) -> match error with | `Wake_limit_exceeded -> true | `Composition(policy) -> false
  in
  let values : json array = Array.make(1, `Null) in
  let cyclic : json = `Array(values) in
  let ignored = values[0] <- cyclic in
  let rec grow count text = if count == 0 then text else grow(count - 1, text ++ text) in
  let big = grow(17, "a") in
  let rec nest count value = if count == 0 then value else nest(count - 1, `Array([value])) in
  Task.pure(`Object([
    {key = "valid_wakes_equal"; value = `Bool(identical)},
    {key = "different_wakes_conflict"; value = `Bool(conflicting)},
    {key = "cycle"; value = `Bool(limited([component("cycle", cyclic)]))},
    {key = "nodes"; value = `Bool(limited([component("nodes", `Array(Array.make(4096, `Null)))]))},
    {key = "depth"; value = `Bool(limited([component("depth", nest(33, `Null))]))},
    {key = "string_bytes"; value = `Bool(limited([component("bytes", `String(big))]))},
    {key = "key_bytes"; value = `Bool(limited([component("key", `Object([{key = big; value = `Null}]))]))},
    {key = "preflight_before_conflict"; value = `Bool(limited([component("left", wake), component("right", different), component("cycle", cyclic)]))}
  ]))|}
    (Jsonaf.to_string (valid_wake ()))
    (Jsonaf.to_string (valid_wake ()))
;;

let%expect_test
    "JSON adapter accepts genuine wake envelopes and bounds complete preflight"
  =
  let surface = Chatml.Chatml_extension_surface.moderator_v1 in
  let _, program =
    Support.build ~target:Moderator_v1 ~entry:(json_entry ()) ~example_modules:true
  in
  let session = instantiate program (Support.runtime surface) in
  H.handle_event
    session
    ~context:(context M.Event.Session_start)
    ~event:(M.Event.to_value M.Event.Session_start)
  |> get;
  Chatml.Chatml_value_codec.value_to_jsonaf_exn (H.current_state session)
  |> Jsonaf.to_string
  |> print_endline;
  [%expect
    {| {"valid_wakes_equal":true,"different_wakes_conflict":true,"cycle":true,"nodes":true,"depth":true,"string_bytes":true,"key_bytes":true,"preflight_before_conflict":true} |}]
;;

let blocked_finish_entry =
  {|let initial_state = false
let on_event = fun ctx state event ->
  let completed = `Object([
    {key = "kind"; value = `String("finish")},
    {key = "terminal"; value = `Object([{key = "kind"; value = `String("completed")}])},
    {key = "relinquish"; value = `Array([])}]) in
  match Lifecycle_run.compose([{id = "peer"; obligation = `Active; action = `No_action}]) with
  | `Error(error) -> Task.pure(false)
  | `Ok(plan) ->
      Task.bind(Lifecycle_run.stage(plan.action), fun ignored ->
        Task.map(Lifecycle_run.finish(plan, completed), fun result ->
          match result with
          | `Ok(ignored) -> false
          | `Error(error) -> match error with
            | `Active_components(ids) -> Array.length(ids) == 1
            | _ -> false))|}
;;

let%test_unit "neutral staging and an active peer finish gate make no native request" =
  let surface = Chatml.Chatml_extension_surface.moderator_v1 in
  let _, program =
    Support.build ~target:Moderator_v1 ~entry:blocked_finish_entry ~example_modules:true
  in
  let owner = service () in
  Exn.protect
    ~finally:(fun () -> A.Run_action_service.close owner)
    ~f:(fun () ->
      let calls = ref 0 in
      let transaction = A.Run_action_service.transaction owner in
      let handlers : Chat_response.Run_operations.handlers =
        { transaction.handlers with
          stage =
            (fun action ->
              incr calls;
              transaction.handlers.stage action)
        }
      in
      let config =
        Chat_response.Run_operations.install ~handlers (Support.runtime surface)
      in
      let session = instantiate program config in
      H.handle_event
        session
        ~context:(context M.Event.Session_start)
        ~event:(M.Event.to_value M.Event.Session_start)
      |> get;
      [%test_eq: int] !calls 0;
      assert (L.equal_value (H.current_state session) (L.VBool true)))
;;

let%expect_test
    "authored Goal.finish uses checked native staging and transactional Catch rollback"
  =
  let intent =
    P.Goal_achievement.Intent.create
      ~expected_goal_revision:3L
      ~evidence:(P.Goal.Reason.of_string "Explicit completion evidence" |> protocol_get)
      ~action:(Finish { terminal = Completed None; relinquish = [] })
    |> protocol_get
  in
  List.iter
    [ ( Chatml.Chatml_compiler_contract.Target.Moderator_v1
      , Chatml.Chatml_extension_surface.moderator_v1 )
    ; Delegated_moderator_v1, Chatml.Chatml_extension_surface.delegated_moderator_v1
    ]
    ~f:(fun (target, surface) ->
      List.iter [ false; true ] ~f:(fun rollback ->
        let body =
          if rollback
          then
            "Task.catch(Task.bind(Goal.finish(intent), fun ignored -> \
             Task.fail(\"discard\")), fun message -> Task.pure(true))"
          else "Task.map(Goal.finish(intent), fun ignored -> true)"
        in
        let entry =
          sprintf
            "let initial_state = false\n\
             let on_event ctx state event =\n\
            \  let intent = Json.parse(%S) in\n\
            \  %s"
            (P.Goal_achievement.Intent.to_json intent |> Jsonaf.to_string)
            body
        in
        let _, program = Support.build ~target ~entry ~example_modules:false in
        let owner = service () in
        Exn.protect
          ~finally:(fun () -> A.Run_action_service.close owner)
          ~f:(fun () ->
            let transaction = A.Run_action_service.transaction owner in
            let session =
              instantiate
                program
                (Chat_response.Run_operations.install
                   ~handlers:transaction.handlers
                   (Support.runtime surface))
            in
            H.handle_event
              session
              ~context:(context M.Event.Session_start)
              ~event:(M.Event.to_value M.Event.Session_start)
              ~prepare_transaction:(fun proposed ->
                let receipts, ordinary =
                  Chat_response.Run_operations.split_actions proposed.local_effects |> get
                in
                assert (List.is_empty ordinary);
                [%test_eq: int] (List.length receipts) (if rollback then 0 else 1);
                ignore (transaction.prepare receipts |> get : P.Run_action.t option);
                assert (
                  Option.equal
                    P.Goal_achievement.Intent.equal
                    (A.Run_action_service.prepared_goal_finish owner |> get)
                    (if rollback then None else Some intent));
                Ok ignore)
            |> get;
            assert (L.equal_value (H.current_state session) (L.VBool true)))));
  print_endline
    "both moderator surfaces: checked Goal.finish; Catch discards staged intent; no \
     completion claim";
  [%expect
    {| both moderator surfaces: checked Goal.finish; Catch discards staged intent; no completion claim |}]
;;
