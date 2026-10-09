open! Core
module A = Agent_session
module P = Agent_protocol
module L = Chatml.Chatml_lang

let get = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let source =
  P.Run_source.create
    ~observer:{ script_id = "workflow"; source_sha256 = String.make 64 'a' }
    ~generation:0
    ~installation_epoch:1L
  |> get
;;

let now = P.Timestamp.of_string "2026-10-09T00:00:00Z" |> get

let run =
  P.Run.create
    ~id:(P.Id.Run.of_string "run_scope_fixture" |> get)
    ~session:
      (P.Session_ref.create
         ~server_id:(P.Id.Server.of_string "srv_fixture" |> get)
         ~session_id:(P.Id.Session.of_string "ses_fixture" |> get))
    ~principal_id:(P.Id.Principal.of_string "pri_fixture" |> get)
    ~source
    ~mode:Workflow
    ~lifecycle:Active
    ~revision:1L
    ~owned_work:[]
    ~relinquished_work:[]
    ~terminal_work:[]
    ~created_at:now
    ~updated_at:now
  |> get
;;

let service () =
  let scope =
    A.Run_scope.create
      ~run
      ~execution_id:(P.Id.Moderator_execution.of_string "mex_scope_fixture" |> get)
    |> get
  in
  A.Run_action_service.create ~scope
;;

let string_get = Result.ok_or_failwith

let%expect_test "run staging rejects foreign rollback stale prepared and closed actions" =
  let first = service () in
  let other = service () in
  let transaction = A.Run_action_service.transaction first in
  let other_transaction = A.Run_action_service.transaction other in
  let ticket = transaction.handlers.stage Continue |> string_get in
  let other_ticket = other_transaction.handlers.stage Continue |> string_get in
  let foreign = Result.is_error (transaction.prepare [ other_ticket ]) in
  let first_invalidated = Result.is_error (A.Run_action_service.prepared first) in
  let rolled = service () in
  let rolled_transaction = A.Run_action_service.transaction rolled in
  let rolled_ticket = rolled_transaction.handlers.stage Continue |> string_get in
  rolled_transaction.handlers.rollback rolled_ticket;
  let rolled_rejected = Result.is_error (rolled_transaction.prepare [ rolled_ticket ]) in
  let sealed = service () in
  let sealed_transaction = A.Run_action_service.transaction sealed in
  let sealed_ticket = sealed_transaction.handlers.stage Continue |> string_get in
  ignore
    (sealed_transaction.prepare [ sealed_ticket ] |> string_get : P.Run_action.t option);
  let late_stage = Result.is_error (sealed_transaction.handlers.stage Continue) in
  let mismatch = Result.is_error (A.Run_action_service.verify sealed None) in
  sealed_transaction.handlers.rollback sealed_ticket;
  let rollback_invalidated = Result.is_error (A.Run_action_service.prepared sealed) in
  A.Run_action_service.close other;
  let closed = Result.is_error (other_transaction.prepare [ other_ticket ]) in
  print_s
    [%sexp
      { unique_tickets = (not (Int.equal ticket other_ticket) : bool)
      ; foreign : bool
      ; first_invalidated : bool
      ; rolled_rejected : bool
      ; late_stage : bool
      ; mismatch : bool
      ; rollback_invalidated : bool
      ; closed : bool
      }];
  [%expect
    {|
    ((unique_tickets true) (foreign true) (first_invalidated true)
     (rolled_rejected true) (late_stage true) (mismatch true)
     (rollback_invalidated true) (closed true))
    |}]
;;

let%expect_test "run receipt splitting rejects duplicates and preserves unrelated order" =
  let action : L.eff =
    { op = "Run.continue"; args = [ VVariant ("Run_receipt", [ VInt 7; VUnit ]) ] }
  in
  let ordinary name : L.eff = { op = name; args = [] } in
  let receipts, ordinary =
    Chat_response.Run_operations.split_actions
      [ ordinary "first"; action; ordinary "second" ]
    |> string_get
  in
  print_s
    [%sexp
      { receipts : int list
      ; ordinary = (List.map ordinary ~f:(fun operation -> operation.L.op) : string list)
      ; duplicate_rejected =
          (Result.is_error (Chat_response.Run_operations.split_actions [ action; action ])
           : bool)
      ; malformed_rejected =
          (Result.is_error
             (Chat_response.Run_operations.split_actions [ { action with args = [] } ])
           : bool)
      }];
  [%expect
    {|
    ((receipts (7)) (ordinary (first second)) (duplicate_rejected true)
     (malformed_rejected true))
    |}]
;;

let%expect_test
    "repreparation invalidates the old selection and foreign wake cannot stage"
  =
  let owner = service () in
  let transaction = A.Run_action_service.transaction owner in
  let ticket = transaction.handlers.stage Continue |> string_get in
  ignore (transaction.prepare [ ticket ] |> string_get : P.Run_action.t option);
  let repeated = Result.is_error (transaction.prepare [ ticket ]) in
  let old_selection_gone = Result.is_error (A.Run_action_service.prepared owner) in
  let fresh = service () in
  let source =
    P.Run_source.create ~observer:source.observer ~generation:0 ~installation_epoch:2L
    |> get
  in
  let wake =
    P.Run_wake.create
      ~run_id:run.id
      ~source
      ~occurrence:
        (Job_completion
           { job_id = P.Id.Job.of_string "job_foreign_wake" |> get; attempt = 0 })
    |> get
  in
  let foreign_wake =
    Result.is_error ((A.Run_action_service.transaction fresh).handlers.stage (Wait wake))
  in
  print_s [%sexp { repeated : bool; old_selection_gone : bool; foreign_wake : bool }];
  [%expect {| ((repeated true) (old_selection_gone true) (foreign_wake true)) |}]
;;

let%expect_test "foreign rollback preserves another callback's prepared selection" =
  let owner = service () in
  let other = service () in
  let transaction = A.Run_action_service.transaction owner in
  let other_transaction = A.Run_action_service.transaction other in
  let ticket = transaction.handlers.stage Continue |> string_get in
  let foreign_ticket = other_transaction.handlers.stage Continue |> string_get in
  ignore (transaction.prepare [ ticket ] |> string_get : P.Run_action.t option);
  transaction.handlers.rollback foreign_ticket;
  transaction.handlers.rollback foreign_ticket;
  let foreign_preserved =
    Result.is_ok (A.Run_action_service.verify owner (Some Continue))
  in
  transaction.handlers.rollback ticket;
  transaction.handlers.rollback ticket;
  print_s
    [%sexp
      { foreign_preserved : bool
      ; own_invalidated = (Result.is_error (A.Run_action_service.prepared owner) : bool)
      }];
  [%expect {| ((foreign_preserved true) (own_invalidated true)) |}]
;;

let%expect_test "sealed run action composes with actual runtime requests" =
  let requests : P.Invocation.follow_up =
    { request_turn = false; request_compaction = true; end_session = None }
  in
  let prepared action =
    let owner = service () in
    let transaction = A.Run_action_service.transaction owner in
    let ticket = transaction.handlers.stage action |> string_get in
    ignore (transaction.prepare [ ticket ] |> string_get : P.Run_action.t option);
    owner
  in
  let continue = prepared Continue in
  let continued =
    A.Run_action_service.compose_requests continue ~action:(Some Continue) ~requests
    |> string_get
  in
  let finish_action : P.Run_action.t =
    Finish { terminal = Completed None; relinquish = [] }
  in
  let finish = prepared finish_action in
  let turn_rejected =
    Result.is_error
      (A.Run_action_service.compose_requests
         finish
         ~action:(Some finish_action)
         ~requests:{ requests with request_turn = true })
  in
  let ending = { requests with end_session = Some "explicit session stop" } in
  let stop_preserved =
    A.Run_action_service.compose_requests
      finish
      ~action:(Some finish_action)
      ~requests:ending
    |> string_get
    |> fun result -> P.Invocation.equal_follow_up result ending
  in
  let continue_stop_rejected =
    Result.is_error
      (A.Run_action_service.compose_requests
         continue
         ~action:(Some Continue)
         ~requests:ending)
  in
  print_s
    [%sexp
      { continued : P.Invocation.follow_up
      ; turn_rejected : bool
      ; stop_preserved : bool
      ; continue_stop_rejected : bool
      }];
  [%expect
    {|
    ((continued ((request_turn true) (request_compaction true) (end_session ())))
     (turn_rejected true) (stop_preserved true) (continue_stop_rejected true))
    |}]
;;
