open! Core
open Agent_server_test_support
open Factory_catalog_publication_tests
open Retry_support
open Goal_native_test_support
module Checks = Goal_native_resume_tests
module Fault = Goal_native_fault_support

(* Only this crash fixture performs a hard process exit. Production cleanup is
   unchanged: a poisoned terminal checkpoint is not called a successful join. *)
let () =
  let test_root =
    match Array.to_list (Sys.get_argv ()) with
    | [ _; root ] -> root
    | _ -> failwith "native fault child requires its parent-owned test root"
  in
  let fault = Fault.create Native_complete in
  with_callback
    ~test_root
    ~v1:true
    ~wrap_env:(Fault.wrap fault)
    (fun
        env
         _daemon
         entry
         runtime
         counters
         _session
         _admission
         _old_live
         held
         request
         operation
         ~release_ordinary:_
         ~resumed_entered
         ~release_resumed:_
         ~reopen:_
       ->
       Fault.arm fault entry request;
       let native_failure = ref None in
       let result, _ =
         callback
           ~native_failure
           entry
           runtime
           ~operation
           ~mode:"resume"
           ~run:(fun admission ->
             Actor.stage_native_goal_resume entry.actor ~admission ~authorize ~request)
       in
       assert (Result.is_error result);
       let primary = Option.value_exn !native_failure in
       [%test_eq: P.Error.code] Persistence_error primary.code;
       let transaction : Agent_store.Transaction.t = Fault.await fault in
       [%test_eq: int] 1 fault.hits;
       assert (G.equal (Goal.goal held) (Goal.goal (state entry)));
       Checks.no_accepted entry;
       [%test_eq: int] 2 !(counters.providers);
       assert (Option.is_none (Eio.Promise.peek resumed_entered));
       let selected =
         A.Session_delta_document.decode ~limits:D.Limits.default transaction.delta
         |> document_ok
         |> A.Session_delta_document.value
         |> Goal_admission_fault_tests.atoms
         |> List.filter_map ~f:(function
           | Invocation_changed invocation
             when Option.is_some invocation.P.Invocation.native_goal_resume ->
             Some invocation
           | _ -> None)
       in
       let invocation = List.hd_exn selected in
       [%test_eq: int] 1 (List.length selected);
       let report =
         `Object
           [ "primary", P.Error.to_json primary
           ; "session_id", P.Id.Session.to_json transaction.session_id
           ; "invocation_id", P.Id.Invocation.to_json invocation.context.id
           ]
       in
       Eio.Flow.copy_string (Jsonaf.to_string report ^ "\n") (Eio.Stdenv.stdout env);
       Core_unix.exit_immediately 73)
;;
