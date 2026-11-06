open! Core
open Agent_server_test_support
module Team = Documentation_agent_team_tests
module P = Agent_protocol
module A = Agent_session
module Actor = A.Session_actor
module D = Agent_server.Daemon
module Registry = Agent_server.Session_registry
module Factory = Agent_server.Session_factory
module L = A.Inference_ledger
module O = Inference.Observation
module U = A.Goal_usage

let observed result =
  Result.map_error result ~f:(fun error -> Sexp.to_string_hum (O.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* Exact late host evidence for the actual retained provider row. No admission
   or ownership is reconstructed from decoded child/snapshot identifiers. *)
let correction handle revision input =
  let actual = O.Count.create (Actual input) |> observed in
  let unknown = O.Count.create (Unknown Not_reported) |> observed in
  let usage =
    O.Usage.create
      ~counts:
        { input = actual
        ; output = unknown
        ; reported_total = unknown
        ; cached_input = unknown
        ; cache_write_input = unknown
        ; reasoning_output = unknown
        }
      ~inclusions:[]
    |> observed
  in
  O.create
    ~scope:(L.Handle.scope handle)
    ~id:(L.Handle.accounting_id handle)
    ~revision
    ~payload:(Usage usage)
    ~limits:O.Admission.observation
  |> observed
;;

let actual_input usage = U.component usage Input |> U.Metric.actual

let contribution (state : A.Session_state.t) =
  Option.value_exn state.child_contributions
  |> A.Child_contributions.aggregate
  |> protocol_ok
;;

let contributed_input state =
  contribution state
  |> A.Child_usage_snapshot.Descendants.partition
  |> A.Child_usage_snapshot.Partition.usage
  |> actual_input
;;

let entry daemon id = Registry.find (D.registry daemon) id |> Option.value_exn

let%expect_test "actual native child registration and historical whole-ledger replacement"
  =
  let child_providers = ref 0 in
  let reviewer_provider ~sw:_ ~inputs =
    let is_child =
      List.exists inputs ~f:(function
        | Openai.Responses.Item.Input_message message ->
          let json = Openai.Responses.Item.jsonaf_of_t (Input_message message) in
          String.equal (Team.text json "role") "developer"
          && Team.contains json "Review Lantern's setup tutorial for a reader"
        | _ -> false)
    in
    if is_child
    then (
      Int.incr child_providers;
      Some (Team.answer ~id:"553-child-provider" "A retained child result."))
    else None
  in
  Team.with_team_runtime
    ~reviewer_provider
    (fun ~env ~daemon create invoke state _continuations ->
       let parent_handle = create "generated" in
       Exn.protect
         ~finally:(fun () -> Agent_client.Session_handle.close parent_handle)
         ~f:(fun () ->
           let parent_id = Agent_client.Session_handle.session_id parent_handle in
           let candidate = Team.source "generated/create.json" |> Jsonaf.of_string in
           let created = invoke parent_handle "agent_create" candidate |> Team.complete in
           let child_id =
             Team.field created "session_id" |> P.Id.Session.of_json |> protocol_ok
           in
           let child = entry daemon child_id in
           invoke
             parent_handle
             "agent_send"
             (`Object
                 [ "session_id", P.Id.Session.to_json child_id
                 ; "message", `String "Produce the retained accounting result."
                 ; "idempotency_key", `String "553-first-child-input"
                 ])
           |> Team.complete
           |> ignore;
           Background_shell_tests.wait ~timeout:15. env (fun () ->
             let current = Actor.state child.actor |> protocol_ok in
             Option.is_none current.active_operation
             && not (List.is_empty (L.rows current.inference_ledger)));
           [%test_eq: int] 1 !child_providers;
           let parent = entry daemon parent_id in
           let before_import = state parent_id in
           let child_state = state child_id in
           let reference = Option.value_exn child_state.spec.delegation in
           (* The native creation path ACKed this slot BEFORE Linked/start.
              First factual import below must not be able to fake this oracle. *)
           let children = Option.value_exn before_import.child_contributions in
           assert (
             List.mem
               (A.Child_contributions.references children)
               reference
               ~equal:Agent_store.Delegation_store.Reference.equal);
           let coverage =
             contribution before_import |> A.Child_usage_snapshot.Descendants.coverage
           in
           assert (
             not (A.Child_usage_snapshot.Coverage.historical_children_unknown coverage));
           [%test_eq: int64]
             1L
             (A.Child_usage_snapshot.Coverage.unobserved_children coverage);
           let handle =
             match L.rows child_state.inference_ledger with
             | [ row ] -> L.Row.handle row
             | [] | _ :: _ :: _ ->
               failwith "actual child did not retain exactly one provider row"
           in
           let usage_revision =
             L.find child_state.inference_ledger ~ordinal:(L.Handle.ordinal handle)
             |> Option.value_exn
             |> L.Row.record
             |> O.Attempt_record.observations
             |> List.filter_map ~f:(fun observation ->
               match O.payload observation with
               | Usage _ -> Some (O.revision observation)
               | Configuration _
               | Context_estimate _
               | Transport_selection _
               | Diagnostic _ -> None)
             |> List.fold ~init:0L ~f:Int64.max
           in
           let actor = Operator_authorization.trusted_local (principal ()) in
           let authorize expected current =
             if P.Id.Session.equal current.A.Session_state.identity.session_id expected
             then Ok ()
             else
               Error (P.Error.invalid_request "fixture read targets another actual owner")
           in
           let import_with actor ~authorize_child =
             Factory.import_child_usage
               (D.factory daemon)
               ~parent
               ~child
               ~actor
               ~authorize_parent:(authorize parent_id)
               ~authorize_child
           in
           let import () =
             import_with actor ~authorize_child:(authorize child_id) |> protocol_ok
           in
           Actor.observe_inference
             child.actor
             ~handle
             (correction handle Int64.(usage_revision + 1L) 7L)
           |> protocol_ok;
           (match import () with
            | Imported Added -> ()
            | Imported (Replaced | Duplicate | Stale) | Historical_absence ->
              failwith "first actual observed child contribution was not Added");
           let seven = state parent_id in
           assert (
             P.Inference_query.Metric.equal_sum (Tokens 7L) (contributed_input seven));
           let complete =
             contribution seven |> A.Child_usage_snapshot.Descendants.coverage
           in
           [%test_eq: int64]
             0L
             (A.Child_usage_snapshot.Coverage.unobserved_children complete);
           (match import () with
            | Imported Duplicate -> ()
            | Imported (Added | Replaced | Stale) | Historical_absence ->
              failwith "same actual child snapshot did not duplicate");
           let duplicate = state parent_id in
           [%test_eq: int64] seven.counters.revision duplicate.counters.revision;
           [%test_eq: int64]
             seven.counters.transaction_sequence
             duplicate.counters.transaction_sequence;
           Actor.observe_inference
             child.actor
             ~handle
             (correction handle Int64.(usage_revision + 2L) 11L)
           |> protocol_ok;
           (match import () with
            | Imported Replaced -> ()
            | Imported (Added | Duplicate | Stale) | Historical_absence ->
              failwith "new acknowledged child snapshot did not replace");
           let eleven = state parent_id in
           assert (
             P.Inference_query.Metric.equal_sum (Tokens 11L) (contributed_input eleven));
           let own = Option.value_exn (state child_id).delegated_usage in
           assert (
             P.Inference_query.Metric.equal_sum
               (Tokens 11L)
               (A.Delegated_usage.usage own ~ledger:(state child_id).inference_ledger
                |> protocol_ok
                |> actual_input));
           (* Revoke the SAME real read original between the child policy's
              pre/post Store checks. No new snapshot may reach a parent ACK. *)
           let live = ref true in
           let policy_calls = ref 0 in
           let retained_original =
             Operator_authorization.guarded
               ~principal:(principal ())
               ~is_current:(fun () -> !live)
           in
           let revoked =
             import_with retained_original ~authorize_child:(fun current ->
               let result = authorize child_id current in
               Int.incr policy_calls;
               if Int.equal !policy_calls 2 then live := false;
               result)
           in
           (match revoked with
            | Error { code = Permission_denied; retryable = false; _ } -> ()
            | Error error -> raise_s [%sexp (error : P.Error.t)]
            | Ok _ -> failwith "revoked child-read original reached parent import");
           [%test_eq: int] 2 !policy_calls;
           let denied = state parent_id in
           [%test_eq: int64] eleven.counters.revision denied.counters.revision;
           [%test_eq: int64]
             eleven.counters.transaction_sequence
             denied.counters.transaction_sequence;
           assert (
             P.Inference_query.Metric.equal_sum (Tokens 11L) (contributed_input denied));
           [%test_eq: int] 1 !child_providers;
           let retry = invoke parent_handle "agent_create" candidate |> Team.complete in
           assert (
             P.Id.Session.equal
               child_id
               (Team.field retry "session_id" |> P.Id.Session.of_json |> protocol_ok));
           [%test_eq: int] 1 !child_providers;
           assert (
             P.Inference_query.Metric.equal_sum
               (Tokens 11L)
               (contributed_input (state parent_id)));
           print_endline
             "actual native slot before publication; child 7 -> 11 replaces once; \
              duplicate/retry no child replay"));
  [%expect
    {| actual native slot before publication; child 7 -> 11 replaces once; duplicate/retry no child replay |}]
;;

let delegated_request ~key ~text ~tools =
  `Object
    [ "version", `Number "1"
    ; "root_file", `String "child.chatmd"
    ; ( "sources"
      , `Array [ `Object [ "path", `String "child.chatmd"; "text", `String text ] ] )
    ; "tools", `Array (List.map tools ~f:(fun name -> `String name))
    ; "start_immediately", `True
    ; "lifetime", `String "owned"
    ; "display_name", `String key
    ; "idempotency_key", `String key
    ]
;;

let developer_contains inputs marker =
  List.exists inputs ~f:(function
    | Openai.Responses.Item.Input_message message ->
      let json = Openai.Responses.Item.jsonaf_of_t (Input_message message) in
      String.equal (Team.text json "role") "developer" && Team.contains json marker
    | _ -> false)
;;

let retained_input entry input =
  let current = Actor.state entry.Registry.actor |> protocol_ok in
  let row = List.hd_exn (L.rows current.inference_ledger) in
  let handle = L.Row.handle row in
  let revision =
    L.Row.record row
    |> O.Attempt_record.observations
    |> List.filter_map ~f:(fun observation ->
      match O.payload observation with
      | Usage _ -> Some (O.revision observation)
      | Configuration _ | Context_estimate _ | Transport_selection _ | Diagnostic _ ->
        None)
    |> List.fold ~init:0L ~f:Int64.max
    |> Int64.succ
  in
  Actor.observe_inference entry.actor ~handle (correction handle revision input)
  |> protocol_ok
;;

let%expect_test "actual two-level child usage follows only its immediate association" =
  let child_calls = ref 0 in
  let grandchild_calls = ref 0 in
  let grandchild_request =
    delegated_request
      ~key:"553-grandchild"
      ~text:"<config model=\"fixture-model\"/><developer>553-grandchild-leaf</developer>"
      ~tools:[]
  in
  let reviewer_provider ~sw:_ ~inputs =
    if developer_contains inputs "553-grandchild-leaf"
    then (
      Int.incr grandchild_calls;
      Some (Team.answer ~id:"553-grandchild" "A genuine nested result."))
    else if developer_contains inputs "553-child-parent"
    then (
      Int.incr child_calls;
      Some
        (match !child_calls with
         | 1 ->
           Fixtures.call_events
             [ "553-nested-create", "agent_create", grandchild_request ]
         | 2 ->
           let created =
             List.find_map_exn inputs ~f:(function
               | Openai.Responses.Item.Function_call_output
                   { call_id; output = Text text; _ }
                 when String.equal call_id "553-nested-create" ->
                 Some (Jsonaf.of_string text)
               | _ -> None)
             |> P.Invocation.outcome_of_json
             |> protocol_ok
             |> Team.complete
           in
           Fixtures.call_events
             [ ( "553-nested-send"
               , "agent_send"
               , `Object
                   [ "session_id", Team.field created "session_id"
                   ; "message", `String "Produce the nested result."
                   ; "idempotency_key", `String "553-grandchild-input"
                   ] )
             ]
         | _ -> Team.answer ~id:"553-child-parent" "The nested child was created."))
    else None
  in
  Team.with_team_runtime
    ~reviewer_provider
    (fun ~env ~daemon create invoke state _continuations ->
       let root_handle = create "generated" in
       Exn.protect
         ~finally:(fun () -> Agent_client.Session_handle.close root_handle)
         ~f:(fun () ->
           let root_id = Agent_client.Session_handle.session_id root_handle in
           let request =
             delegated_request
               ~key:"553-child-parent"
               ~text:
                 "<config model=\"fixture-model\"/><developer>553-child-parent</developer>\n\
                  <authoring_context policy=\"manual\"/>\n\
                  <tool type=\"inherited\" name=\"agent_create\"/>\n\
                  <tool type=\"inherited\" name=\"agent_send\"/>"
               ~tools:[ "agent_create"; "agent_send" ]
           in
           let created = invoke root_handle "agent_create" request |> Team.complete in
           let child_id =
             Team.field created "session_id" |> P.Id.Session.of_json |> protocol_ok
           in
           let child = entry daemon child_id in
           invoke
             root_handle
             "agent_send"
             (`Object
                 [ "session_id", P.Id.Session.to_json child_id
                 ; "message", `String "Create and run the nested child."
                 ; "idempotency_key", `String "553-parent-input"
                 ])
           |> Team.complete
           |> ignore;
           let grandchild_id = ref None in
           Background_shell_tests.wait ~timeout:20. env (fun () ->
             let current = Actor.state child.actor |> protocol_ok in
             grandchild_id
             := List.find_map current.invocations ~f:(fun invocation ->
                  match invocation.P.Invocation.status with
                  | Published (Complete (`Object fields)) ->
                    List.Assoc.find fields "session_id" ~equal:String.equal
                    |> Option.map ~f:(fun value ->
                      P.Id.Session.of_json value |> protocol_ok)
                  | Admitted | Dispatching | Resolved _ | Published _ -> None);
             Option.is_some !grandchild_id
             && Int.equal !child_calls 3
             && Int.equal !grandchild_calls 1
             && Option.is_none current.active_operation);
           let grandchild_id = Option.value_exn !grandchild_id in
           let grandchild = entry daemon grandchild_id in
           Background_shell_tests.wait ~timeout:10. env (fun () ->
             Option.is_none (state grandchild_id).active_operation);
           retained_input child 7L;
           retained_input grandchild 11L;
           let root = entry daemon root_id in
           let actor = Operator_authorization.trusted_local (principal ()) in
           let authorize _ = Ok () in
           let import parent child =
             Factory.import_child_usage
               (D.factory daemon)
               ~parent
               ~child
               ~actor
               ~authorize_parent:authorize
               ~authorize_child:authorize
           in
           import root child |> protocol_ok |> ignore;
           let before_nested = state root_id in
           assert (
             P.Inference_query.Metric.equal_sum
               (Tokens 7L)
               (contributed_input before_nested));
           let coverage =
             contribution before_nested |> A.Child_usage_snapshot.Descendants.coverage
           in
           [%test_eq: int64]
             1L
             (A.Child_usage_snapshot.Coverage.unobserved_children coverage);
           import child grandchild |> protocol_ok |> ignore;
           import root child |> protocol_ok |> ignore;
           let complete = state root_id in
           assert (
             P.Inference_query.Metric.equal_sum (Tokens 18L) (contributed_input complete));
           let coverage =
             contribution complete |> A.Child_usage_snapshot.Descendants.coverage
           in
           [%test_eq: int64]
             0L
             (A.Child_usage_snapshot.Coverage.unobserved_children coverage);
           (match import root grandchild with
            | Error { code = Permission_denied; retryable = false; _ } -> ()
            | Error error ->
              raise_s [%sexp "unexpected foreign-grandchild result", (error : P.Error.t)]
            | Ok _ ->
              failwith "root accepted its grandchild through a second accounting path");
           let unchanged = state root_id in
           [%test_eq: int64] complete.counters.revision unchanged.counters.revision;
           assert (
             P.Inference_query.Metric.equal_sum (Tokens 18L) (contributed_input unchanged));
           [%test_eq: int] 3 !child_calls;
           [%test_eq: int] 1 !grandchild_calls;
           print_endline
             "two actual native edges: unknown subtree -> 7+11 exactly once; direct \
              grandchild import denied"));
  [%expect
    {| two actual native edges: unknown subtree -> 7+11 exactly once; direct grandchild import denied |}]
;;
