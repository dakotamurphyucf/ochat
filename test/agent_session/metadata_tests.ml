open! Core
open Fixtures
module P = Agent_protocol
module S = Agent_session

let%expect_test
    "metadata edits ignore stream revisions and atomically retain execution state"
  =
  with_actor_workspace (fun _ workspace_instance ->
    let original =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let state = { original with counters = { original.counters with revision = 71L } } in
    let patch =
      P.Session_metadata.Patch.create
        ~name:(Set "renamed")
        ~set_labels:[ "tag", "release" ]
        ~remove_labels:[]
      |> protocol_ok
    in
    let delta =
      S.Session_metadata_transition.apply state ~expected_metadata_revision:0L ~patch
      |> protocol_ok
      |> Option.value_exn
    in
    let next = S.Session_delta.apply state delta |> protocol_ok in
    let stale =
      S.Session_metadata_transition.apply next ~expected_metadata_revision:0L ~patch
    in
    print_s
      [%sexp
        { name = (next.identity.display_name : string option)
        ; metadata_revision = (next.identity.metadata_revision : int64)
        ; stream_revision = (next.counters.revision : int64)
        ; mirrored =
            (Option.equal
               String.equal
               next.identity.display_name
               next.spec.protocol.display_name
             : bool)
        ; workspace_preserved =
            (P.Id.Workspace_instance.equal
               next.spec.workspace_instance.id
               state.spec.workspace_instance.id
             : bool)
        ; stale =
            ((match stale with
              | Ok _ -> "unexpected"
              | Error error -> P.Error.code_to_string error.code)
             : string)
        }];
    let no_op =
      S.Session_metadata_transition.apply next ~expected_metadata_revision:1L ~patch
      |> protocol_ok
    in
    print_s [%sexp (Option.is_none no_op : bool)]);
  [%expect
    {|
    ((name (renamed)) (metadata_revision 1) (stream_revision 71) (mirrored true)
     (workspace_preserved true) (stale conflict))
    true |}]
;;

let%expect_test "metadata delta codec survives independent state recovery" =
  with_actor_workspace (fun _ workspace_instance ->
    let state =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let values =
      P.Session_metadata.Values.create
        ~display_name:(Some "restart")
        ~labels:[ "tag", "kept" ]
      |> protocol_ok
    in
    let delta = S.Session_delta.Metadata_changed (values, 1L) in
    let limits = document_limits in
    let encoded =
      S.Session_delta_document.create
        delta
        ~limits
        ~state_document:S.Session_state_document.authored
      |> document_ok
    in
    let document = S.Session_delta_document.document encoded in
    let recovered = S.Session_delta_document.decode ~limits document |> document_ok in
    let next =
      S.Session_delta.apply state (S.Session_delta_document.value recovered)
      |> protocol_ok
    in
    print_s
      [%sexp
        { name = (next.identity.display_name : string option)
        ; labels = (next.spec.protocol.labels : (string * string) list)
        ; revision = (next.identity.metadata_revision : int64)
        }]);
  [%expect {| ((name (restart)) (labels ((tag kept))) (revision 1)) |}]
;;

let%expect_test
    "actor metadata admission preserves active work and failed commits publish nothing"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let initial =
        actor_state ~workspace_instance ~liveness:Process_bound ~start_immediately:false
      in
      let operation =
        P.Operation.
          { id = P.Id.Operation.of_string "op_metadata_active" |> protocol_ok
          ; generation = 0
          ; kind = Turn User_submit
          ; state = Running
          ; started_at = timestamp
          ; updated_at = timestamp
          }
      in
      let initial =
        { initial with
          active_operation = Some operation
        ; lifecycle = { desired = Running; observed = Running_turn operation.id }
        }
      in
      let fail_commit = ref false
      and next_attachment = ref 0 in
      let actor =
        S.Session_actor.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:initial
          ~operation_worker:None
          ~persistence:
            { archive_reference
            ; commit =
                (fun ~command_audit:_ ~previous:_ _ ->
                  if !fail_commit
                  then
                    Error
                      (P.Error.create
                         Persistence_error
                         ~message:"injected write failure"
                         ~retryable:false
                         ())
                  else Ok ())
            }
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Int.incr next_attachment;
                  P.Id.Attachment.of_string (sprintf "att_metadata_%d" !next_attachment)
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "metadata-token")
            ; job_results = None
            ; monotonic_now = (fun () -> Mtime.min_stamp)
            ; schedule_limits = S.Staged_schedules.default_limits
            ; notification_limits = S.Staged_notifications.default_limits
            ; ingress_limits = S.Staged_ingress.default_limits
            ; subscription_limits = S.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let writer, _ =
        S.Session_actor.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
      in
      let reader, _ =
        S.Session_actor.attach actor ~mode:Read_only ~subscribe:false |> protocol_ok
      in
      let patch name =
        P.Session_metadata.Patch.create ~name:(Set name) ~set_labels:[] ~remove_labels:[]
        |> protocol_ok
      in
      let edit attachment revision name =
        S.Session_actor.update_metadata
          actor
          ~attachment_id:attachment.P.Session.Attachment.id
          ~expected_metadata_revision:revision
          ~patch:(patch name)
          ()
      in
      let status = function
        | Ok _ -> "ok"
        | Error (e : P.Error.t) -> P.Error.code_to_string e.code
      in
      print_endline (status (edit reader 0L "denied"));
      let changed = edit writer 0L "during operation" |> protocol_ok in
      print_s
        [%sexp
          (Option.exists changed.active_operation ~f:(fun actual ->
             P.Id.Operation.equal actual.id operation.id)
           : bool)];
      print_endline (status (edit writer 0L "stale"));
      fail_commit := true;
      print_endline (status (edit writer 1L "not published"));
      let state = S.Session_actor.state actor |> protocol_ok in
      print_s
        [%sexp
          { name = (state.identity.display_name : string option)
          ; mirror = (state.spec.protocol.display_name : string option)
          ; revision = (state.identity.metadata_revision : int64)
          }];
      fail_commit := false;
      S.Session_actor.shutdown actor));
  [%expect
    {|
    permission_denied
    true
    conflict
    persistence_error
    ((name ("during operation")) (mirror ("during operation")) (revision 1)) |}]
;;

let%expect_test
    "authored metadata patches preserve and can remove historical unusual keys"
  =
  let historical =
    P.Session_metadata.Values.create
      ~display_name:(Some "legacy\nname")
      ~labels:[ "", "old"; "odd\nkey", "retained" ]
    |> protocol_ok
  in
  let rename =
    P.Session_metadata.Patch.create
      ~name:(Set "new name")
      ~set_labels:[]
      ~remove_labels:[]
    |> protocol_ok
  in
  let preserved = P.Session_metadata.Patch.apply rename historical |> protocol_ok in
  print_s
    [%sexp
      (List.equal
         [%equal: string * string]
         preserved.labels
         [ "", "old"; "odd\nkey", "retained" ]
       : bool)];
  let cleanup =
    P.Session_metadata.Patch.create ~name:Keep ~set_labels:[] ~remove_labels:[ "" ]
    |> protocol_ok
  in
  let cleaned = P.Session_metadata.Patch.apply cleanup preserved |> protocol_ok in
  print_s
    [%sexp
      (List.equal [%equal: string * string] cleaned.labels [ "odd\nkey", "retained" ]
       : bool)];
  print_s
    [%sexp
      (Result.is_error
         (P.Session_metadata.Patch.create
            ~name:Keep
            ~set_labels:[ "odd\nkey", "new" ]
            ~remove_labels:[])
       : bool)];
  [%expect
    {|
    true
    true
    true |}]
;;

let%expect_test "label mirrors compare keys and values independently of retained order" =
  with_actor_workspace (fun _ workspace_instance ->
    let before =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let state =
      { before with
        identity = { before.identity with labels = [ "z", "second"; "a", "first" ] }
      ; spec =
          { before.spec with
            protocol =
              { before.spec.protocol with labels = [ "a", "first"; "z", "second" ] }
          }
      }
    in
    print_s [%sexp (Result.is_ok (S.Session_state.validate state) : bool)];
    let different =
      { state with
        spec =
          { state.spec with
            protocol =
              { state.spec.protocol with labels = [ "a", "changed"; "z", "second" ] }
          }
      }
    in
    print_s [%sexp (Result.is_error (S.Session_state.validate different) : bool)]);
  [%expect
    {|
    true
    true |}]
;;
