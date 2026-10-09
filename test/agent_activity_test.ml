open! Core
module P = Agent_protocol

let accepted result = Result.is_ok result

let protocol_ok = function
  | Ok value -> value
  | Error (error : P.Error.t) -> failwith error.message
;;

let%expect_test "attention reasons and expiry cannot contradict their entity" =
  let permission =
    P.Session_activity.Attention.Permission
      (P.Id.Permission.of_string "per_activity_fixture" |> protocol_ok)
  in
  let work =
    P.Session_activity.Attention.Work
      (P.Session_work.Key.Schedule
         (P.Id.Schedule.of_string "sch_activity_fixture" |> protocol_ok))
  in
  let create entity reason expired =
    P.Session_activity.Attention.create ~entity ~reason ~unresolved:true ~expired
    |> accepted
  in
  print_s
    [%sexp
      { approval = (create permission Approval true : bool)
      ; wrong_permission_reason = (create permission Failure false : bool)
      ; wrong_work_reason = (create work Approval false : bool)
      ; completion = (create work Completion_pending false : bool)
      ; expired_completion = (create work Completion_pending true : bool)
      }];
  [%expect
    {|
    ((approval true) (wrong_permission_reason false) (wrong_work_reason false)
     (completion true) (expired_completion false))
    |}]
;;

let%expect_test "transient counts reject impossible state through JSON and sexp" =
  let module T = P.Session_activity.Transient in
  let validate value = T.of_json (T.to_json value) |> accepted in
  let invalid_sexp_rejected =
    try
      ignore (T.t_of_sexp (T.sexp_of_t (Live { tool_calls = 1; agent_calls = 2 })) : T.t);
      false
    with
    | Sexplib.Conv.Of_sexp_error _ -> true
  in
  print_s
    [%sexp
      { unavailable = (validate Unavailable : bool)
      ; live = (validate (Live { tool_calls = 2; agent_calls = 1 }) : bool)
      ; negative = (validate (Live { tool_calls = -1; agent_calls = 0 }) : bool)
      ; excess_agents = (validate (Live { tool_calls = 1; agent_calls = 2 }) : bool)
      ; invalid_sexp_rejected : bool
      }];
  [%expect
    {|
    ((unavailable true) (live true) (negative false) (excess_agents false)
     (invalid_sexp_rejected true))
    |}]
;;

let%expect_test "work occurrence survives wire/sexp and rejects noncanonical revision" =
  let module W = P.Session_work in
  let session =
    P.Session_ref.create
      ~server_id:(P.Id.Server.of_string "srv_activity_fixture" |> protocol_ok)
      ~session_id:(P.Id.Session.of_string "ses_activity_fixture" |> protocol_ok)
  in
  let row =
    W.create
      ~session
      ~generation:4
      ~key:
        (Job
           { id = P.Id.Job.of_string "job_activity_fixture" |> protocol_ok; attempt = 2 })
      ~status:Running
      ~delivery:Not_applicable
      ~revision:9L
    |> protocol_ok
  in
  let encoded = W.to_json row in
  let wire = W.of_json encoded |> protocol_ok in
  let sexp = W.t_of_sexp (W.sexp_of_t row) in
  let corrupt =
    match encoded with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           name, if String.equal name "revision" then `String "09" else value))
    | _ -> assert false
  in
  print_s
    [%sexp
      { wire_identity = (P.Session_ref.equal row.session wire.session : bool)
      ; wire_occurrence =
          (Int.equal row.generation wire.generation && W.Key.equal row.key wire.key
           : bool)
      ; sexp_occurrence = (W.Key.equal row.key sexp.key : bool)
      ; corrupt_revision = (accepted (W.of_json corrupt) : bool)
      }];
  [%expect
    {|
    ((wire_identity true) (wire_occurrence true) (sexp_occurrence true)
     (corrupt_revision false))
    |}]
;;

let%expect_test "safe activity summary excludes hostile configuration and errors" =
  let secret = "HOSTILE_PRIVATE_PAYLOAD" in
  let now = P.Timestamp.of_string "2026-10-08T00:00:00Z" |> protocol_ok in
  let spec =
    P.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Local_path secret)
      ~workspace:(Local_path secret)
      ~liveness:Detached
      ~persistence:Durable
      ~permission_profile:secret
      ~start_immediately:false
      ~display_name:"Visible name"
      ~labels:[ "team", "visible" ]
      ()
    |> protocol_ok
  in
  let error = P.Error.invalid_request secret in
  let operation =
    P.Operation.
      { id = P.Id.Operation.of_string "op_activity_fixture" |> protocol_ok
      ; generation = 2
      ; kind = Compaction
      ; state = Interrupted { reason = secret; retryable = true }
      ; started_at = now
      ; updated_at = now
      }
  in
  let session =
    P.Session.
      { id = P.Id.Session.of_string "ses_activity_fixture" |> protocol_ok
      ; creator = None
      ; created_at = now
      ; updated_at = now
      ; generation = 2
      ; spec
      ; desired_state = Stopped
      ; observed_state = Failed error
      ; prompt_revision = None
      ; workspace_instance = None
      ; active_operation = Some operation
      ; revision = 9L
      ; metadata_revision = 3L
      ; organization = P.Session_organization.Values.empty
      ; latest_event_sequence = 7L
      ; inference_summary = History_entry.Payload.Presence.Absent
      }
  in
  let catalog =
    P.Session_catalog.
      { session
      ; archived = true
      ; lifecycle_revision = P.Session_lifecycle.Revision.one
      ; admission = Explicit_resume_required
      ; active_owner_principal_id = None
      ; effective_organization = P.Session_organization.Values.empty
      }
  in
  let module S = P.Session_activity_summary in
  let summary =
    S.of_catalog
      catalog
      ~server_id:(P.Id.Server.of_string "srv_activity_fixture" |> protocol_ok)
    |> protocol_ok
  in
  let json = S.to_json summary in
  let decoded = S.of_json json |> protocol_ok in
  let sexp = S.sexp_of_t summary in
  let from_sexp = S.t_of_sexp sexp in
  let invalid_policy =
    match json with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (key, value) ->
           key, if String.equal key "execution_host" then `String "embedded" else value))
    | _ -> assert false
  in
  let invalid_sexp =
    try
      ignore (S.t_of_sexp (Jsonaf.sexp_of_t invalid_policy) : S.t);
      false
    with
    | Sexplib.Conv.Of_sexp_error _ -> true
  in
  let invalid =
    match json with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (key, value) ->
           key, if String.equal key "revision" then `String "-1" else value))
    | _ -> assert false
  in
  print_s
    [%sexp
      { json_payload_absent =
          (not (String.is_substring (Jsonaf.to_string json) ~substring:secret) : bool)
      ; sexp_payload_absent =
          (not (String.is_substring (Sexp.to_string sexp) ~substring:secret) : bool)
      ; failed_status = (S.Observed.equal decoded.observed Failed : bool)
      ; metadata_retained =
          (Option.equal String.equal from_sexp.display_name (Some "Visible name") : bool)
      ; lifecycle_policy = (P.Session.equal_liveness decoded.liveness Detached : bool)
      ; malformed_policy_rejected = (Result.is_error (S.of_json invalid_policy) : bool)
      ; malformed_policy_sexp_rejected = (invalid_sexp : bool)
      ; malformed_revision_rejected = (Result.is_error (S.of_json invalid) : bool)
      }];
  [%expect
    {|
    ((json_payload_absent true) (sexp_payload_absent true) (failed_status true)
     (metadata_retained true) (lifecycle_policy true)
     (malformed_policy_rejected true) (malformed_policy_sexp_rejected true)
     (malformed_revision_rejected true))
    |}]
;;

let%expect_test
    "foreground activity status preserves cancellation and rejects work-only states"
  =
  let module S = P.Session_activity_summary.Operation.Status in
  let statuses =
    [ S.Starting; Running; Cancelling; Completed; Failed; Cancelled; Interrupted ]
  in
  let roundtrips =
    List.for_all statuses ~f:(fun status ->
      match S.of_json (S.to_json status) with
      | Ok decoded ->
        S.equal status decoded && S.equal (S.t_of_sexp (S.sexp_of_t status)) status
      | Error _ -> false)
  in
  let impossible =
    List.for_all
      [ "accepted"; "waiting_work"; "waiting_approval"; "unsupported" ]
      ~f:(fun state -> Result.is_error (S.of_json (`String state)))
  in
  print_s [%sexp { roundtrips : bool; impossible_rejected = (impossible : bool) }];
  [%expect {| ((roundtrips true) (impossible_rejected true)) |}]
;;
