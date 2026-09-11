open Core
open Agent_server_test_support
module P = Agent_protocol
module A = Agent_session.Session_actor
module State = Agent_session.Session_state
module C = Chat_response.Tool_capability

let same before after =
  assert (Sexp.equal (State.sexp_of_t before) (State.sexp_of_t after))
;;

let rejected result =
  match result with
  | Error _ -> ()
  | Ok _ -> failwith "invalid delivery change accepted"
;;

let fields = function
  | `Object fields -> fields
  | _ -> failwith "expected encoded object"
;;

let check_storage before (job : P.Job.t) =
  let retired =
    { job with
      delivery =
        Discarded { at = Option.value_exn job.completed_at; reason = Authority_changed }
    }
  in
  let delta = Agent_session.Session_delta.Job_changed retired in
  let state = Agent_session.Session_delta.apply before delta |> protocol_ok in
  let restored =
    Agent_session.Session_persistence.restore_snapshot
      (State.sexp_of_t state |> Sexp.to_string_mach)
    |> Background_recovery_tests.store_ok
  in
  same state restored;
  same state (Agent_session.Session_delta.apply state delta |> protocol_ok);
  List.iter [ 7; 8 ] ~f:(fun version ->
    rejected
      (State.upgrade_schema { state with schema_version = version; stop_epoch = 0L }));
  (* Legacy schemas had no stop counter; test retirement migration independently
     of that later lifecycle field, including the rejection cases above. *)
  let legacy_before = { before with stop_epoch = 0L } in
  State.upgrade_schema { legacy_before with schema_version = 8 }
  |> protocol_ok
  |> same legacy_before;
  List.iter
    [ { retired with delivery = Pending }
    ; { retired with delivery = Delivered (Option.value_exn retired.completed_at) }
    ; { retired with delivery = Not_required }
    ; { retired with result = Some (P.Completion.to_json (Succeeded (`String "forged"))) }
    ; { retired with attempt = retired.attempt + 1 }
    ]
    ~f:(fun changed ->
      rejected (Agent_session.Session_delta.apply state (Job_changed changed)));
  rejected
    (Agent_session.Session_delta.apply
       before
       (Job_changed { retired with status = Running; completed_at = None }));
  rejected
    (Agent_session.Session_delta.apply
       before
       (Job_changed
          { retired with result = Some (P.Completion.to_json (Succeeded `Null)) }));
  let roundtrip = P.Job.of_json (P.Job.to_json retired) |> protocol_ok in
  assert (Jsonaf.exactly_equal (P.Job.to_json retired) (P.Job.to_json roundtrip));
  let job_fields = fields (P.Job.to_json retired) in
  let delivery = List.Assoc.find_exn job_fields ~equal:String.equal "delivery" in
  let delivery_fields = fields delivery in
  List.iter
    [ "type", `String "pending"
    ; "schema_version", `Number "2"
    ; "reason", `String "unknown"
    ; "secret", `String "PRIVATE"
    ]
    ~f:(fun (name, value) ->
      let changed =
        `Object (List.Assoc.add delivery_fields ~equal:String.equal name value)
      in
      rejected
        (P.Job.of_json
           (`Object (List.Assoc.add job_fields ~equal:String.equal "delivery" changed))));
  print_endline "retirement survives storage; replay cannot revive or rewrite the job"
;;

(* Use a captured real compiled-session state with an isolated persistence fault.
   No worker is installed, so these checks cannot run a provider or shell. *)
let with_actor env initial f =
  Eio.Switch.run (fun sw ->
    let backend =
      Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
    in
    let persistence = Agent_session.Memory_backend.persistence backend in
    let reject_save = ref false in
    let actor =
      A.create
        ~sw
        ~clock:(Eio.Stdenv.clock env)
        ~mailbox_capacity:32
        ~compaction_env:None
        ~initial_state:initial
        ~operation_worker:None
        ~persistence:
          { commit =
              (fun ~command_audit ~previous next ->
                match !reject_save with
                | true ->
                  Error (P.Error.invalid_request "injected retirement save failure")
                | false -> persistence.commit ~command_audit ~previous next)
          }
        ~services:
          { now = P.Timestamp.now
          ; monotonic_now = (fun () -> Eio.Time.Mono.now (Eio.Stdenv.mono_clock env))
          ; create_attachment_id = P.Id.Attachment.create
          ; create_reclaim_token = (fun () -> "retirement-fixture")
          ; job_results = None
          ; schedule_limits = Agent_session.Staged_schedules.default_limits
          ; notification_limits = Agent_session.Staged_notifications.default_limits
          ; ingress_limits = Agent_session.Staged_ingress.default_limits
          ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
          ; state_committed = (fun _ _ -> ())
          }
    in
    Exn.protect
      ~finally:(fun () -> A.shutdown actor)
      ~f:(fun () -> f actor backend reject_save))
;;

let check_actor_case env initial current_capabilities =
  let job = List.hd_exn initial.State.jobs in
  let owner =
    match job.launch with
    | Some { owner = Invocation id; _ } -> id
    | _ -> failwith "missing standalone owner"
  in
  let invocation =
    List.find_exn initial.invocations ~f:(fun invocation ->
      P.Id.Invocation.equal invocation.context.id owner)
  in
  let contract = Option.value_exn invocation.completion_contract in
  let targets = contract.tool_name :: List.map contract.capability_pins ~f:fst in
  List.iter targets ~f:(fun removed ->
    let selected =
      C.select
        current_capabilities
        ~names:
          (C.references current_capabilities
           |> List.filter_map ~f:(fun reference ->
             Option.some_if (not (String.equal reference.name removed)) reference.name))
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
    in
    with_actor env initial (fun actor backend reject_save ->
      let deliver ?(capabilities = selected) revision job =
        A.deliver_standalone_completion
          actor
          ~revision
          ~job
          ~current_capabilities:capabilities
          ~policy:Chat_response.One_off_request.default_policy
      in
      let revision = initial.counters.revision in
      rejected (deliver (Int64.pred revision) job);
      rejected (deliver revision { job with attempt = job.attempt + 1 });
      reject_save := true;
      (* A storage/admission error under unchanged authority is retryable too.
             For the artifact case the absent loader fails before persistence. *)
      rejected (deliver ~capabilities:current_capabilities revision job);
      same initial (A.state actor |> protocol_ok);
      rejected (deliver revision job);
      same initial (A.state actor |> protocol_ok);
      same initial (Agent_session.Memory_backend.state backend);
      reject_save := false;
      deliver revision job |> protocol_ok;
      let state = A.state actor |> protocol_ok in
      let retained = List.hd_exn state.jobs in
      (match retained.delivery with
       | Discarded { reason = Authority_changed; _ } -> ()
       | _ -> failwith "authority failure not retired");
      assert (
        Jsonaf.exactly_equal
          (P.Job.to_json { job with delivery = retained.delivery })
          (P.Job.to_json retained));
      assert (List.is_empty state.deliveries);
      same state (Agent_session.Memory_backend.state backend);
      rejected (deliver state.counters.revision retained);
      A.cancel_job_internal actor ~job_id:retained.id |> protocol_ok |> ignore;
      same state (A.state actor |> protocol_ok)))
;;

let check_actor env initial current_capabilities =
  check_actor_case env initial current_capabilities;
  let job = List.hd_exn initial.State.jobs in
  let completion = P.Job.terminal_completion job |> protocol_ok |> Option.value_exn in
  let bytes = P.Completion.to_json completion |> Jsonaf.to_string in
  let blob =
    P.Blob.Metadata.create
      ~id:(P.Id.Blob.create ())
      ~kind:File
      ~media_type:P.Job_artifact.media_type
      ~byte_length:(Int64.of_int (String.length bytes))
      ~digest:Digestif.SHA256.(digest_string bytes |> to_hex)
      ()
    |> protocol_ok
  in
  let reference =
    P.Job_artifact.create
      ~session_id:job.session_id
      ~job_id:job.id
      ~generation:job.generation
      ~attempt:job.attempt
      ~blob
    |> protocol_ok
  in
  let result = P.Stored_completion.artifact reference completion |> protocol_ok in
  let artifact_job = { job with result = Some (P.Stored_completion.to_json result) } in
  (* Deliberately no backing blob or loader: revoked authority must retire the
     descriptor without materializing it; unchanged authority must remain pending. *)
  check_actor_case env { initial with jobs = [ artifact_job ] } current_capabilities;
  let interrupted =
    { job with
      status = Interrupted "fixture interruption"
    ; result =
        Some
          (P.Completion.to_json
             (Failed
                { code = "background.interrupted"
                ; message = "fixture interruption"
                ; retryable = false
                ; details = `Null
                }))
    }
  in
  check_actor_case env { initial with jobs = [ interrupted ] } current_capabilities;
  print_endline
    "publisher/dependency revocation: stale and failed saves preserve pending work"
;;
