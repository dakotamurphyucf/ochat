open Core
open Fixtures
module A = Agent_session.Session_actor
module I = Agent_protocol.Invocation
module J = Agent_protocol.Job
module N = Agent_session.Native_tool_invocation
module C = Chat_response.Tool_capability

let deadline = Agent_protocol.Timestamp.of_string "2099-01-01T00:00:00Z" |> protocol_ok

let add_claimed_job
      ?(payload = `Object [ "fixture", `String "owned tool job" ])
      ?(retry_policy = J.Never)
      actor
  =
  let job =
    J.
      { id = Agent_protocol.Id.Job.create ()
      ; session_id
      ; generation = 0
      ; kind = Async_tool
      ; payload
      ; status = Queued
      ; retry_policy
      ; attempt = 0
      ; created_at = timestamp
      ; started_at = None
      ; next_run_at = None
      ; completed_at = None
      ; result = None
      ; delivery = Pending
      ; launch = None
      ; progress = None
      }
  in
  A.add_job actor job |> protocol_ok |> ignore;
  A.claim_job actor ~job_id:job.id ~generation:0 |> protocol_ok |> Option.value_exn
;;

let with_job actor (job : J.t) f =
  A.with_job_invocations
    actor
    ~job_id:job.id
    ~generation:job.generation
    ~attempt:job.attempt
    ~deadline:(Some deadline)
    f
;;

let root (job : J.t) =
  I.create
    { (invocation_fixture ()).context with
      id = Agent_protocol.Id.Invocation.create ()
    ; parent_job = Some job.id
    ; deadline = Some deadline
    }
  |> protocol_ok
;;

let complete actor (job : J.t) =
  A.complete_job
    actor
    ~job_id:job.id
    ~generation:job.generation
    ~attempt:job.attempt
    (Agent_session.Runtime_builder.Model_succeeded `Null)
;;

let reject label result =
  match result with
  | Error (error : Agent_protocol.Error.t) ->
    print_s [%sexp (label : string), (error.code : Agent_protocol.Error.code)]
  | Ok _ -> failwith (label ^ " was accepted")
;;

let with_actor
      ?(reject_save = fun _ -> false)
      ?(prepare_state = Fn.id)
      ?(now = fun () -> timestamp)
      ?monotonic_now
      ?(make_job_results = fun _ _ _ -> None)
      ?(subscription_limits = Agent_session.Staged_subscriptions.default_limits)
      ?(schedule_limits = Agent_session.Staged_schedules.default_limits)
      ?(notification_limits = Agent_session.Staged_notifications.default_limits)
      ?(ingress_limits = Agent_session.Staged_ingress.default_limits)
      f
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let initial =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        |> prepare_state
      in
      let backend =
        Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
      in
      let persistence = Agent_session.Memory_backend.persistence backend in
      let job_results = make_job_results env sw initial in
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
                  match reject_save next with
                  | true -> Error (handoff_error "injected job transition failure")
                  | false -> persistence.commit ~command_audit ~previous next)
            }
          ~services:
            { now
            ; monotonic_now =
                Option.value monotonic_now ~default:(fun () ->
                  now ()
                  |> Agent_protocol.Timestamp.to_time_ns
                  |> Time_ns.to_int_ns_since_epoch
                  |> Int64.of_int
                  |> Mtime.of_uint64_ns)
            ; create_attachment_id = Agent_protocol.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "background-fixture")
            ; job_results
            ; schedule_limits
            ; notification_limits
            ; ingress_limits
            ; subscription_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      Exn.protect
        ~finally:(fun () -> A.shutdown actor)
        ~f:(fun () ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
            let writer, _ =
              A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
            in
            A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore;
            f env sw actor writer backend))))
;;

let rec await_permission actor =
  let state = A.state actor |> protocol_ok in
  match
    List.find state.permissions ~f:(fun p ->
      Agent_protocol.Permission.equal_state p.state Pending)
  with
  | Some permission -> permission
  | None ->
    Eio.Fiber.yield ();
    await_permission actor
;;

let permission (invocation : I.t) =
  Agent_protocol.Permission.
    { id = Agent_protocol.Id.Permission.create ()
    ; session_id
    ; generation = 0
    ; owner = Invocation invocation.context.id
    ; call_id = "background-read"
    ; tool_name = "read_file"
    ; runtime_identity = None
    ; invocation_display = "background read"
    ; rationale = None
    ; effects = [ "read" ]
    ; choices = [ Approve_once; Deny ]
    ; created_at = timestamp
    ; expires_at = None
    ; state = Pending
    ; resolution = None
    }
;;
