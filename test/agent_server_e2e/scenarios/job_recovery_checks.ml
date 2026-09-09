open Core
module F = Support.Background_fixture
module Host = Support.Daemon_host
module Config = Support.Config_fixture
module Actor = Agent_session.Session_actor

let actor daemon session =
  (Agent_server.Session_registry.find
     (Agent_server.Daemon.registry daemon)
     session.F.summary.id
   |> Option.value_exn)
    .actor
;;

let job session retry_policy =
  Agent_protocol.Job.
    { id = Agent_protocol.Id.Job.create ()
    ; session_id = session.F.summary.id
    ; generation = session.summary.generation
    ; kind = Model_call
    ; payload = `Object [ "recipe", `String "deliberately-unknown"; "payload", `Null ]
    ; status = Queued
    ; retry_policy
    ; attempt = 0
    ; created_at = Agent_protocol.Timestamp.now ()
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; delivery = Pending
    }
;;

let fixture env environment name =
  let fixture = Config.create environment ~name ~http_port:(F.reserve_port env) in
  F.save
    fixture
    (Config.prompt_path fixture)
    {|<developer>Isolated job recovery fixture.</developer>
<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start | `Session_resume
 | `Model_job_succeeded(string, string, json) | `Model_job_failed(string, string, string) ]
let initial_state = 0
let on_event : context -> state -> event -> state task = fun ctx state event ->
 match event with
 | `Session_start -> Task.pure(state)
 | `Session_resume -> Task.pure(state)
 | `Model_job_succeeded(_, _, _) -> Task.pure(state + 1)
 | `Model_job_failed(_, _, _) -> Task.pure(state + 1)
</script>|};
  fixture
;;

let find snapshot id =
  List.find_exn snapshot.Agent_protocol.Snapshot.jobs ~f:(fun job ->
    Agent_protocol.Id.Job.compare job.id id = 0)
;;

let queued_once snapshot =
  List.exists snapshot.Agent_protocol.Snapshot.jobs ~f:(fun job ->
    match job.status with
    | Queued -> job.attempt = 1
    | _ -> false)
;;

let retry_checkpoint env fixture policy =
  let result = ref None in
  Host.with_ env fixture ~options:Agent_server.Daemon.default_options (fun sw daemon ->
    F.with_client ~sw env fixture (fun client ->
      let session = F.create client "retry:create" in
      let job = job session policy in
      ignore
        (Actor.add_job (actor daemon session) job |> F.protocol_ok : Agent_protocol.Job.t);
      let snapshot =
        F.await_snapshot env client session "first retry backoff" queued_once
      in
      let queued = find snapshot job.id in
      F.require
        (Option.is_some queued.next_run_at && Option.is_none queued.completed_at)
        "retry deadline was not durably queued";
      result := Some (session, queued)));
  Option.value_exn !result
;;

let assert_unchanged expected actual =
  F.require
    (String.equal
       (Jsonaf.to_string (Agent_protocol.Job.to_json expected))
       (Jsonaf.to_string (Agent_protocol.Job.to_json actual)))
    "job changed across recovery before its retry deadline"
;;

let retry_case env environment name policy =
  let fixture = fixture env environment name in
  let session, queued = retry_checkpoint env fixture policy in
  Host.with_ env fixture ~options:Agent_server.Daemon.default_options (fun sw daemon ->
    F.with_client ~sw env fixture (fun client ->
      let initial = find (F.snapshot client session) queued.id in
      assert_unchanged queued initial;
      let terminal =
        F.await_snapshot env client session "bounded retries delivered" (fun state ->
          match (find state queued.id).delivery with
          | Delivered _ -> true
          | _ -> false)
      in
      let completed = find terminal queued.id in
      F.require
        (completed.attempt = 2
         &&
         match completed.status with
         | Failed _ -> true
         | _ -> false)
        "retry attempt limit was not enforced";
      let owner = actor daemon session in
      let before = Actor.state owner |> F.protocol_ok in
      F.require
        (Result.is_error
           (Actor.complete_job
              owner
              ~job_id:queued.id
              ~generation:queued.generation
              ~attempt:1
              (Agent_session.Runtime_builder.Model_succeeded `Null)))
        "late completion overwrote a terminal retry";
      let after = Actor.state owner |> F.protocol_ok in
      F.require
        (Int64.equal
           before.counters.transaction_sequence
           after.counters.transaction_sequence)
        "rejected completion still committed a transaction"));
  Host.with_ env fixture ~options:Agent_server.Daemon.default_options (fun sw _daemon ->
    F.with_client ~sw env fixture (fun client ->
      let completed = find (F.snapshot client session) queued.id in
      F.require
        (completed.attempt = 2
         &&
         match completed.delivery with
         | Delivered _ -> true
         | _ -> false)
        "delivered failure was retried after a second reopen"))
;;

let run env environment =
  retry_case
    env
    environment
    "retry-safe"
    (Safe_retry { max_attempts = 2; backoff_ms = 2000 });
  retry_case
    env
    environment
    "retry-idempotent"
    (Idempotent { key = F.key "retry-key"; max_attempts = 2; backoff_ms = 2000 })
;;
