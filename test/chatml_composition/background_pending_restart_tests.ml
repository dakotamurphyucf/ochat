open Core
open Agent_server_test_support
open Background_fixtures

let rec waiting env actor id =
  let state = A.state actor |> protocol_ok in
  let job =
    List.find_exn state.jobs ~f:(fun job -> Agent_protocol.Id.Job.equal id job.id)
  in
  match job.status with
  | Waiting_completion dependency -> dependency
  | Running | Queued ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    waiting env actor id
  | status -> raise_s [%sexp (status : J.status)]
;;

let%expect_test "restart resumes a durable wait without replaying its target invocation" =
  with_background_daemon
    ~agent:(Background_pending_tests.source `Standalone)
    ~sources:[ "any.json", "true"; "string.json", {|{"type":"string"}|} ]
    ~profile:{ permission_profile with tool_default = Ask }
    ~check_restored:(fun before after ->
      assert (Agent_protocol.Id.Job.equal before.id after.id);
      [%test_eq: int] before.attempt after.attempt)
    ~after_recovery:(fun env client entry before ->
      let parent =
        List.find_exn before.jobs ~f:(fun job ->
          match job.status with
          | Waiting_completion _ -> true
          | _ -> false)
      in
      let _, completion = await env client parent in
      (match completion with
       | Failed { code = "background.interrupted"; _ } -> ()
       | completion -> raise_s [%sexp (completion : Completion.t)]);
      let after = A.state entry.actor |> protocol_ok in
      [%test_eq: int] (List.length before.invocations) (List.length after.invocations);
      [%test_eq: int] 2 (List.length after.jobs);
      print_endline "saved wait resumed; interrupted child reported; no invocation replay")
    (fun env _client entry capabilities ->
       let parent = submit entry (B.to_json (tool capabilities "pending" (`Object []))) in
       let permission = Background_moderator_tests.pending_permission env entry.actor in
       [%test_eq: string] "pending" permission.tool_name;
       A.resolve_permission_as_system
         entry.actor
         ~permission_id:permission.id
         ~permission_generation:permission.generation
         ~choice:Approve_once
         ~reason:None
       |> protocol_ok
       |> ignore;
       let dependency = waiting env entry.actor parent.id in
       let permission = Background_moderator_tests.pending_permission env entry.actor in
       [%test_eq: string] "read_file" permission.tool_name;
       let state = A.state entry.actor |> protocol_ok in
       let child =
         List.find_exn state.jobs ~f:(fun job ->
           Agent_protocol.Id.Job.equal dependency.job_id job.id)
       in
       assert (Option.is_none child.result);
       print_endline "parent is waiting while the child's native permission is unresolved");
  [%expect
    {|
    parent is waiting while the child's native permission is unresolved
    saved wait resumed; interrupted child reported; no invocation replay
    |}]
;;

let%expect_test "cancelling a waiting parent cancels the entire owned dependency chain" =
  let agent =
    Background_pending_tests.source `Standalone
    ^ {|
<script id="forward" language="chatml" kind="tool">
let run ctx input = Task.bind(Job.start_tool("pending", input), fun id ->
  Task.pure(`Pending(`Job(id), `String("forwarded"))))
</script>
<tool name="forward" type="chatml" script="forward" entrypoint="run" input_schema="any.json" output_schema="string.json"><uses tool="pending"/></tool>|}
  in
  with_background_daemon
    ~agent
    ~sources:[ "any.json", "true"; "string.json", {|{"type":"string"}|} ]
    ~profile:{ permission_profile with tool_default = Ask }
    (fun env client entry capabilities ->
       let parent = submit entry (B.to_json (tool capabilities "forward" (`Object []))) in
       List.iter [ "forward"; "pending" ] ~f:(fun name ->
         let permission = Background_moderator_tests.pending_permission env entry.actor in
         [%test_eq: string] name permission.tool_name;
         A.resolve_permission_as_system
           entry.actor
           ~permission_id:permission.id
           ~permission_generation:permission.generation
           ~choice:Approve_once
           ~reason:None
         |> protocol_ok
         |> ignore);
       let middle = waiting env entry.actor parent.id in
       let leaf = waiting env entry.actor middle.job_id in
       let permission = Background_moderator_tests.pending_permission env entry.actor in
       [%test_eq: string] "read_file" permission.tool_name;
       A.cancel_job_internal entry.actor ~job_id:parent.id |> protocol_ok |> ignore;
       let state = A.state entry.actor |> protocol_ok in
       [%test_eq: int] 3 (List.length state.jobs);
       assert (
         List.exists state.jobs ~f:(fun job ->
           Agent_protocol.Id.Job.equal job.id leaf.job_id));
       List.iter state.jobs ~f:(fun job ->
         let _, completion = await env client job in
         match completion with
         | Cancelled _ -> ()
         | _ -> raise_s [%sexp (completion : Completion.t)]);
       (match
          A.resolve_permission_as_system
            entry.actor
            ~permission_id:permission.id
            ~permission_generation:permission.generation
            ~choice:Approve_once
            ~reason:None
        with
        | Error { code = Already_resolved; _ } -> ()
        | _ -> failwith "late approval escaped dependency cancellation");
       Background_moderator_tests.await_cleanup env entry.actor |> ignore;
       print_endline
         "three jobs cancelled atomically; leaf approval retired; workers cleaned up");
  [%expect
    {| three jobs cancelled atomically; leaf approval retired; workers cleaned up |}]
;;
