open Core
open Agent_server_test_support
open Background_fixtures

let agent =
  native_agent
  ^ {|
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event = fun ctx state event -> match event with
| `Tool_invoked(p) -> Task.bind(Tool.call("read_file", `Object([
    {key = "root"; value = `String("reports")},
    {key = "file"; value = `String("second.txt")}
  ])), fun result -> match result with
    | `Error(code) -> Task.fail(code)
    | `Ok(value) -> Task.bind(Invocation.resolve(p.context.invocation_id,
        `Complete(`String(to_string(state + 1)))), fun ignored -> Task.pure(state + 1)))
| _ -> Task.pure(state)
</script>
<tool name="counter" type="moderator" moderator="owner" input_schema="input.json" output_schema="output.json"/>|}
;;

let sources =
  [ "input.json", {|{"type":"object"}|}; "output.json", {|{"type":"string"}|} ]
;;

let rec pending_permission env actor =
  let state = A.state actor |> protocol_ok in
  match
    List.find state.permissions ~f:(fun permission ->
      Agent_protocol.Permission.equal_state permission.state Pending)
  with
  | Some permission -> permission
  | None ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    pending_permission env actor
;;

let rec await_cleanup env actor =
  let state = A.state actor |> protocol_ok in
  match
    List.for_all state.invocations ~f:(fun invocation ->
      match invocation.status with
      | Resolved _ | Published _ -> true
      | _ -> false)
  with
  | true -> state
  | false ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    await_cleanup env actor
;;

let%expect_test
    "background jobs invoke the real stateful moderator with native descendants"
  =
  with_background_daemon ~agent ~sources (fun env client entry capabilities ->
    List.iter [ 1; 2 ] ~f:(fun expected ->
      let job = submit entry (B.to_json (tool capabilities "counter" (`Object []))) in
      let _, completion = await env client job in
      (match completion with
       | Succeeded (`String count) -> [%test_eq: string] (Int.to_string expected) count
       | other -> raise_s [%sexp (other : Completion.t)]);
      print_s [%sexp (completion : Completion.t)]);
    let state = A.state entry.actor |> protocol_ok in
    let handlers =
      List.filter state.invocations ~f:(fun invocation ->
        String.equal invocation.context.tool_name "counter")
    in
    [%test_eq: int] 2 (List.length handlers);
    List.iter handlers ~f:(fun handler ->
      assert (Option.is_some handler.context.parent_invocation);
      let children =
        List.filter state.invocations ~f:(fun child ->
          Option.exists
            child.context.parent_invocation
            ~f:(Agent_protocol.Id.Invocation.equal handler.context.id))
      in
      [%test_eq: int] 1 (List.length children);
      let child = List.hd_exn children in
      assert (Agent_protocol.Invocation.equal_origin child.context.origin Moderator);
      assert (
        Option.equal
          Agent_protocol.Timestamp.equal
          handler.context.deadline
          child.context.deadline)));
  [%expect
    {|
    (Succeeded (String 1))
    (Succeeded (String 2))
    |}]
;;

let%expect_test
    "job cancellation owns approvals for moderator handlers and their native children"
  =
  List.iter [ false; true ] ~f:(fun approve_handler ->
    with_background_daemon
      ~agent
      ~sources
      ~profile:{ permission_profile with tool_default = Ask }
      (fun env _client entry capabilities ->
         let job = submit entry (B.to_json (tool capabilities "counter" (`Object []))) in
         let permission = pending_permission env entry.actor in
         [%test_eq: string] "counter" permission.tool_name;
         let permission =
           match approve_handler with
           | false -> permission
           | true ->
             A.resolve_permission_as_system
               entry.actor
               ~permission_id:permission.id
               ~permission_generation:permission.generation
               ~choice:Approve_once
               ~reason:(Some "fixture authorizes handler only")
             |> protocol_ok
             |> ignore;
             let permission = pending_permission env entry.actor in
             [%test_eq: string] "read_file" permission.tool_name;
             permission
         in
         A.cancel_job_internal entry.actor ~job_id:job.id |> protocol_ok |> ignore;
         (match
            A.resolve_permission_as_system
              entry.actor
              ~permission_id:permission.id
              ~permission_generation:permission.generation
              ~choice:Approve_once
              ~reason:None
          with
          | Error error ->
            print_s
              [%sexp
                (permission.tool_name : string), (error.code : Agent_protocol.Error.code)]
          | Ok _ -> failwith "late approval revived cancelled background work");
         let state = await_cleanup env entry.actor in
         let job =
           List.find_exn state.jobs ~f:(fun current ->
             Agent_protocol.Id.Job.equal current.id job.id)
         in
         print_s
           [%sexp
             (Completion.of_json (Option.value_exn job.result) |> protocol_ok
              : Completion.t)]));
  [%expect
    {|
    (counter Already_resolved)
    (Cancelled "job cancelled")
    (read_file Already_resolved)
    (Cancelled "job cancelled")
    |}]
;;
