open Core
open Agent_server_test_support
open Background_fixtures
module P = Agent_protocol
module Stored = P.Stored_completion

let source =
  {|<script id="large" language="chatml" kind="tool">
let rec grow n text = if n == 0 then text else grow(n - 1, text ++ text)
let run ctx input = Task.pure(`Complete(`String(grow(17, "x"))))
</script>
<tool name="large" type="chatml" script="large" entrypoint="run" input_schema="any.json" output_schema="string.json"/>
<script id="pending" language="chatml" kind="tool">
let run ctx input = Task.bind(Job.start_tool("large", input), fun id ->
  Task.pure(`Pending(`Job(id), `String("accepted"))))
</script>
<tool name="pending" type="chatml" script="pending" entrypoint="run" input_schema="any.json" output_schema="string.json" completion_schema="string.json"><uses tool="large"/></tool>
<tool name="invalid" type="chatml" script="pending" entrypoint="run" input_schema="any.json" output_schema="string.json" completion_schema="short.json"><uses tool="large"/></tool>
<script id="reader" language="chatml" kind="tool">
let run ctx input = match input with
 | `String(id) -> Task.bind(Job.get(id), fun descriptor ->
   match Json.get_field(descriptor, "completion") with
   | `Some(stored) -> (match Json.get_field(stored, "type") with
     | `Some(`String("artifact")) -> Task.bind(Job.read_result(id), fun completion ->
       match Json.get_field(completion, "value") with
       | `Some(`String(text)) -> if String.length(text) == 131072 then
           Task.pure(`Complete(`String("verified"))) else Task.fail("incomplete artifact")
       | _ -> Task.fail("missing completion value"))
     | _ -> Task.fail("expected explicit artifact descriptor"))
   | _ -> Task.fail("missing stored result"))
 | _ -> Task.fail("expected job id")
</script>
<tool name="reader" type="chatml" script="reader" entrypoint="run" input_schema="string.json" output_schema="string.json"><uses tool="large"/></tool>
<script id="probe" language="chatml" kind="tool">
let run ctx input = match input with
 | `String(id) -> Task.bind(Task.catch(
     Task.bind(Job.read_result(id), fun ignored -> Task.pure("unexpected success")),
     fun message -> Task.pure(message)), fun message -> Task.pure(`Complete(`String(message))))
 | _ -> Task.fail("expected job id")
</script>
<tool name="probe" type="chatml" script="probe" entrypoint="run" input_schema="string.json" output_schema="string.json"><uses tool="large"/></tool>|}
;;

let key () =
  P.Id.Transaction.create ()
  |> P.Id.Transaction.to_string
  |> P.Idempotency_key.of_string
  |> protocol_ok
;;

let read_artifact client (reference : P.Job_artifact.t) =
  let attachment =
    match
      Agent_client.Connection.request
        client
        (Session_attach
           { session_id = reference.session_id
           ; requested_mode = Read_only
           ; subscribe = false
           ; after_sequence = None
           ; reclaim_token = None
           ; idempotency_key = key ()
           })
      |> protocol_ok
    with
    | Session_attach result -> result.attachment
    | _ -> failwith "expected artifact read attachment"
  in
  Exn.protect
    ~finally:(fun () ->
      Agent_client.Connection.request
        client
        (Session_detach
           { session_id = reference.session_id
           ; attachment_id = attachment.id
           ; idempotency_key = key ()
           })
      |> protocol_ok
      |> ignore)
    ~f:(fun () ->
      let bytes = Buffer.create 4096 in
      let rec read offset =
        match
          Agent_client.Connection.request
            client
            (Blob_read
               { session_id = reference.session_id
               ; attachment_id = attachment.id
               ; blob_id = reference.blob.id
               ; offset
               ; max_bytes = 4096
               })
          |> protocol_ok
        with
        | Blob_read chunk ->
          assert (
            Jsonaf.exactly_equal
              (P.Blob.Metadata.to_json chunk.blob)
              (P.Blob.Metadata.to_json reference.blob));
          Buffer.add_string bytes (Base64.decode_exn chunk.data_base64);
          (match chunk.eof with
           | true -> ()
           | false ->
             assert (Int64.(chunk.next_offset > offset));
             read chunk.next_offset)
        | _ -> failwith "expected artifact blob chunk"
      in
      read 0L;
      Buffer.contents bytes |> Jsonaf.of_string |> P.Completion.of_json)
;;

let rec await_result env client (job : J.t) =
  let current =
    match
      Agent_client.Connection.request
        client
        (Job_get { session_id = job.session_id; job_id = job.id })
      |> protocol_ok
    with
    | Job_get current -> current
    | _ -> failwith "expected job result"
  in
  match J.terminal_result current |> protocol_ok with
  | Some stored -> current, stored
  | None ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    await_result env client job
;;

let assert_large client (job : J.t) stored =
  (match stored with
   | Stored.Artifact { outcome = Succeeded; reference } ->
     assert (P.Id.Job.equal reference.job_id job.id);
     assert (String.length (Jsonaf.to_string (Option.value_exn job.result)) < 2048);
     assert (Result.is_error (J.terminal_completion job))
   | _ -> failwith "large job did not use an explicit artifact");
  let completion =
    Stored.materialize stored ~load:(read_artifact client) |> protocol_ok
  in
  match completion with
  | Succeeded (`String text) -> [%test_eq: string] (String.make 131072 'x') text
  | _ -> failwith "large completion changed"
;;

let%expect_test
    "artifact-backed jobs and dependencies remain readable after daemon restart"
  =
  with_background_daemon
    ~max_request_bytes:(64 * 1024)
    ~agent:source
    ~sources:
      [ "any.json", "true"
      ; "string.json", {|{"type":"string"}|}
      ; "short.json", {|{"type":"string","maxLength":4}|}
      ]
    ~after_recovery:(fun env client entry before ->
      let artifacts = ref 0 in
      List.iter before.jobs ~f:(fun job ->
        let current, stored = await_result env client job in
        match stored with
        | Artifact _ ->
          incr artifacts;
          assert_large client current stored
        | Inline (Failed { code = "background.invalid_completion"; _ })
        | Inline (Succeeded (`String "verified")) -> ()
        | _ -> failwith "unexpected restored outcome");
      [%test_eq: int] 4 !artifacts;
      print_endline
        "four exact job artifacts read through blob.read after restart; no model calls";
      let capabilities =
        Agent_server.Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
          Ok
            (Agent_session.Script_tool_calls.current_capabilities
               (Option.value_exn runtime.moderator_script_tools)))
        |> protocol_ok
      in
      let reader_selection =
        C.select capabilities ~names:[ "large" ]
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      let job, reference =
        List.find_map_exn before.jobs ~f:(fun job ->
          let request =
            B.of_json ~policy:Chat_response.One_off_request.default_policy job.payload
            |> protocol_ok
          in
          match J.terminal_result job |> protocol_ok with
          | Some (Artifact { reference; _ })
            when Result.is_ok
                   (B.validate_capabilities request ~capabilities:reader_selection) ->
            Some (job, reference)
          | _ -> None)
      in
      let store_handle = Option.value_exn entry.store_handle in
      let path =
        Filename.concat
          (Filename.concat
             (Agent_store.Session_store.Handle.directory store_handle)
             "blobs")
          (P.Id.Blob.to_string reference.blob.id ^ ".blob")
      in
      Eio.Path.save
        ~create:(`Or_truncate 0o600)
        Eio.Path.(Eio.Stdenv.fs env / path)
        "corrupt fixture";
      let reader =
        submit entry (B.to_json (tool capabilities "probe" (P.Id.Job.to_json job.id)))
      in
      let _, failure = await_result env client reader in
      match failure with
      | Inline (Succeeded (`String message)) ->
        assert (
          String.is_substring
            message
            ~substring:"saved job result is unavailable or failed verification");
        assert (
          not
            (String.is_substring
               (Stored.to_json failure |> Jsonaf.to_string)
               ~substring:path));
        print_endline
          "corrupt artifact read failed without exposing its storage path or replaying \
           work"
      | _ -> failwith "corrupt artifact was readable")
    (fun env client entry capabilities ->
       List.iter [ "large"; "pending"; "invalid" ] ~f:(fun name ->
         let job = submit entry (B.to_json (tool capabilities name `Null)) in
         let current, stored = await_result env client job in
         match name, stored with
         | ( "invalid"
           , Inline
               (Failed { code = "background.invalid_completion"; details = `Null; _ }) )
           ->
           print_endline
             "artifact child result failed the captured parent schema without disclosure"
         | ("large" | "pending"), _ ->
           assert_large client current stored;
           print_endline (name ^ ": bounded descriptor, complete verified blob");
           (match name with
            | "large" ->
              let reader =
                submit
                  entry
                  (B.to_json (tool capabilities "reader" (P.Id.Job.to_json job.id)))
              in
              let _, read = await_result env client reader in
              (match read with
               | Inline (Succeeded (`String "verified")) ->
                 print_endline
                   "Job.get descriptor and Job.read_result verified through a scoped \
                    script"
               | _ -> raise_s [%sexp (read : Stored.t)])
            | _ -> ())
         | _ -> failwith "unexpected artifact completion"));
  [%expect
    {|
    large: bounded descriptor, complete verified blob
    Job.get descriptor and Job.read_result verified through a scoped script
    pending: bounded descriptor, complete verified blob
    artifact child result failed the captured parent schema without disclosure
    four exact job artifacts read through blob.read after restart; no model calls
    corrupt artifact read failed without exposing its storage path or replaying work
    |}]
;;

let%expect_test "host result ceilings terminate publication without retrying execution" =
  List.iter
    [ 32 * 1024; 1 ]
    ~f:(fun job_result_max_bytes ->
      with_background_daemon
        ~agent:source
        ~job_result_max_bytes
        ~sources:
          [ "any.json", "true"
          ; "string.json", {|{"type":"string"}|}
          ; "short.json", {|{"type":"string","maxLength":4}|}
          ]
        (fun env client entry capabilities ->
           let created_at =
             match job_result_max_bytes with
             | 1 -> Some (P.Timestamp.of_string "2000-01-01T00:00:00Z" |> protocol_ok)
             | _ -> None
           in
           let job =
             submit ?created_at entry (B.to_json (tool capabilities "large" `Null))
           in
           let current, stored = await_result env client job in
           [%test_eq: int] 1 current.attempt;
           match job_result_max_bytes, stored with
           | 1, Inline Expired ->
             print_endline
               "expiry remains an explicit control outcome under a one-byte data ceiling"
           | ( _
             , Inline
                 (Failed
                    { code = "background.result_limit"
                    ; retryable = false
                    ; details = `Null
                    ; _
                    }) ) ->
             assert (String.length (Stored.to_json stored |> Jsonaf.to_string) < 512);
             print_endline
               "oversized completed result became one bounded terminal failure and \
                survived restart"
           | _ -> raise_s [%sexp (stored : Stored.t)]));
  [%expect
    {|
    oversized completed result became one bounded terminal failure and survived restart
    expiry remains an explicit control outcome under a one-byte data ceiling
    |}]
;;
