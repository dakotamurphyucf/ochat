open Core
open Fixtures
open Agent_server_test_support
module P = Agent_protocol

let base_sources ~reject ~repetitions =
  Standalone_notification_tests.sources ~reject
  |> List.map ~f:(fun (name, source) ->
    ( name
    , match name with
      | "agent.chatmd" ->
        source
        |> String.substr_replace_all ~pattern:{|stdout="4KiB"|} ~with_:{|stdout="256KiB"|}
        |> String.substr_replace_all
             ~pattern:{|total_output="8KiB"|}
             ~with_:{|total_output="260KiB"|}
        |> String.substr_replace_all
             ~pattern:{|kind="tool" src="begin.chatml"|}
             ~with_:
               {|kind="tool" max_output="512KiB" max_value="1MiB" src="begin.chatml"|}
      | "work.sh" ->
        source
        ^ "\ni=0\nwhile [ \"$i\" -lt "
        ^ Int.to_string repetitions
        ^ " ]; do\n"
        ^ "  printf 'PRIVATE-LARGE-COMPLETION-0123456789012345678901234567890123456789\\n'\n"
        ^ "  i=$((i + 1))\ndone\n"
      | _ -> source ))
;;

let sources ~reject ~repetitions ~failed =
  let sources = base_sources ~reject ~repetitions in
  match failed with
  | false -> sources
  | true ->
    ( "failed.chatml"
    , {|let run ctx input =
  let* result = Tool.call("fixture_work", input) in
  match result with
  | `Ok(value) -> Task.pure(`Fail({code = "business.failed"; message = "Retained failure details"; retryable = false; details = value}))
  | `Error(code) -> Task.fail(code)|}
    )
    :: List.map sources ~f:(fun (name, source) ->
      ( name
      , match name with
        | "agent.chatmd" ->
          String.substr_replace_all
            source
            ~pattern:{|<uses tool="fixture_work"/>|}
            ~with_:{|<uses tool="failed_work"/>|}
          ^ {|<script id="failed" language="chatml" kind="tool" max_output="512KiB" max_value="1MiB" src="failed.chatml"/>
<tool name="failed_work" type="chatml" script="failed" entrypoint="run" input_schema="input.json" output_schema="accepted.json"><uses tool="fixture_work"/></tool>|}
        | "begin.chatml" ->
          String.substr_replace_all source ~pattern:"fixture_work" ~with_:"failed_work"
        | _ -> source ))
;;

let%expect_test
    "large standalone results publish bounded references and artifacts survive \
     authorized reload reads"
  =
  List.iter
    [ false, false, false; true, false, false; true, false, true; true, true, false ]
    ~f:(fun (artifact, reject, failed) ->
      let received = ref None in
      let client = ref None in
      let factory_limits = Agent_server.Daemon.default_options.factory_limits in
      let factory_limits =
        { factory_limits with
          notifications = { factory_limits.notifications with max_payload_bytes = 2048 }
        }
      in
      with_daemon
        ~factory_limits
        ~sources:(sources ~reject ~failed ~repetitions:(if artifact then 2048 else 64))
        ~calls:[ "begin", "begin_work", `Object [] ]
        ~expected_requests:3
        ~connect:(fun ~sw:_ ~env:_ ~root:_ daemon ->
          let value = connection daemon (principal ()) in
          client := Some value;
          value)
        ~inspect_request:(fun request inputs ->
          match request with
          | 3 ->
            let text =
              List.find_map_exn inputs ~f:(function
                | Openai.Responses.Item.Input_message
                    { role = User; content = Text { text; _ } :: _; _ }
                  when String.is_prefix text ~prefix:"Ochat runtime notification." ->
                  Some text
                | _ -> None)
            in
            assert (String.length text < 4096);
            assert (not (String.is_substring text ~substring:"PRIVATE-LARGE-COMPLETION"));
            received := Some (String.lsplit2_exn text ~on:'\n' |> snd |> Jsonaf.of_string)
          | _ -> ())
        ~after_turn:(fun env handle entry ->
          let state = A.state entry.actor |> protocol_ok in
          let workspace = state.spec.workspace_instance.canonical_root.native_path in
          let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
          Job_launch_tests.wait env (fun () ->
            Eio.Path.is_file (file "fixture-work.started"));
          Eio.Path.save ~create:(`Exclusive 0o600) (file "fixture-work.release") "finish";
          Job_launch_tests.wait env (fun () ->
            let state = A.state entry.actor |> protocol_ok in
            Option.is_some !received
            && Option.is_none state.active_operation
            &&
            match state.deliveries with
            | [ { status = Committed _; wake_disposition = Some (Accepted_wake _); _ } ]
              -> true
            | _ -> false);
          let saved = A.state entry.actor |> protocol_ok in
          let job = List.hd_exn saved.jobs in
          let stored = P.Job.terminal_result job |> protocol_ok |> Option.value_exn in
          let original =
            match stored with
            | Inline completion ->
              assert (not artifact);
              completion
            | Artifact { reference; _ } ->
              assert artifact;
              Background_artifact_tests.read_artifact (Option.value_exn !client) reference
              |> protocol_ok
          in
          assert (P.Stored_completion.matches stored original |> protocol_ok);
          (match failed, original with
           | false, Succeeded _ -> ()
           | true, Failed error -> [%test_eq: string] "business.failed" error.code
           | _ -> failwith "original business outcome changed");
          assert (
            String.is_substring
              (P.Completion.to_json original |> Jsonaf.to_string)
              ~substring:"PRIVATE-LARGE-COMPLETION");
          let delivery = List.hd_exn saved.deliveries in
          let receipt = Option.value_exn delivery.completion_projection in
          [%test_eq: bool] reject receipt.rejected;
          let notice = Option.value_exn !received in
          (match reject, receipt.result_reference with
           | true, None ->
             [%test_eq: string]
               "1"
               (Jsonaf.member_exn "version" notice |> Jsonaf.to_string);
             (match delivery.context.completion with
              | Failed error ->
                [%test_eq: string] "background.invalid_completion" error.code
              | _ -> failwith "invalid result was disclosed")
           | false, Some reference ->
             [%test_eq: string]
               "2"
               (Jsonaf.member_exn "version" notice |> Jsonaf.to_string);
             [%test_eq: string]
               "retained_result"
               (Jsonaf.member_exn "completion_representation" notice |> Jsonaf.string_exn);
             [%test_eq: bool] artifact (Option.is_some reference.artifact);
             assert (
               P.Stored_completion.equal_outcome
                 reference.outcome
                 (if failed then Failed else Succeeded));
             P.Job_result_reference.validate_job reference job |> protocol_ok;
             assert (
               P.Job_result_reference.equal
                 reference
                 (P.Job_result_reference.of_json
                    (P.Job_result_reference.to_json reference)
                  |> protocol_ok));
             let encoded = P.Completion_projection.to_json receipt in
             let downgraded =
               match encoded with
               | `Object fields ->
                 `Object
                   (List.Assoc.add fields ~equal:String.equal "version" (`Number "1"))
               | _ -> assert false
             in
             assert (Result.is_error (P.Completion_projection.of_json downgraded));
             let old_job = { job with attempt = job.attempt + 1 } in
             assert (
               Result.is_error (P.Job_result_reference.validate_job reference old_job))
           | _ -> failwith "wrong reference projection");
          H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
          Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          Agent_server.Runtime_owner.drain_idle_moderator entry.runtime
          |> protocol_ok
          |> ignore;
          let reloaded = A.state entry.actor |> protocol_ok in
          assert (P.Delivery.equal delivery (List.hd_exn reloaded.deliveries));
          (match stored with
           | Inline _ -> ()
           | Artifact { reference; _ } ->
             let reread =
               Background_artifact_tests.read_artifact
                 (Option.value_exn !client)
                 reference
               |> protocol_ok
             in
             assert (P.Completion.equal original reread));
          [%test_eq: string] "started\n" (Eio.Path.load (file "fixture-work.started"));
          print_s
            [%sexp
              (artifact : bool)
            , (reject : bool)
            , (failed : bool)
            , "bounded notification; exact retained result; no replay"])
        ~settle:Job_launch_tests.settle
        (fun _ -> ()));
  [%expect
    {|
    (false false false "bounded notification; exact retained result; no replay")
    (true false false "bounded notification; exact retained result; no replay")
    (true false true "bounded notification; exact retained result; no replay")
    (true true false "bounded notification; exact retained result; no replay")
  |}]
;;
