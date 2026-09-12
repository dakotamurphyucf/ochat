open Core
open Authoring_evaluation
open Runner
module D = Driver

(* This executable deliberately links only the offline provider. A future live
   adapter must enter Driver through the explicit real-model authorization gate. *)
let () =
  let seeds = ref [] in
  let check = ref false in
  let case_timeout = ref 120. in
  let transcript_bytes = ref 8000000 in
  Stdlib.Arg.parse
    [ ( "--seed"
      , Stdlib.Arg.Int (fun seed -> seeds := Some seed :: !seeds)
      , "Record a seed/repetition; repeat this option for paired repetitions" )
    ; ( "--check"
      , Stdlib.Arg.Set check
      , "Verify the offline transcript contract and print a concise result" )
    ; ( "--case-timeout"
      , Stdlib.Arg.Set_float case_timeout
      , "Cooperative per-case deadline in seconds" )
    ; ( "--transcript-bytes"
      , Stdlib.Arg.Set_int transcript_bytes
      , "Maximum serialized exchange bytes per case" )
    ]
    (fun argument -> raise (Stdlib.Arg.Bad ("unexpected argument: " ^ argument)))
    "evaluate.exe [options] > private-evaluation.json (offline only)";
  let binary_digest =
    In_channel.read_all Stdlib.Sys.executable_name |> Chatmd_shell_spec.Source_ref.digest
  in
  let config : D.config =
    { model = "offline-scripted-author-v1"
    ; model_parameters = `Object []
    ; seeds =
        (match !seeds with
         | [] -> [ None ]
         | seeds -> List.rev seeds)
    ; provenance = Offline_transcript
    ; runtime_revision = "evaluation-binary-sha256:" ^ binary_digest
    ; oracle_revision = "evaluation-binary-sha256:" ^ binary_digest
    ; provider_timeout_seconds = 10.
    ; case_timeout_seconds = !case_timeout
    ; max_transcript_bytes = !transcript_bytes
    }
  in
  let artifact =
    Eio_main.run (fun env ->
      D.run
        ~env
        ~config
        ~with_backend:(Suite.with_backend ~env ~runtime_revision:config.runtime_revision)
        ~make_provider:Authoring_evaluation_fixtures.Offline_transcript.make_provider
        ())
  in
  (* Exercise the same persisted representation consumed by later reporting. *)
  let encoded = D.jsonaf_of_artifact artifact in
  let restored = Jsonaf.to_string encoded |> Jsonaf.of_string |> D.artifact_of_jsonaf in
  let report = Report.create restored |> Result.ok_or_failwith in
  match !check with
  | false ->
    print_endline
      (Jsonaf.to_string
         (`Object [ "artifact", encoded; "report", Report.jsonaf_of_t report ]))
  | true ->
    List.iter artifact.rows ~f:(fun row ->
      let fail () = raise_s [%sexp (row : D.row)] in
      match row.result with
      | Some result when result.runtime_success ->
        (match row.task_id, result.attempts with
         | ( "ocaml-transfer-repair"
           , [ { validation = Invalid (Semantics, _); execution = None }
             ; { validation = Valid; execution = Some (Failed (Semantics, _)) }
             ; { validation = Valid; execution = Some Passed }
             ] ) -> ()
         | ( "missing-process-capability"
           , [ { validation = Invalid (Semantics, _); execution = None }
             ; { validation = Valid; execution = Some Passed }
             ] ) -> ()
         | ( "compacted-event-repair"
           , [ { validation = Valid; execution = Some (Failed (Semantics, _)) }
             ; { validation = Valid; execution = Some Passed }
             ] )
           when result.compactions = 1 && result.retrieval_calls = 2 -> ()
         | ( ( "one-off-reconcile"
             | "standalone-delta"
             | "moderator-quota"
             | "async-observe-once"
             | "child-evidence-review" )
           , [ { validation = Valid; execution = Some Passed } ] ) -> ()
         | _ -> fail ())
      | _ -> fail ());
    assert (String.equal report.real_model_evaluation "not_run");
    assert (Report.equal_verdict report.threshold_verdict Report.Incomplete);
    List.iter report.policies ~f:(fun policy ->
      assert (policy.infrastructure_cases = 0 && policy.estimated_costs_complete);
      assert (Option.is_none policy.provider_input_tokens);
      assert (Float.equal policy.first_pass_compile_rate 0.75);
      assert (Float.equal policy.runtime_success_rate 1.));
    printf
      "%d offline cases passed across all three guidance conditions; repair and \
       context-loss evidence retained; real-model evaluation not run\n"
      (List.length artifact.rows)
;;
