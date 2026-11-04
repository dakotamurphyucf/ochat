open! Core
open Agent_server_test_support
open Background_fixtures
module P = Agent_protocol

let%expect_test "synchronous Model preparation rejection retires its claimed Job" =
  let reject_context = ref false in
  let rejected = ref 0 in
  let agent =
    native_agent
    ^ {|
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = match event with
| `Tool_invoked(p) ->
  let* message = Task.catch(
    (let* ignored = Model.call("agent_prompt_v1", `Object([
      {key = "prompt"; value = `String("recipe.chatmd")},
      {key = "input"; value = `String("must not submit")},
      {key = "is_local"; value = `Bool(true)}
    ])) in Task.pure("unexpected model return")),
    fun message -> Task.pure(message)) in
  let* ignored = Invocation.resolve(p.context.invocation_id, `Complete(`String(message))) in
  Task.pure(state + 1)
| _ -> Task.pure(state)
</script>
<tool name="prepare_probe" type="moderator" moderator="owner" input_schema="input.json" output_schema="output.json"/>|}
  in
  with_background_daemon
    ~agent
    ~sources:
      [ "input.json", {|{"type":"object"}|}
      ; "output.json", {|{"type":"string"}|}
      ; "recipe.chatmd", "<developer>Independent recipe.</developer>"
      ]
    ~map_inference_policy:(fun policy ->
      { policy with
        Agent_server.Session_factory.resolve_inference_context =
          (fun target ->
            if !reject_context
            then (
              incr rejected;
              Error Inference_runtime.Preparation_error.Target_denied)
            else policy.resolve_inference_context target)
      })
    (fun env client entry capabilities ->
       let before = A.state entry.actor |> protocol_ok in
       assert (
         Option.value_map before.run_state ~default:true ~f:(fun index ->
           List.is_empty (Agent_session.Run_state.runs index)));
       reject_context := true;
       let outer =
         (* The test host explicitly classifies this unrelated root. This does
            not qualify production provenance capture; real claim/scope gates
            still govern its Model child. Missing historical provenance is tested
            separately through document restoration. *)
         submit
           ~inference_origin:(Some P.Inference_origin.Unbound)
           entry
           (B.to_json (tool capabilities "prepare_probe" (`Object [])))
       in
       let _, completion = await env client outer in
       (match completion with
        | Succeeded (`String message) ->
          if not (String.is_substring message ~substring:"Target_denied")
          then
            raise_s
              [%sexp
                "unexpected caught model failure", (message : string), (!rejected : int)]
        | _ ->
          let state = A.state entry.actor |> protocol_ok in
          let model_jobs =
            List.filter_map state.jobs ~f:(fun job ->
              Option.some_if
                (P.Job.equal_kind job.kind Model_call)
                (job.attempt, job.status))
          in
          raise_s
            [%sexp
              (completion : Completion.t)
            , { rejected_contexts = (!rejected : int)
              ; model_jobs : (int * P.Job.status) list
              ; attempts =
                  (List.length
                     (Agent_session.Inference_ledger.rows state.inference_ledger)
                   : int)
              }]);
       [%test_eq: int] 1 !rejected;
       let state = A.state entry.actor |> protocol_ok in
       let model =
         List.filter state.jobs ~f:(fun job -> P.Job.equal_kind job.kind Model_call)
         |> fun jobs ->
         [%test_eq: int] 1 (List.length jobs);
         List.hd_exn jobs
       in
       [%test_eq: int] 1 model.attempt;
       (match model.status with
        | Failed error ->
          assert (String.is_substring error.message ~substring:"Target_denied")
        | _ -> raise_s [%sexp (model.status : P.Job.status)]);
       assert (Option.is_some model.completed_at);
       assert (List.is_empty (Agent_session.Inference_ledger.rows state.inference_ledger));
       reject_context := false);
  print_endline "claimed Model Job fails durably; no provider attempt or request";
  [%expect {| claimed Model Job fails durably; no provider attempt or request |}]
;;
