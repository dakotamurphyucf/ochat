open Core
open Authoring_evaluation
open Runner

let replace candidate name value =
  match candidate with
  | `Object fields -> `Object (List.Assoc.add fields name value ~equal:String.equal)
  | _ -> invalid_arg "candidate must be an object"
;;

let make_provider ~config:_ ~(task : task) ~policy:_ ~repetition:_ ~seed:_ ~step ~messages
  =
  let reference topic =
    Retrieve (Reference_backend.request ~task:task.family ~topic_id:topic "topic")
  in
  let decline = Decline "offline repair exhausted; inspect the recorded feedback" in
  let action =
    match task.id, step with
    | "ocaml-transfer-repair", 1 ->
      Submit (Solutions.count "let main input = Task.pure input")
    | "ocaml-transfer-repair", 2 -> reference "chatml.syntax.calls"
    | "ocaml-transfer-repair", 3 ->
      Submit (Solutions.count "let main input = Task.pure(`Number(0.0))")
    | "ocaml-transfer-repair", 4 ->
      Submit (Solutions.count [%blob "fixtures/count.chatml"])
    | "missing-process-capability", 1 ->
      Submit
        (replace
           Solutions.digest
           "source"
           (`String
               "let initial_state = 0\n\
                let on_event ctx state event = let* value = Process.run(\"echo\", `Null) \
                in Task.pure(state)"))
    | "missing-process-capability", 2 -> reference "runtime.invocations.moderator"
    | "missing-process-capability", 3 -> Submit Solutions.digest
    | "compacted-event-repair", 1 -> reference "runtime.invocations.moderator"
    | "compacted-event-repair", 2 ->
      Submit
        (replace
           Solutions.tally
           "source"
           (`String
               "let initial_state = 0\nlet on_event ctx state event = Task.pure(state)"))
    | "compacted-event-repair", 3 ->
      assert (
        not (List.exists messages ~f:(fun m -> equal_category m.category Documentation)));
      assert (
        List.exists messages ~f:(fun m ->
          String.is_substring m.text ~substring:"Earlier reference text was compacted"));
      reference "runtime.invocations.moderator"
    | "compacted-event-repair", 4 ->
      assert (
        List.exists messages ~f:(fun m ->
          equal_category m.category Documentation
          && String.is_substring m.text ~substring:"Tool_invoked"));
      Submit Solutions.tally
    | ( ("ocaml-transfer-repair" | "missing-process-capability" | "compacted-event-repair")
      , _ ) -> decline
    | _, 1 -> Retrieve (Reference_backend.request ~task:task.family "prepare")
    | "one-off-reconcile", 2 -> Submit Solutions.reconciliation
    | "standalone-delta", 2 -> Submit Solutions.delta
    | "moderator-quota", 2 -> Submit Solutions.quota
    | "async-observe-once", 2 -> Submit Solutions.background
    | "child-evidence-review", 2 -> Submit Solutions.child
    | _, _ -> decline
  in
  { action; provider_input_tokens = None }
;;
