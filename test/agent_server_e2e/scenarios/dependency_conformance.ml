open! Core
module P = Agent_protocol
module D = P.Prompt_dependency_selection

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp "dependency conformance rejected", (error : P.Error.t)]
;;

let check ~definition ~request ~key_prefix =
  let command (command : P.Command.t) =
    request command |> ok |> Support.Public_view.non_history
  in
  let status () =
    match command P.Command.Prompt_dependencies_status with
    | Prompt_dependencies_status status -> status
    | _ -> failwith "dependency status response kind"
  in
  let before = status () in
  let generation =
    match before.D.Status.history, before.publication with
    | Tracked generation, Available -> generation
    | _ -> failwith "fixture dependency history is not tracked and available"
  in
  let update key =
    D.Update_request.create
      ~definitions:[ definition ]
      ~expected_generation:generation
      ~idempotency_key:(P.Idempotency_key.of_string (key_prefix ^ key) |> ok)
    |> ok
  in
  let original_request = update ":update" in
  let original = P.Command.Prompt_dependencies_update original_request in
  let receipt () =
    match
      command
        (Command_receipt
           { method_name = P.Command.method_name original
           ; original_params = P.Command.params original
           })
    with
    | Command_receipt receipt -> receipt
    | _ -> failwith "dependency receipt response kind"
  in
  (match receipt () with
   | Missing -> ()
   | _ -> failwith "unsubmitted dependency update has a receipt");
  let execute () =
    match command original with
    | Prompt_dependencies_update result -> result
    | _ -> failwith "dependency update response kind"
  in
  let result = execute () in
  assert (D.Update_result.matches_request result original_request);
  assert (D.Generation.equal (D.Update_result.previous_generation result) generation);
  assert (
    Int.equal
      (D.Generation.to_int (D.Update_result.generation result))
      (D.Generation.to_int generation + 1));
  let selected_revision =
    match D.Update_result.targets result with
    | [ { definition_id; outcome = Ready revision } ] ->
      assert (P.Id.Prompt_definition.equal definition_id definition);
      revision
    | _ -> failwith "dependency update did not install its authentic ready target"
  in
  (match command (Prompt_get { prompt_id = definition }) with
   | Prompt_get prompt ->
     assert (
       Option.equal
         P.Id.Prompt_revision.equal
         prompt.current_revision
         (Some selected_revision))
   | _ -> failwith "dependency target catalog response kind");
  (match receipt () with
   | Committed (Prompt_dependencies_updated recorded) ->
     assert (D.Update_result.equal result recorded)
   | _ -> failwith "dependency update receipt does not match its committed transition");
  assert (D.Update_result.equal result (execute ()));
  let installed = status () in
  assert (
    D.Status.equal
      installed
      { history = Tracked (D.Update_result.generation result); publication = Available });
  (match request (Prompt_dependencies_update (update ":stale")) with
   | Error error -> assert (P.Error.equal_code error.code Invalid_state)
   | Ok _ -> failwith "fresh stale dependency update was admitted");
  assert (D.Status.equal installed (status ()))
;;
