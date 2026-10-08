open! Core
module R = Inference.Request
module E = Inference.Event
module Client = Inference_client
module Res = Openai.Responses

let ok result =
  Result.map_error result ~f:(fun _ -> "fixture test admission") |> Result.ok_or_failwith
;;

let call status arguments : Res.Function_call.t =
  { id = Some "provider-call"
  ; call_id = "actual-alias"
  ; name = "inspect"
  ; arguments
  ; status = Some status
  ; _type = "function_call"
  }
;;

let stream ~sw:_ ~inputs:_ =
  let added = Res.Response_stream.Item.Function_call (call "in_progress" "") in
  let completed =
    Res.Response_stream.Item.Function_call (call "completed" "{\"path\":\"exact\"}")
  in
  [ Res.Response_stream.Output_item_added
      { item = added; output_index = 0; type_ = "response.output_item.added" }
  ; Function_call_arguments_delta
      { delta = "{\"path\":\"exact\"}"
      ; item_id = "provider-call"
      ; output_index = 0
      ; type_ = "response.function_call_arguments.delta"
      }
  ; Output_item_done
      { item = completed; output_index = 0; type_ = "response.output_item.done" }
  ]
  |> Stdlib.List.to_seq
;;

let run ~on_event ~on_completion =
  let fixture =
    Inference_fixture.create
      ~namespace:"compound-fixture"
      ~default_model:"explicit"
      ~post_stream:stream
  in
  let target =
    Inference_fixture.capture_config fixture Chat_response.Config.default |> ok
  in
  let context = Inference_fixture.resolve fixture target |> ok in
  let request =
    R.create
      ~target
      ~history:[]
      ~tools:[]
      ~assets:[]
      ~limits:Document_schema.Limits.default
    |> ok
  in
  Eio_main.run (fun _ ->
    Eio.Switch.run (fun sw ->
      Client.run
        context
        ~sw
        ~identity:(Inference_fixture.identity fixture)
        ~relation:Root
        ~request
        ~before_dispatch:ignore
        ~on_attempt:ignore
        ~on_completion
        ~on_event
        ~on_observation:ignore))
;;

let%test_unit "synthetic compound stream finalizes once with reconstructed evidence" =
  let seen = ref [] in
  let completions = ref 0 in
  let result =
    run
      ~on_completion:(fun _ -> Int.incr completions)
      ~on_event:(fun event ->
        match E.view event with
        | Live stream ->
          (match Transcript.Stream.view stream with
           | Source_started { origin; _ } ->
             assert (not (History_entry.Payload.Origin.is_available origin));
             seen := "source" :: !seen
           | Item_announced _ -> seen := "announced" :: !seen
           | Changed _ -> seen := "changed" :: !seen
           | _ -> assert false)
        | Candidate_ready { payload; local_execution; _ } ->
          assert (E.equal_local_execution local_execution Tool_candidate);
          (match History_entry.Payload.representation payload with
           | Reconstructed _ -> ()
           | Authored | Captured _ -> assert false);
          seen := "candidate" :: !seen
        | Terminal _ -> seen := "terminal" :: !seen)
    |> ok
  in
  assert (
    List.equal
      String.equal
      (List.rev !seen)
      [ "source"; "announced"; "changed"; "candidate"; "terminal" ]);
  assert (Int.equal (List.length (Inference_runtime.Receipt.output result)) 1);
  assert (Int.equal !completions 1)
;;

let%test_unit "synthetic observer exceptions propagate with interruption evidence" =
  let completion = ref None in
  (match
     run
       ~on_completion:(fun value -> completion := Some value)
       ~on_event:(fun event ->
         match E.view event with
         | Candidate_ready _ -> raise Exit
         | Live _ | Terminal _ -> ())
   with
   | exception Exit -> ()
   | exception exn -> raise exn
   | Ok _ | Error _ -> assert false);
  match Client.Completion.outcome (Option.value_exn !completion) with
  | Interrupted { reason = Host_interrupted; delivery = Response_started } -> ()
  | Returned _ | Interrupted _ -> assert false
;;
