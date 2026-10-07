open! Core

type t =
  { context : Inference_runtime.Context.t
  ; identity : Chat_response.Neutral_turn.Identity.t
  }

let create
      ?(post_stream = fun ~sw:_ ~inputs:_ -> failwith "unexpected fixture inference")
      ~config
      ()
  =
  let fixture =
    Inference_fixture.create
      ~namespace:
        (Agent_protocol.Id.Transaction.create ()
         |> Agent_protocol.Id.Transaction.to_string)
      ~default_model:"o3"
      ~post_stream
  in
  let target =
    Inference_fixture.capture_config fixture config
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  let context =
    (Inference_fixture.resolve fixture) target
    |> Result.map_error ~f:(fun error ->
      Sexp.to_string_hum (Inference_runtime.Preparation_error.sexp_of_t error))
    |> Result.ok_or_failwith
  in
  { context; identity = Inference_fixture.identity fixture }
;;

let execution t =
  Inference_client.Execution.create
    ~context:t.context
    ~identity:t.identity
    ~relation:Root
    ~before_dispatch:ignore
    ~on_attempt:ignore
    ~on_completion:ignore
    ~on_observation:ignore
;;

let compaction_execution () =
  let item =
    Openai.Responses.Response_stream.Item.Output_message
      { role = Assistant
      ; id = "fixture-compaction-summary"
      ; content =
          [ { annotations = []; text = "Offline fixture summary"; _type = "output_text" }
          ]
      ; status = "completed"
      ; phase = None
      ; _type = "message"
      }
  in
  let post_stream ~sw:_ ~inputs:_ =
    Stdlib.List.to_seq
      [ Openai.Responses.Response_stream.Output_item_done
          { item; output_index = 0; type_ = "response.output_item.done" }
      ]
  in
  create ~post_stream ~config:Chat_response.Config.default () |> execution
;;

let install_compaction_runtime actor =
  let worker =
    Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _ ->
      failwith "compaction-only fixture attempted a foreground turn")
  in
  Agent_session.Session_actor.set_runtime_worker
    actor
    ~worker:(Some worker)
    ~inference:(Some (compaction_execution ()))
;;
