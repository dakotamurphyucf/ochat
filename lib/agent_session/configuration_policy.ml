open! Core

type t =
  { select_profile :
      current:Inference.Request.Target.t
      -> profile:string
      -> (Inference.Request.Target.t, Agent_protocol.Error.t) Result.t
  ; approve :
      current:Inference.Request.Target.t
      -> proposed:Inference.Request.Target.t
      -> (unit, Agent_protocol.Error.t) Result.t
  ; resolve :
      Inference.Request.Target.t
      -> (Inference_runtime.Context.t, Agent_protocol.Error.t) Result.t
  }

let invalid _ =
  Agent_protocol.Error.invalid_request
    "selected model/settings or retained history are incompatible"
;;

let validate t ~current ~proposed ~history =
  let open Result.Let_syntax in
  let%bind () = t.approve ~current ~proposed in
  let%bind context = t.resolve proposed in
  let%bind () =
    Inference_runtime.Context.preflight_history context history
    |> Result.map_error ~f:invalid
  in
  let%bind request =
    Inference.Request.create
      ~target:proposed
      ~history:[]
      ~tools:[]
      ~assets:[]
      ~limits:Transcript.Admission.default
    |> Result.map_error ~f:invalid
  in
  Inference_runtime.Context.prepare
    context
    ~preparation_id:"configuration-validation"
    request
  |> Result.map ~f:(fun _ -> context)
  |> Result.map_error ~f:invalid
;;
