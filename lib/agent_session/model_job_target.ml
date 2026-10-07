open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec

type t =
  { job_id : P.Id.Job.t
  ; generation : int
  ; source : (Inference.Selection.t[@sexp.opaque])
  ; execution : (Inference.Selection.t[@sexp.opaque])
  }
[@@deriving sexp]

let inference_error error =
  P.Error.invalid_request (Sexp.to_string_hum (Inference.Request.Error.sexp_of_t error))
;;

let of_json json ~limits =
  let open Result.Let_syntax in
  let%bind () =
    D.Json.validate ~limits json
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
  in
  let%bind fields = X.object_ json in
  let%bind job_id = X.required fields "job_id" P.Id.Job.of_json in
  let%bind generation = X.required fields "generation" X.host_counter_of_json in
  let%bind source =
    X.required fields "source" (fun json ->
      Inference.Selection.of_json json ~limits |> Result.map_error ~f:inference_error)
  in
  let%bind execution =
    X.required fields "execution" (fun json ->
      Inference.Selection.of_json json ~limits |> Result.map_error ~f:inference_error)
  in
  let%map () =
    match Inference.Selection.view source, Inference.Selection.view execution with
    | Unresolved, Captured _ ->
      Error
        (P.Error.invalid_request "recipe capture requires a captured model job source")
    | Unresolved, Unresolved | Captured _, (Unresolved | Captured _) -> Ok ()
  in
  { job_id; generation; source; execution }
;;

let create (job : P.Job.t) ~target ~limits =
  let open Result.Let_syntax in
  let%bind () =
    match job.kind with
    | Model_call -> Ok ()
    | Nested_agent | Scheduled_event | Async_tool | Shell_process | Compaction ->
      Error (P.Error.invalid_request "only a model job may capture an inference target")
  in
  let%bind selection =
    Inference.Selection.captured target ~limits |> Result.map_error ~f:inference_error
  in
  let%bind execution =
    Inference.Selection.unresolved ~limits |> Result.map_error ~f:inference_error
  in
  of_json
    (`Object
        [ "job_id", P.Id.Job.to_json job.id
        ; "generation", X.host_counter_to_json job.generation
        ; "source", Inference.Selection.to_json selection
        ; "execution", Inference.Selection.to_json execution
        ])
    ~limits
;;

let job_id t = t.job_id
let generation t = t.generation
let source t = t.source
let execution t = t.execution

let to_json t =
  `Object
    [ "job_id", P.Id.Job.to_json t.job_id
    ; "generation", X.host_counter_to_json t.generation
    ; "source", Inference.Selection.to_json t.source
    ; "execution", Inference.Selection.to_json t.execution
    ]
;;

let shape =
  X.shape_exn
    [ "job_id", D.Shape.value
    ; "generation", D.Shape.value
    ; "source", D.Shape.value
    ; "execution", D.Shape.value
    ]
;;

let capture_source t ~target ~limits =
  let open Result.Let_syntax in
  let%bind _ = of_json (to_json t) ~limits in
  let%map source =
    Inference.Selection.capture t.source ~target ~limits
    |> Result.map_error ~f:inference_error
  in
  { t with source }
;;

let capture_recipe t ~target ~limits =
  let open Result.Let_syntax in
  let%bind _ = of_json (to_json t) ~limits in
  let%bind () =
    match Inference.Selection.view t.source with
    | Captured _ -> Ok ()
    | Unresolved ->
      Error (P.Error.invalid_request "model job source requires explicit migration")
  in
  let%map execution =
    Inference.Selection.capture t.execution ~target ~limits
    |> Result.map_error ~f:inference_error
  in
  { t with execution }
;;
