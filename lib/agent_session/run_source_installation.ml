open! Core
module P = Agent_protocol
module X = Persistence_codec

type t =
  { epoch : int64
  ; source : P.Invocation.observer option
  }
[@@deriving equal]

module Change = struct
  type t =
    | Checkpoint of P.Invocation.observer option
    | Replace of P.Invocation.observer
    | Remove
    | Reset of P.Invocation.observer option
  [@@deriving sexp_of]
end

let initial = { epoch = 0L; source = None }

let observer_to_json (source : P.Invocation.observer) =
  `Object
    [ "script_id", `String source.script_id
    ; "source_sha256", `String source.source_sha256
    ]
;;

let observer_of_json json =
  let open Result.Let_syntax in
  let%bind f = P.Json_codec.fields json in
  let%bind script_id = P.Json_codec.required_as f "script_id" P.Json_codec.string in
  let%bind source_sha256 =
    P.Json_codec.required_as f "source_sha256" P.Json_codec.string
  in
  let%map captured =
    P.Run_source.create
      ~observer:{ script_id; source_sha256 }
      ~generation:0
      ~installation_epoch:1L
  in
  captured.observer
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () =
    match t.source with
    | None -> Ok ()
    | Some source -> Result.map (observer_of_json (observer_to_json source)) ~f:ignore
  in
  if Int64.(t.epoch < 0L) || (Int64.equal t.epoch 0L && Option.is_some t.source)
  then Error (P.Error.invalid_request "invalid run source installation")
  else Ok ()
;;

let apply t ~(change : Change.t) =
  let open Result.Let_syntax in
  let%bind () = validate t in
  match change with
  | Checkpoint source ->
    if Option.equal P.Invocation.equal_observer t.source source
    then Ok t
    else
      Error
        (P.Error.create
           Conflict
           ~message:"checkpoint cannot replace source installation"
           ~retryable:false
           ())
  | Replace source ->
    if Int64.equal t.epoch Int64.max_value
    then
      Error
        (P.Error.create
           Resource_limit
           ~message:"run source epoch exhausted"
           ~retryable:false
           ())
    else (
      let next = { epoch = Int64.succ t.epoch; source = Some source } in
      let%map () = validate next in
      next)
  | Remove | Reset None ->
    if Int64.equal t.epoch Int64.max_value
    then
      Error
        (P.Error.create
           Resource_limit
           ~message:"run source epoch exhausted"
           ~retryable:false
           ())
    else Ok { epoch = Int64.succ t.epoch; source = None }
  | Reset (Some source) ->
    if Int64.equal t.epoch Int64.max_value
    then
      Error
        (P.Error.create
           Resource_limit
           ~message:"run source epoch exhausted"
           ~retryable:false
           ())
    else (
      let next = { epoch = Int64.succ t.epoch; source = Some source } in
      let%map () = validate next in
      next)
;;

let captured t ~generation =
  match t.source with
  | None ->
    Error
      (P.Error.create
         Invalid_state
         ~message:"run source is not installed"
         ~retryable:false
         ())
  | Some observer -> P.Run_source.create ~observer ~generation ~installation_epoch:t.epoch
;;

let to_jsonaf t =
  `Object
    [ "epoch", `String (Int64.to_string t.epoch)
    ; ( "source"
      , match t.source with
        | None -> `Null
        | Some source -> observer_to_json source )
    ]
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind f = P.Json_codec.fields json in
  let%bind epoch =
    P.Json_codec.required_as f "epoch" P.History.Content_revision.of_json
  in
  let%bind source =
    P.Json_codec.required_as f "source" (function
      | `Null -> Ok None
      | json -> Result.map (observer_of_json json) ~f:Option.some)
  in
  let t = { epoch = P.History.Content_revision.to_int64 epoch; source } in
  let%map () = validate t in
  t
;;

let sexp_of_t t = Jsonaf.sexp_of_t (to_jsonaf t)

let t_of_sexp sexp =
  match of_jsonaf (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error e -> Sexplib.Conv.of_sexp_error e.message sexp
;;

let shape =
  X.shape_exn
    [ "epoch", Document_schema.Shape.value
    ; ( "source"
      , Document_schema.Shape.nullable (X.fields_shape [ "script_id"; "source_sha256" ]) )
    ]
;;
