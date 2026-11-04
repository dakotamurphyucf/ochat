open Core

let failed message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let shape =
  match
    Document_schema.Shape.object_
      [ "identity_snapshot", Session.Moderator_state.Identity_snapshot.shape
      ; "queue_provenance", Document_schema.Shape.nullable Moderator_queue.shape
      ]
  with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let digest snapshot =
  Session.Moderator_state.Identity_snapshot.to_jsonaf snapshot
  |> Jsonaf.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let encode snapshot =
  `Object
    [ "identity_snapshot", Session.Moderator_state.Identity_snapshot.to_jsonaf snapshot ]
;;

let queue = function
  | None -> Ok None
  | Some json ->
    (match Document_schema.Json.field json ~name:"queue_provenance" with
     | Absent | Null -> Ok None
     | Value json -> Result.map (Moderator_queue.of_jsonaf json) ~f:Option.some)
;;

let validate_queue snapshot metadata =
  let source = Moderator_queue.source metadata in
  if
    Agent_protocol.Invocation.equal_observer
      source.observer
      { script_id = snapshot.Session.Moderator_state.Identity_snapshot.script_id
      ; source_sha256 = snapshot.script_source_hash
      }
  then Moderator_queue.validate_queue metadata snapshot.queued_internal_events
  else Error (failed "queued occurrence source differs from moderator checkpoint")
;;

let encode_with_queue snapshot ~queue =
  let%map.Result () = validate_queue snapshot queue in
  `Object
    [ "identity_snapshot", Session.Moderator_state.Identity_snapshot.to_jsonaf snapshot
    ; "queue_provenance", Moderator_queue.to_jsonaf queue
    ]
;;

let decode = function
  | None -> Ok None
  | Some json ->
    let open Result.Let_syntax in
    let%bind () =
      Document_schema.Json.validate ~limits:Document_schema.Limits.default json
      |> Result.map_error ~f:(fun error ->
        failed (Sexp.to_string_hum ([%sexp_of: Document_schema.Error.t] error)))
    in
    (match Document_schema.Json.field json ~name:"identity_snapshot" with
     | Value snapshot ->
       let%bind snapshot =
         Session.Moderator_state.Identity_snapshot.of_jsonaf snapshot
         |> Result.map_error ~f:(fun error ->
           failed ("moderator snapshot decode failed: " ^ error))
       in
       let%bind metadata = queue (Some json) in
       let%map () =
         Option.value_map metadata ~default:(Ok ()) ~f:(validate_queue snapshot)
       in
       Some snapshot
     | Absent | Null -> Error (failed "moderator snapshot is missing identity state"))
;;

let observer snapshot =
  Result.map
    (decode snapshot)
    ~f:
      (Option.map ~f:(fun snapshot ->
         Agent_protocol.Invocation.
           { script_id = snapshot.Session.Moderator_state.Identity_snapshot.script_id
           ; source_sha256 = snapshot.script_source_hash
           }))
;;

let is_halted snapshot =
  Result.map
    (decode snapshot)
    ~f:
      (Option.value_map ~default:false ~f:(fun snapshot ->
         snapshot.Session.Moderator_state.Identity_snapshot.halted))
;;

let validate_encoded_capacity = function
  | None -> Ok ()
  | Some json ->
    (match Document_schema.Json.field json ~name:"queue_provenance" with
     | Absent | Null -> Ok ()
     | Value raw -> Moderator_queue.validate_encoded_capacity raw)
;;
