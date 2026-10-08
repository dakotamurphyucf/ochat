open Core

let failed message =
  Agent_protocol.Error.create Invalid_state ~message ~retryable:false ()
;;

let shape =
  match
    Document_schema.Shape.object_
      [ "identity_snapshot", Session.Moderator_state.Identity_snapshot.shape ]
  with
  | Ok shape -> shape
  | Error error -> raise_s [%sexp (error : Document_schema.Error.t)]
;;

let encode snapshot =
  `Object
    [ "identity_snapshot", Session.Moderator_state.Identity_snapshot.to_jsonaf snapshot ]
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
       Session.Moderator_state.Identity_snapshot.of_jsonaf snapshot
       |> Result.map ~f:Option.some
       |> Result.map_error ~f:(fun error ->
         failed ("moderator snapshot decode failed: " ^ error))
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
