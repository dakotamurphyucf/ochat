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

module D = Document_schema
module C = D.Extension_carrier
module S = Session.Moderator_state.Identity_snapshot

let domain result =
  Result.map_error result ~f:(fun error ->
    failed (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let checkpoint_field = function
  | `Object [ ("checkpoint", `Null) ] -> Ok None
  | `Object [ ("checkpoint", checkpoint) ] -> Ok (Some checkpoint)
  | _ -> Error (D.Error.Invalid_field { path = []; reason = "checkpoint wrapper" })
;;

let validate_optional checkpoint =
  let open Result.Let_syntax in
  let%bind snapshot = decode checkpoint in
  let%bind () = validate_encoded_capacity checkpoint in
  let%map () =
    Option.value_map snapshot ~default:(Ok ()) ~f:(fun snapshot ->
      S.validate snapshot |> Result.map_error ~f:failed)
  in
  snapshot
;;

let wrapper checkpoint = `Object [ "checkpoint", Option.value checkpoint ~default:`Null ]

let invariant_exn = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "checkpoint component invariant", (error : Agent_protocol.Error.t)]
;;

let component_kind = "session.moderator-checkpoint-component"

let component_limits =
  (* Measure the actual internal envelope, including its nullable checkpoint
     member. Only the raw child's original profile is externally admissible. *)
  let raw = `Null in
  let document =
    D.Document.create
      ~limits:D.Limits.default
      ~kind:component_kind
      ~version:1
      ~payload:(wrapper None)
    |> domain
    |> invariant_exn
  in
  let measure json =
    Capacity_growth.measure json ~limits:D.Limits.default |> invariant_exn
  in
  let overhead =
    Capacity_growth.positive_difference
      (measure (D.Document.json document))
      ~previous:(measure raw)
  in
  D.Limits.create
    ~max_bytes:
      (D.Limits.max_bytes D.Limits.default + Capacity_growth.encoded_bytes overhead)
    ~max_depth:(D.Limits.max_depth D.Limits.default + 2)
    ~max_fields:(D.Limits.max_fields D.Limits.default + Capacity_growth.fields overhead)
    ~max_nodes:(D.Limits.max_nodes D.Limits.default + Capacity_growth.nodes overhead)
  |> function
  | Ok limits -> limits
  | Error error -> raise_s [%sexp "checkpoint component limits", (error : D.Error.t)]
;;

let component_codec =
  let component_shape =
    D.Shape.object_ [ "checkpoint", D.Shape.nullable shape ]
    |> function
    | Ok shape -> shape
    | Error error -> raise_s [%sexp (error : D.Error.t)]
  in
  D.Domain_codec.create
    ~limits:component_limits
    ~kind:component_kind
    ~version:1
    ~shape:component_shape
    ~supported_semantics:[]
    ~decode:(fun json ->
      let open Result.Let_syntax in
      let%bind checkpoint = checkpoint_field json in
      let%map _ =
        validate_optional checkpoint
        |> Result.map_error ~f:(fun error ->
          D.Error.Invalid_field { path = []; reason = error.Agent_protocol.Error.message })
      in
      json)
    ~encode:Result.return
  |> function
  | Ok codec -> codec
  | Error error -> raise_s [%sexp "checkpoint component codec", (error : D.Error.t)]
;;

let carrier checkpoint =
  let open Result.Let_syntax in
  let%bind _ = validate_optional checkpoint in
  let%bind document =
    D.Document.create
      ~limits:component_limits
      ~kind:component_kind
      ~version:1
      ~payload:(wrapper checkpoint)
    |> domain
  in
  D.Domain_codec.decode component_codec document |> domain
;;

let encoded carrier =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode component_codec carrier |> domain in
  let%bind checkpoint = checkpoint_field (D.Document.payload document) |> domain in
  let%map _ = validate_optional checkpoint in
  checkpoint
;;

type t =
  { raw : Jsonaf.t
  ; identity : S.t
  ; carrier : Jsonaf.t C.t
  }

let of_jsonaf raw =
  let open Result.Let_syntax in
  let%bind carrier = carrier (Some raw) in
  let%map identity =
    decode (Some raw)
    |> Result.bind ~f:(fun identity ->
      Result.of_option identity ~error:(failed "checkpoint identity is absent"))
  in
  { raw; identity; carrier }
;;

let to_jsonaf t = t.raw
let snapshot t = t.identity

module Queue_update = struct
  type t =
    | Keep
    | Replace of Moderator_queue.t
end

let known carrier = checkpoint_field (C.value carrier) |> domain

let has_extensions carrier raw =
  let%map.Result known = known carrier in
  not (Option.equal Jsonaf.exactly_equal known raw)
;;

let check_source ~previous ~incoming ~extended =
  let open Result.Let_syntax in
  if not extended
  then Ok ()
  else (
    let%bind before = observer previous in
    let%bind after = observer incoming in
    let%bind before_queue = queue previous in
    let%bind after_queue = queue incoming in
    let queue_source_continues =
      match before_queue, after_queue with
      | None, (None | Some _) -> true
      | Some previous, Some incoming ->
        Agent_protocol.Run_source.equal
          (Moderator_queue.source previous)
          (Moderator_queue.source incoming)
      | Some _, None -> false
    in
    if
      Option.equal Agent_protocol.Invocation.equal_observer before after
      && queue_source_continues
    then Ok ()
    else Error (failed "checkpoint extensions cannot move to a different source lifetime"))
;;

let replace_field fields name value =
  if List.Assoc.mem fields name ~equal:String.equal
  then
    List.map fields ~f:(fun (key, previous) ->
      key, if String.equal key name then value else previous)
  else fields @ [ name, value ]
;;

let update t ~snapshot ~queue:queue_update =
  let open Result.Let_syntax in
  let%bind projected = known t.carrier in
  let%bind fields =
    match projected with
    | Some (`Object fields) -> Ok fields
    | None | Some _ -> Error (failed "checkpoint identity object is absent")
  in
  let fields = replace_field fields "identity_snapshot" (S.to_jsonaf snapshot) in
  let fields =
    match queue_update with
    | Queue_update.Keep -> fields
    | Replace queue ->
      replace_field fields "queue_provenance" (Moderator_queue.to_jsonaf queue)
  in
  let incoming = Some (`Object fields) in
  let%bind extended = has_extensions t.carrier (Some t.raw) in
  let%bind () = check_source ~previous:(Some t.raw) ~incoming ~extended in
  (* Incoming queue data is captured by this same codec before adoption. It
     never becomes unchecked known JSON or a second preservation owner. *)
  let%bind incoming = carrier incoming in
  let%bind adopted =
    D.Domain_codec.adopt component_codec ~previous:t.carrier ~incoming |> domain
  in
  let%bind incoming = encoded adopted in
  match incoming with
  | Some raw -> of_jsonaf raw
  | None -> Error (failed "checkpoint update removed its identity")
;;

let update_with_queue previous ~snapshot ~queue =
  let open Result.Let_syntax in
  match previous with
  | None ->
    let%bind raw = encode_with_queue snapshot ~queue in
    let%map admitted = of_jsonaf raw in
    to_jsonaf admitted
  | Some raw ->
    let%bind previous = of_jsonaf raw in
    let%map incoming = update previous ~snapshot ~queue:(Queue_update.Replace queue) in
    to_jsonaf incoming
;;

let validate_nonempty_update ~previous incoming =
  let open Result.Let_syntax in
  let%bind old = carrier previous in
  let%bind next = carrier incoming in
  let%bind adopted =
    D.Domain_codec.adopt component_codec ~previous:old ~incoming:next |> domain
  in
  let%bind extended = has_extensions old previous in
  let%bind () = check_source ~previous ~incoming ~extended in
  let%bind expected = encoded adopted in
  if Option.equal Jsonaf.exactly_equal expected incoming
  then Ok ()
  else Error (failed "checkpoint update discarded or reordered compatible raw fields")
;;

let validate_update ~previous incoming =
  match previous, incoming with
  | None, None -> Ok ()
  | Some _, (None | Some _) | None, Some _ -> validate_nonempty_update ~previous incoming
;;
