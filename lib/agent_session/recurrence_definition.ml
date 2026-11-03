open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module I = Recurrence_identity

module Schedule = struct
  type t =
    | Interval of
        { rule : Recurrence_rules.Rule.Interval.t
        ; first_due : Time_ns.Alternate_sexp.t
        }
    | Calendar of Recurrence_rules.Rule.Calendar.t
  [@@deriving sexp_of]

  let rule = function
    | Interval { rule; _ } -> Recurrence_rules.Rule.Interval rule
    | Calendar rule -> Recurrence_rules.Rule.Calendar rule
  ;;

  let to_json t =
    match t with
    | Interval { first_due; _ } ->
      `Object
        [ "kind", `String "interval"
        ; "rule", Recurrence_rules.Rule.to_json (rule t)
        ; "first_due", P.Timestamp.to_json (P.Timestamp.of_time_ns first_due)
        ]
    | Calendar _ ->
      `Object
        [ "kind", `String "calendar"; "rule", Recurrence_rules.Rule.to_json (rule t) ]
  ;;

  let limits =
    D.Limits.create ~max_bytes:65536 ~max_depth:24 ~max_fields:256 ~max_nodes:512
    |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
    |> Result.ok_or_failwith
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind () =
      D.Json.validate ~limits json
      |> Result.map_error ~f:(fun error ->
        P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
    in
    let%bind f = X.object_ json in
    let%bind kind = X.required f "kind" P.Json_codec.string in
    let%bind rule =
      X.required f "rule" (fun json ->
        Recurrence_rules.Rule.of_json json
        |> Result.map_error ~f:(fun error ->
          P.Error.invalid_request
            (Sexp.to_string_hum (Recurrence_rules.Timing_error.sexp_of_t error))))
    in
    match kind, rule with
    | "interval", Recurrence_rules.Rule.Interval rule ->
      let%map first_due = X.required f "first_due" P.Timestamp.of_json in
      Interval { rule; first_due = P.Timestamp.to_time_ns first_due }
    | "calendar", Recurrence_rules.Rule.Calendar rule -> Ok (Calendar rule)
    | _, (Recurrence_rules.Rule.Interval _ | Recurrence_rules.Rule.Calendar _) ->
      Error (P.Error.invalid_request "recurrence schedule and rule kind differ")
  ;;

  let shape =
    let v = D.Shape.value in
    X.tagged_shape_exn
      ~discriminator:"kind"
      [ "interval", X.fields_shape [ "kind"; "rule"; "first_due" ]
      ; "calendar", X.shape_exn [ "kind", v; "rule", v ]
      ]
  ;;
end

type projection =
  { key : I.Definition.t
  ; creator : P.Id.Principal.t
  ; source : P.Run_source.t
  ; birth_lifecycle_revision : P.Session_lifecycle.Revision.t
  ; schedule : Schedule.t
  ; target : Recurrence_target.t
  ; policy : Recurrence_policy.t
  ; created_at : P.Timestamp.t
  }

type t =
  { projection : projection
  ; document : D.Document.t
  ; sha256 : string
  }

let key t = t.projection.key
let creator t = t.projection.creator
let source t = t.projection.source
let birth_lifecycle_revision t = t.projection.birth_lifecycle_revision
let schedule t = t.projection.schedule
let target t = t.projection.target
let policy t = t.projection.policy
let created_at t = t.projection.created_at
let digest t = t.sha256
let to_document t = t.document

let initial_cursor t =
  match schedule t with
  | Interval { first_due; _ } -> Recurrence_cursor.create_interval (key t) ~first_due
  | Calendar rule -> Recurrence_cursor.create_calendar (key t) ~rule
;;

let limits =
  D.Limits.create
    ~max_bytes:(26 * 1024 * 1024)
    ~max_depth:160
    ~max_fields:32768
    ~max_nodes:65536
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let validate (t : projection) =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Principal.of_json (P.Id.Principal.to_json t.creator) in
  let%bind () = P.Run_source.validate t.source in
  let%bind _ =
    P.Session_lifecycle.Revision.of_int64
      (P.Session_lifecycle.Revision.to_int64 t.birth_lifecycle_revision)
  in
  let%bind _ = P.Timestamp.of_json (P.Timestamp.to_json t.created_at) in
  let%bind _ = Schedule.of_json (Schedule.to_json t.schedule) in
  let%bind _ = Recurrence_policy.of_json (Recurrence_policy.to_json t.policy) in
  let%map _ = Recurrence_target.to_document t.target |> protocol in
  ()
;;

let encode (t : projection) =
  let%map.Result target = Recurrence_target.to_document t.target |> protocol in
  `Object
    [ "key", I.Definition.to_json t.key
    ; "creator", P.Id.Principal.to_json t.creator
    ; "source", P.Run_source.to_json t.source
    ; ( "birth_lifecycle_revision"
      , X.int64_json (P.Session_lifecycle.Revision.to_int64 t.birth_lifecycle_revision) )
    ; "schedule", Schedule.to_json t.schedule
    ; "target", D.Document.json target
    ; "policy", Recurrence_policy.to_json t.policy
    ; "created_at", P.Timestamp.to_json t.created_at
    ]
;;

let decode json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind key = X.required f "key" I.Definition.of_json in
  let%bind creator = X.required f "creator" P.Id.Principal.of_json in
  let%bind source = X.required f "source" P.Run_source.of_json in
  let%bind birth_lifecycle_revision =
    X.required f "birth_lifecycle_revision" (fun json ->
      let%bind n =
        match json with
        | `String text when String.length text <= 19 -> X.nonnegative_int64 (`String text)
        | _ -> Error (P.Error.invalid_request "invalid recurrence birth revision")
      in
      P.Session_lifecycle.Revision.of_int64 n)
  in
  let%bind schedule = X.required f "schedule" Schedule.of_json in
  let%bind target =
    X.required f "target" (fun json ->
      let%bind document =
        D.Document.inspect ~limits:Recurrence_target.limits json |> protocol
      in
      Recurrence_target.of_document document |> protocol)
  in
  let%bind policy = X.required f "policy" Recurrence_policy.of_json in
  let%bind created_at = X.required f "created_at" P.Timestamp.of_json in
  let t =
    { key
    ; creator
    ; source
    ; birth_lifecycle_revision
    ; schedule
    ; target
    ; policy
    ; created_at
    }
  in
  let%map () = validate t in
  t
;;

let shape =
  let v = D.Shape.value in
  X.shape_exn
    [ "key", I.Definition.shape
    ; "creator", v
    ; "source", Run_record_shapes.source
    ; "birth_lifecycle_revision", v
    ; "schedule", Schedule.shape (* Target owns its complete nested universal carrier. *)
    ; "target", v
    ; "policy", Recurrence_policy.shape
    ; "created_at", v
    ]
;;

let codec =
  D.Domain_codec.create_validated
    ~limits
    ~kind:"ochat.recurrence_definition"
    ~version:1
    ~shape
    ~supported_semantics:[]
    ~validate:(fun t -> X.document_result (validate t))
    ~decode:(fun json -> X.document_result (decode json))
    ~encode:(fun t -> X.document_result (encode t))
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let admitted projection document =
  let%map.Result canonical =
    P.Json_codec.canonical_string (D.Document.json document) |> X.document_result
  in
  { projection; document; sha256 = Digestif.SHA256.(digest_string canonical |> to_hex) }
;;

let of_document document =
  let open Result.Let_syntax in
  let%bind carrier = D.Domain_codec.decode codec document in
  admitted (C.value carrier) document
;;

let create
      ~key
      ~creator
      ~source
      ~birth_lifecycle_revision
      ~schedule
      ~target
      ~policy
      ~created_at
  =
  let open Result.Let_syntax in
  let projection =
    { key
    ; creator
    ; source
    ; birth_lifecycle_revision
    ; schedule
    ; target
    ; policy
    ; created_at
    }
  in
  let%bind document =
    D.Domain_codec.encode codec (C.of_authored_value projection) |> protocol
  in
  admitted projection document |> protocol
;;
