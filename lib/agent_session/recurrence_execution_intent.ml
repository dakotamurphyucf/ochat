open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module F = Recurrence_definition
module I = Recurrence_identity

module Child = struct
  type t =
    { reservation : Recurrence_child_reservation.t
    ; association : Recurrence_child_association.t
    }

  let create ~reservation ~association =
    let%map.Result () = Recurrence_child_association.validate association ~reservation in
    { reservation; association }
  ;;
end

type projection =
  { nominal : Recurrence_nominal.t
  ; nominal_json : Jsonaf.t
  ; definition : I.Definition.t
  ; definition_sha256 : string
  ; control_revision : int64
  ; controller_source : P.Run_source.t
  ; birth : P.Session_lifecycle.Revision.t
  ; target : P.Session_ref.t
  ; source : P.Run_source.t
  ; mode : P.Run.Mode.t
  ; input : P.Run_start.Input.t
  ; input_json : Jsonaf.t
  ; child : Recurrence_child_association.t option
  }

type t =
  { projection : projection
  ; json : Jsonaf.t
  ; sha256 : string
  }

let occurrence t = Recurrence_nominal.key t.projection.nominal
let nominal t = t.projection.nominal
let nominal_json t = t.projection.nominal_json
let control_revision t = t.projection.control_revision
let controller_source t = t.projection.controller_source
let birth_lifecycle_revision t = t.projection.birth
let target t = t.projection.target
let source t = t.projection.source
let mode t = t.projection.mode
let input t = t.projection.input
let input_json t = t.projection.input_json
let child t = t.projection.child
let digest t = t.sha256
let to_json t = t.json
let equal a b = D.Json.equal a.json b.json
let invalid message = Error (P.Error.invalid_request message)

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create ~max_bytes:(384 * 1024) ~max_depth:40 ~max_fields:2048 ~max_nodes:4096
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let input_limits =
  (* Preserve the target's complete 256 KiB input profile inside the two-field
     User_submission wrapper: fewer than 64 bytes, one level and two nodes. *)
  D.Limits.create ~max_bytes:(262144 + 64) ~max_depth:33 ~max_fields:1026 ~max_nodes:2050
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let digest_of_json json =
  let%bind.Result text = P.Json_codec.string json in
  if
    String.length text = 64
    && String.for_all text ~f:(function
      | '0' .. '9' | 'a' .. 'f' -> true
      | _ -> false)
  then Ok text
  else invalid "invalid recurrence execution digest"
;;

let decode json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind nominal_json = X.required fields "nominal" X.raw in
  let%bind nominal = Recurrence_nominal.of_json nominal_json in
  let%bind definition = X.required fields "definition" I.Definition.of_json in
  let%bind definition_sha256 = X.required fields "definition_sha256" digest_of_json in
  let%bind control_revision = X.required fields "control_revision" X.nonnegative_int64 in
  let%bind controller_source =
    X.required fields "controller_source" P.Run_source.of_json
  in
  let%bind birth =
    X.required fields "birth_lifecycle_revision" (fun json ->
      let%bind revision = X.nonnegative_int64 json in
      P.Session_lifecycle.Revision.of_int64 revision)
  in
  let%bind target = X.required fields "target" P.Session_ref.of_json in
  let%bind source = X.required fields "source" P.Run_source.of_json in
  let%bind mode = X.required fields "mode" P.Run.Mode.of_json in
  let%bind input_json = X.required fields "input" X.raw in
  let%bind () = D.Json.validate ~limits:input_limits input_json |> protocol in
  let%bind input = P.Run_start.Input.of_json input_json in
  let%bind child =
    X.required fields "child" (X.nullable Recurrence_child_association.of_json)
  in
  if
    I.Definition.equal
      definition
      (I.Occurrence.definition (Recurrence_nominal.key nominal))
    && String.equal definition_sha256 (Recurrence_nominal.definition_sha256 nominal)
    && Option.for_all child ~f:(fun child ->
      I.Occurrence.equal
        (Recurrence_child_association.occurrence child)
        (Recurrence_nominal.key nominal))
  then
    Ok
      { nominal
      ; nominal_json
      ; definition
      ; definition_sha256
      ; control_revision
      ; controller_source
      ; birth
      ; target
      ; source
      ; mode
      ; input
      ; input_json
      ; child
      }
  else invalid "recurrence execution intent identity differs from its nominal"
;;

let encode p =
  `Object
    [ "nominal", p.nominal_json
    ; "definition", I.Definition.to_json p.definition
    ; "definition_sha256", `String p.definition_sha256
    ; "control_revision", X.int64_json p.control_revision
    ; "controller_source", P.Run_source.to_json p.controller_source
    ; ( "birth_lifecycle_revision"
      , X.int64_json (P.Session_lifecycle.Revision.to_int64 p.birth) )
    ; "target", P.Session_ref.to_json p.target
    ; "source", P.Run_source.to_json p.source
    ; "mode", P.Run.Mode.to_json p.mode
    ; "input", p.input_json
    ; "child", X.option_json Recurrence_child_association.to_json p.child
    ]
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.recurrence_execution_intent"
    ~shape:
      (X.fields_shape
         [ "nominal"
         ; "definition"
         ; "definition_sha256"
         ; "control_revision"
         ; "controller_source"
         ; "birth_lifecycle_revision"
         ; "target"
         ; "source"
         ; "mode"
         ; "input"
         ; "child"
         ])
    ~decode
    ~encode:(fun value -> Ok (encode value))
;;

let of_json json =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect ~limits json |> protocol in
  let%bind carrier = D.Domain_codec.decode codec document |> protocol in
  let%map canonical = P.Json_codec.canonical_string json in
  { projection = D.Extension_carrier.value carrier
  ; json
  ; sha256 = Digestif.SHA256.(digest_string canonical |> to_hex)
  }
;;

let request t ~expected_revision ~key =
  Run_admission_request.Headless.create
    ~session_id:(P.Session_ref.session_id (target t))
    ~generation:(source t).generation
    ~expected_revision
    ~mode:(mode t)
    ~input:(input t)
    ~key
;;

let existing_input_json target =
  let open Result.Let_syntax in
  let%bind document = Recurrence_target.to_document target |> protocol in
  match D.Json.field (D.Document.payload document) ~name:"input" with
  | Value content ->
    Ok (`Object [ "kind", `String "user_submission"; "content", content ])
  | Absent | Null -> invalid "existing recurrence input is unavailable"
;;

let validate_target t ~definition ~reservation =
  let open Result.Let_syntax in
  let captured = F.target definition in
  let%bind selected_mode = Recurrence_target.run_mode captured in
  if not (P.Run.Mode.equal selected_mode (mode t))
  then invalid "execution mode differs from immutable target selection"
  else (
    match Recurrence_target.view captured, child t, reservation, input t with
    | ( Existing_session
          { session; source = selected_source; input = _; configuration_revision = _ }
      , None
      , None
      , User_submission _ ) ->
      let%bind expected_input = existing_input_json captured in
      if
        P.Session_ref.equal session (target t)
        && P.Run_source.equal selected_source (source t)
        && D.Json.equal expected_input (input_json t)
      then Ok ()
      else invalid "execution intent differs from existing target/input/source"
    | Fresh_child _, Some association, Some reservation, Authored_start ->
      let%bind () = Recurrence_child_reservation.validate reservation ~definition in
      let%bind () = Recurrence_child_association.validate association ~reservation in
      let reference = Recurrence_child_association.reference association in
      if
        P.Id.Session.equal
          reference.child_session_id
          (P.Session_ref.session_id (target t))
        && P.Id.Server.equal
             (P.Session_ref.server_id (target t))
             (P.Session_ref.server_id (I.Definition.controller (F.key definition)))
        && Int64.equal
             (control_revision t)
             (Recurrence_child_reservation.control_revision reservation)
      then Ok ()
      else invalid "execution intent differs from captured child reservation"
    | Existing_session _, _, _, _ | Fresh_child _, _, _, _ ->
      invalid "execution input or child custody differs from target kind")
;;

let validate t ~definition ~reservation =
  let open Result.Let_syntax in
  let%bind () = Recurrence_nominal.validate (nominal t) ~definition in
  if
    I.Definition.equal t.projection.definition (F.key definition)
    && String.equal t.projection.definition_sha256 (F.digest definition)
    && P.Run_source.equal (controller_source t) (F.source definition)
    && P.Session_lifecycle.Revision.equal
         (birth_lifecycle_revision t)
         (F.birth_lifecycle_revision definition)
  then validate_target t ~definition ~reservation
  else invalid "execution intent differs from immutable controller definition"
;;

let create ~definition ~control_revision ~nominal_json ~target ~source ~child =
  let open Result.Let_syntax in
  let%bind nominal = Recurrence_nominal.of_json nominal_json in
  let%bind _ = P.Session_ref.of_json (P.Session_ref.to_json target) in
  let%bind () = P.Run_source.validate source in
  let%bind () =
    if Int64.(control_revision >= 0L)
    then Ok ()
    else invalid "negative execution control revision"
  in
  let%bind mode = Recurrence_target.run_mode (F.target definition) in
  let%bind input_json =
    match Recurrence_target.view (F.target definition) with
    | Existing_session _ -> existing_input_json (F.target definition)
    | Fresh_child _ -> Ok (P.Run_start.Input.to_json Authored_start)
  in
  let%bind () = D.Json.validate ~limits:input_limits input_json |> protocol in
  let%bind input = P.Run_start.Input.of_json input_json in
  let projection =
    { nominal
    ; nominal_json
    ; definition = F.key definition
    ; definition_sha256 = F.digest definition
    ; control_revision
    ; controller_source = F.source definition
    ; birth = F.birth_lifecycle_revision definition
    ; target
    ; source
    ; mode
    ; input
    ; input_json
    ; child = Option.map child ~f:(fun child -> child.Child.association)
    }
  in
  let%bind document =
    D.Domain_codec.encode codec (D.Extension_carrier.of_authored_value projection)
    |> protocol
  in
  let%bind t = of_json (D.Document.json document) in
  let%map () =
    validate
      t
      ~definition
      ~reservation:(Option.map child ~f:(fun child -> child.Child.reservation))
  in
  t
;;
