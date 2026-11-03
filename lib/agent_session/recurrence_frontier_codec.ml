open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module I = Recurrence_identity
module V = Recurrence_frontier_records
module S = V.State

let invalid message = Error (P.Error.invalid_request message)

let limits =
  D.Limits.create
    ~max_bytes:(2 * 1024 * 1024)
    ~max_depth:48
    ~max_fields:32768
    ~max_nodes:65536
  |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;

let time_json t = P.Timestamp.to_json (P.Timestamp.of_time_ns t)
let time_of_json j = Result.map (P.Timestamp.of_json j) ~f:P.Timestamp.to_time_ns
let nullable = X.nullable
let optional = X.option_json
let stored = V.Stored_nominal.json
let stored_of_json = V.Stored_nominal.of_json

let list maximum decode = function
  | `Array rows when List.length rows <= maximum -> List.map rows ~f:decode |> Result.all
  | _ -> invalid "frontier retained array exceeds bound"
;;

let mark_json (m : V.Mark.t) =
  `Object
    [ "revision", X.int64_json (I.Definition.revision m.definition)
    ; "definition", I.Definition.to_json m.definition
    ; "definition_sha256", `String m.definition_sha256
    ; "through", optional X.int64_json m.through
    ; "last_at", optional time_json m.last_at
    ; "omitted", X.int64_json m.omitted
    ; "withdrawn", X.int64_json m.withdrawn
    ; "last_alarm_sequence", optional X.int64_json m.last_alarm_sequence
    ]
;;

let mark_of_json json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind revision = X.required f "revision" X.nonnegative_int64 in
  let%bind definition = X.required f "definition" I.Definition.of_json in
  let%bind definition_sha256 = X.required f "definition_sha256" P.Json_codec.string in
  let%bind through = X.required f "through" (nullable X.nonnegative_int64) in
  let%bind last_at = X.required f "last_at" (nullable time_of_json) in
  let%bind omitted = X.required f "omitted" X.nonnegative_int64 in
  let%bind withdrawn = X.required f "withdrawn" X.nonnegative_int64 in
  let%bind last_alarm_sequence =
    X.required f "last_alarm_sequence" (nullable X.nonnegative_int64)
  in
  if not (Int64.equal revision (I.Definition.revision definition))
  then invalid "frontier mark identity differs from definition"
  else
    Ok
      V.Mark.
        { definition
        ; definition_sha256
        ; through
        ; last_at
        ; omitted
        ; withdrawn
        ; last_alarm_sequence
        }
;;

let detail_json (r : V.Detail.t) =
  match r.original_json with
  | Some json -> json
  | None ->
    `Object
      [ "id", `String (V.Detail.identity r)
      ; "definition", I.Definition.to_json r.definition
      ; "first_slot", X.int64_json r.first_slot
      ; "last_slot", X.int64_json r.last_slot
      ; ( "kind"
        , `String
            (match r.kind with
             | Skipped -> "skipped"
             | Withdrawn -> "withdrawn") )
      ; "reason", `String (V.Reason.to_string r.reason)
      ; "nominal", optional stored r.nominal
      ]
;;

let detail_of_json json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind id = X.required f "id" P.Json_codec.string in
  let%bind definition = X.required f "definition" I.Definition.of_json in
  let%bind first_slot = X.required f "first_slot" X.nonnegative_int64 in
  let%bind last_slot = X.required f "last_slot" X.nonnegative_int64 in
  let%bind kind =
    X.required
      f
      "kind"
      (P.Json_codec.enum
         ~name:"frontier history"
         [ "skipped", V.Detail.Skipped; "withdrawn", V.Detail.Withdrawn ])
  in
  let%bind reason = X.required f "reason" V.Reason.of_json in
  let%bind nominal = X.required f "nominal" (nullable stored_of_json) in
  let row =
    V.Detail.
      { definition
      ; first_slot
      ; last_slot
      ; kind
      ; reason
      ; nominal
      ; original_json = Some json
      }
  in
  if String.equal id (V.Detail.identity row)
  then Ok row
  else invalid "frontier history identity differs from range"
;;

let batch_owned_json (b : V.Batch.t) =
  `Object [ "cutoff", time_json b.cutoff; "candidates", X.list_json stored b.candidates ]
;;

let batch_json (b : V.Batch.t) =
  match b.original_json, batch_owned_json b with
  | None, owned -> owned
  | Some (`Object original), `Object owned ->
    `Object
      (List.map original ~f:(fun (name, json) ->
         name, Option.value (List.Assoc.find owned name ~equal:String.equal) ~default:json))
  | Some _, _ -> assert false
;;

let batch_has_extensions (b : V.Batch.t) =
  not (D.Json.equal (batch_json b) (batch_owned_json b))
;;

let batch_of_json json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind cutoff = X.required f "cutoff" time_of_json in
  let%map candidates = X.required f "candidates" (list 32 stored_of_json) in
  V.Batch.{ cutoff; candidates; original_json = Some json }
;;

let control_json (detail : V.Control_detail.t) =
  match detail.original_json with
  | Some json -> json
  | None ->
    `Object
      [ ( "kind"
        , `String
            (match detail.kind with
             | Retired_alarm -> "retired_alarm"
             | Retired_batch -> "retired_batch") )
      ; "value", detail.json
      ]
;;

let control_of_json original_json =
  let open Result.Let_syntax in
  let json = original_json in
  let%bind f = X.object_ json in
  let%bind kind =
    X.required
      f
      "kind"
      (P.Json_codec.enum
         ~name:"retained control metadata"
         [ "retired_alarm", V.Control_detail.Retired_alarm
         ; "retired_batch", Retired_batch
         ])
  in
  let%bind json = X.required f "value" X.raw in
  let%map () =
    match kind with
    | Retired_alarm -> Result.map (V.Stored_alarm.of_json json) ~f:(fun _ -> ())
    | Retired_batch -> Result.map (batch_of_json json) ~f:(fun _ -> ())
  in
  V.Control_detail.{ kind; json; original_json = Some original_json }
;;

let encode (s : S.t) =
  `Object
    [ "revision", X.int64_json s.revision
    ; "marks", X.list_json mark_json s.marks
    ; "ready", X.list_json stored s.ready
    ; ( "protected_children"
      , X.list_json Recurrence_child_reservation.to_json s.protected_children )
    ; "batch", optional batch_json s.batch
    ; "lookahead", optional stored s.lookahead
    ; "alarm", optional V.Stored_alarm.json s.alarm
    ; "issue", optional (fun i -> `String (V.Issue.to_string i)) s.issue
    ; "history", X.list_json detail_json s.history
    ; "control_history", X.list_json control_json s.control_history
    ]
;;

let validate (s : S.t) =
  let open Result.Let_syntax in
  let%bind () =
    if
      Int64.(
        s.revision < 0L
        || s.revision > max_value - of_int (List.length s.protected_children))
      || List.is_empty s.marks
      || List.length s.marks > 8
      || List.length s.ready > 32
      || List.length s.history > 256
      || List.length s.control_history > 256
      || Option.exists s.batch ~f:(fun b -> List.length b.candidates > 32)
    then invalid "frontier intrinsic bounds exceeded"
    else Ok ()
  in
  let%bind _ = V.State.summary s in
  let%bind () =
    List.fold_result s.marks ~init:0 ~f:(fun index (m : V.Mark.t) ->
      let%bind consumed = V.Mark.consumed m in
      let%bind omitted = V.add m.omitted m.withdrawn in
      if
        (not (Int64.equal (I.Definition.revision m.definition) (Int64.of_int index)))
        || (not
              (P.Session_ref.equal
                 (I.Definition.controller (List.hd_exn s.marks).definition)
                 (I.Definition.controller m.definition)))
        || (not
              (P.Id.Recurrence.equal
                 (I.Definition.recurrence_id (List.hd_exn s.marks).definition)
                 (I.Definition.recurrence_id m.definition)))
        || String.length m.definition_sha256 <> 64
        || (not
              (String.for_all m.definition_sha256 ~f:(function
                 | '0' .. '9' | 'a' .. 'f' -> true
                 | _ -> false)))
        || Int64.(omitted > consumed)
        || not (Bool.equal (Option.is_some m.through) (Option.is_some m.last_at))
      then invalid "frontier mark has invalid high-water or totals"
      else Ok (index + 1))
    |> Result.map ~f:(fun _ -> ())
  in
  let rows = s.ready @ Option.value_map s.batch ~default:[] ~f:(fun b -> b.candidates) in
  let keys =
    List.map rows ~f:(fun n -> Recurrence_nominal.key (V.Stored_nominal.value n))
    @ List.map s.protected_children ~f:Recurrence_child_reservation.occurrence
  in
  let%bind () =
    if List.length s.protected_children > 256
    then invalid "protected creation retention exhausted"
    else Ok ()
  in
  let%bind () =
    if List.contains_dup keys ~compare:I.Occurrence.compare
    then invalid "frontier contains duplicate pending occurrence"
    else Ok ()
  in
  let%bind () =
    List.fold_result s.history ~init:() ~f:(fun () (r : V.Detail.t) ->
      let%bind _ = V.Detail.count r in
      match r.kind, r.nominal with
      | Skipped, None -> Ok ()
      | (Withdrawn | Skipped), Some nominal ->
        let key = Recurrence_nominal.key (V.Stored_nominal.value nominal) in
        if
          I.Definition.equal r.definition (I.Occurrence.definition key)
          && Int64.equal r.first_slot r.last_slot
          && Int64.equal r.first_slot (I.Occurrence.slot key)
        then Ok ()
        else invalid "withdrawn nominal differs from retained range"
      | Withdrawn, None -> invalid "unsupported frontier retained evidence")
  in
  if List.contains_dup (List.map s.history ~f:V.Detail.identity) ~compare:String.compare
  then invalid "duplicate frontier retained range"
  else Ok ()
;;

let decode json =
  let open Result.Let_syntax in
  let%bind f = X.object_ json in
  let%bind revision = X.required f "revision" X.nonnegative_int64 in
  let%bind marks = X.required f "marks" (list 8 mark_of_json) in
  let%bind ready = X.required f "ready" (list 32 stored_of_json) in
  let%bind protected_children =
    match D.Json.field json ~name:"protected_children" with
    | Absent -> Ok []
    | Null -> invalid "protected children must be an array"
    | Value json -> list 256 Recurrence_child_reservation.of_json json
  in
  let%bind batch = X.required f "batch" (nullable batch_of_json) in
  let%bind lookahead = X.required f "lookahead" (nullable stored_of_json) in
  let%bind alarm = X.required f "alarm" (nullable V.Stored_alarm.of_json) in
  let%bind issue = X.required f "issue" (nullable V.Issue.of_json) in
  let%bind history = X.required f "history" (list 256 detail_of_json) in
  let%bind control_history = X.required f "control_history" (list 256 control_of_json) in
  let state =
    S.
      { revision
      ; marks
      ; ready
      ; protected_children
      ; batch
      ; lookahead
      ; alarm
      ; issue
      ; history
      ; control_history
      }
  in
  let%map () = validate state in
  state
;;

let shape =
  let v = D.Shape.value in
  X.shape_exn
    [ "revision", v
    ; ( "marks"
      , X.array_shape_exn
          ~identity_field:"revision"
          (X.fields_shape
             [ "revision"
             ; "definition"
             ; "definition_sha256"
             ; "through"
             ; "last_at"
             ; "omitted"
             ; "withdrawn"
             ; "last_alarm_sequence"
             ]) )
    ; "ready", v
    ; "protected_children", v
    ; "batch", v
    ; "lookahead", v
    ; "alarm", v
    ; "issue", v
    ; "history", v
    ; "control_history", v
    ]
;;

let codec =
  D.Domain_codec.create_validated
    ~limits
    ~kind:"ochat.recurrence_frontier"
    ~version:1
    ~shape
    ~supported_semantics:[ "recurrence.child-reservation.v1" ]
    ~validate:(fun s -> X.document_result (validate s))
    ~decode:(fun j -> X.document_result (decode j))
    ~encode:(fun s -> Ok (encode s))
  |> Result.map_error ~f:(fun e -> Sexp.to_string_hum (D.Error.sexp_of_t e))
  |> Result.ok_or_failwith
;;
