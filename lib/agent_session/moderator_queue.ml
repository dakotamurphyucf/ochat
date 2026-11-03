open! Core
module P = Agent_protocol
module J = P.Json_codec
module D = Document_schema

let invalid message = Error (P.Error.invalid_request message)
let max_rows = 256

let limits =
  D.Limits.create ~max_bytes:(1024 * 1024) ~max_depth:32 ~max_fields:8192 ~max_nodes:16384
  |> function
  | Ok t -> t
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let bounded json =
  D.Json.validate ~limits json
  |> Result.map_error ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

module Capture = struct
  type t =
    { producer : Queue_producer.t option
    ; origin : P.Inference_origin.t option
    }
  [@@deriving equal]

  let unavailable = { producer = None; origin = None }

  let validate t =
    let open Result.Let_syntax in
    let%bind () =
      Option.value_map t.producer ~default:(Ok ()) ~f:Queue_producer.validate
    in
    let%bind () =
      Option.value_map t.origin ~default:(Ok ()) ~f:P.Inference_origin.validate
    in
    match t.producer, t.origin with
    | None, Some _ -> invalid "queued origin requires an actual producer correlation"
    | None, None | Some _, _ -> Ok ()
  ;;

  let create ~producer ~origin =
    let t = { producer = Some producer; origin } in
    Result.map (validate t) ~f:(fun () -> t)
  ;;
end

module Entry = struct
  type t =
    { ordinal : int64
    ; payload_sha256 : string
    ; capture : Capture.t
    }
  [@@deriving equal]

  let ordinal t = t.ordinal
  let payload_sha256 t = t.payload_sha256
  let producer t = t.capture.producer
  let origin t = t.capture.origin
end

module Consumption = struct
  type t =
    | Keep
    | Consume_head
  [@@deriving equal, sexp]
end

type t =
  { source : P.Run_source.t
  ; head_ordinal : int64
  ; next_ordinal : int64
  ; entries : Entry.t list
  }

let source t = t.source
let head_ordinal t = t.head_ordinal
let next_ordinal t = t.next_ordinal
let entries t = t.entries

let valid_hash hash =
  String.length hash = 64
  && String.for_all hash ~f:(function
    | '0' .. '9' | 'a' .. 'f' -> true
    | _ -> false)
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = P.Run_source.validate t.source in
  if Int64.(t.head_ordinal < 0L || t.next_ordinal < t.head_ordinal)
  then invalid "invalid queued occurrence frontier"
  else if List.length t.entries > max_rows
  then invalid "queued occurrence row limit"
  else if
    not
      (Int64.equal
         Int64.(t.next_ordinal - t.head_ordinal)
         (Int64.of_int (List.length t.entries)))
  then invalid "queued occurrence count differs from frontier"
  else
    List.foldi t.entries ~init:(Ok ()) ~f:(fun offset result (entry : Entry.t) ->
      let%bind () = result in
      if
        (not (Int64.equal entry.ordinal Int64.(t.head_ordinal + of_int offset)))
        || not (valid_hash entry.payload_sha256)
      then invalid "invalid queued occurrence identity or digest"
      else Capture.validate entry.capture)
;;

let validate_queue t queue =
  let open Result.Let_syntax in
  let%bind () = validate t in
  let%bind hashes = Queue_payload.digests queue in
  if List.equal String.equal hashes (List.map t.entries ~f:Entry.payload_sha256)
  then Ok ()
  else invalid "queued occurrence metadata differs from raw queue"
;;

let bootstrap ~source ~queue =
  let open Result.Let_syntax in
  let%bind () = P.Run_source.validate source in
  let%map hashes = Queue_payload.digests queue in
  { source
  ; head_ordinal = 0L
  ; next_ordinal = Int64.of_int (List.length hashes)
  ; entries =
      List.mapi hashes ~f:(fun ordinal payload_sha256 ->
        { Entry.ordinal = Int64.of_int ordinal
        ; payload_sha256
        ; capture = Capture.unavailable
        })
  }
;;

let retained t consume =
  match consume, t.entries with
  | Consumption.Keep, rows -> Ok (t.head_ordinal, rows)
  | Consume_head, _ :: rows -> Ok (Int64.succ t.head_ordinal, rows)
  | Consume_head, [] -> invalid "cannot consume an empty queued occurrence frontier"
;;

let advance t ~before ~after ~consume ~appended =
  let open Result.Let_syntax in
  let%bind () = validate_queue t before in
  let%bind head_ordinal, rows = retained t consume in
  let%bind hashes = Queue_payload.digests after in
  let preserved, fresh = List.split_n hashes (List.length rows) in
  if not (List.equal String.equal preserved (List.map rows ~f:Entry.payload_sha256))
  then invalid "queued transition did not preserve the exact retained suffix"
  else if List.length fresh <> List.length appended
  then invalid "queued transition capture count differs from appended suffix"
  else if Int64.(of_int (List.length fresh) > max_value - t.next_ordinal)
  then invalid "queued occurrence frontier exhausted"
  else (
    let%bind () =
      List.fold_result appended ~init:() ~f:(fun () capture -> Capture.validate capture)
    in
    let fresh_rows =
      List.mapi (List.zip_exn fresh appended) ~f:(fun offset (payload_sha256, capture) ->
        { Entry.ordinal = Int64.(t.next_ordinal + of_int offset)
        ; payload_sha256
        ; capture
        })
    in
    let incoming =
      { t with
        head_ordinal
      ; next_ordinal = Int64.(t.next_ordinal + of_int (List.length fresh))
      ; entries = rows @ fresh_rows
      }
    in
    let%map () = validate incoming in
    incoming)
;;

let validate_transition ~previous ~before ~incoming ~after =
  let open Result.Let_syntax in
  let%bind () = validate_queue previous before in
  let%bind () = validate_queue incoming after in
  if not (P.Run_source.equal previous.source incoming.source)
  then invalid "queued occurrence lifetime changed without an enclosing source transition"
  else (
    let%bind consume =
      if Int64.equal previous.head_ordinal incoming.head_ordinal
      then Ok Consumption.Keep
      else if
        Int64.(previous.head_ordinal < max_value)
        && Int64.equal (Int64.succ previous.head_ordinal) incoming.head_ordinal
      then Ok Consumption.Consume_head
      else invalid "queued transition consumed more than one head"
    in
    let%bind _, rows = retained previous consume in
    let preserved, fresh = List.split_n incoming.entries (List.length rows) in
    if not (List.equal Entry.equal rows preserved)
    then invalid "queued transition changed a retained occurrence"
    else if
      Int64.(of_int (List.length fresh) > max_value - previous.next_ordinal)
      || not
           (Int64.equal
              incoming.next_ordinal
              Int64.(previous.next_ordinal + of_int (List.length fresh)))
    then invalid "queued transition reuses or skips occurrence ordinals"
    else Ok ())
;;

let entry_json (entry : Entry.t) =
  `Object
    [ "ordinal", `String (Int64.to_string entry.ordinal)
    ; "payload_sha256", `String entry.payload_sha256
    ; ( "producer"
      , Option.value_map entry.capture.producer ~default:`Null ~f:Queue_producer.to_jsonaf
      )
    ; ( "origin"
      , Option.value_map entry.capture.origin ~default:`Null ~f:P.Inference_origin.to_json
      )
    ]
;;

let to_jsonaf t =
  `Object
    [ "schema_version", `Number "1"
    ; "source", P.Run_source.to_json t.source
    ; "head_ordinal", `String (Int64.to_string t.head_ordinal)
    ; "next_ordinal", `String (Int64.to_string t.next_ordinal)
    ; "entries", `Array (List.map t.entries ~f:entry_json)
    ]
;;

let ordinal_of_json = function
  | `String encoded when String.length encoded <= 19 ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value >= 0L) && String.equal encoded (Int64.to_string value)
       -> Ok value
     | Some _ | None -> invalid "invalid queued occurrence ordinal")
  | _ -> invalid "queued occurrence ordinal must be a canonical decimal string"
;;

let entry_of_json json =
  let open Result.Let_syntax in
  let%bind fields = J.fields json in
  let get name decode = J.required_as fields name decode in
  let%bind ordinal = get "ordinal" ordinal_of_json in
  let%bind payload_sha256 = get "payload_sha256" J.string in
  let%bind producer =
    get "producer" (function
      | `Null -> Ok None
      | json -> Result.map (Queue_producer.of_jsonaf json) ~f:Option.some)
  in
  let%map origin =
    get "origin" (function
      | `Null -> Ok None
      | json -> Result.map (P.Inference_origin.of_json json) ~f:Option.some)
  in
  { Entry.ordinal; payload_sha256; capture = { Capture.producer; origin } }
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () = bounded json in
  let%bind fields = J.fields json in
  let get name decode = J.required_as fields name decode in
  let%bind version = get "schema_version" (J.bounded_int ~min:0 ~max:Int.max_value) in
  if version <> 1
  then invalid "unsupported queued occurrence schema"
  else (
    let%bind source = get "source" P.Run_source.of_json in
    let%bind head_ordinal = get "head_ordinal" ordinal_of_json in
    let%bind next_ordinal = get "next_ordinal" ordinal_of_json in
    let%bind entries = get "entries" (J.list entry_of_json) in
    let t = { source; head_ordinal; next_ordinal; entries } in
    let%map () = validate t in
    t)
;;

let object_shape fields =
  D.Shape.object_ fields
  |> function
  | Ok shape -> shape
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let value_fields names =
  object_shape (List.map names ~f:(fun name -> name, D.Shape.value))
;;

let origin_shape =
  D.Shape.tagged_object
    ~discriminator:"type"
    [ "unbound", value_fields [ "schema_version"; "type" ]
    ; "run", value_fields [ "schema_version"; "type"; "run_id" ]
    ]
  |> function
  | Ok shape -> shape
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let shape =
  let entry =
    object_shape
      [ "ordinal", D.Shape.value
      ; "payload_sha256", D.Shape.value
      ; "producer", D.Shape.nullable Queue_producer.shape
      ; "origin", D.Shape.nullable origin_shape
      ]
  in
  let rows =
    D.Shape.array entry ~identity_field:(Some "ordinal")
    |> function
    | Ok shape -> shape
    | Error error -> raise_s [%sexp (error : D.Error.t)]
  in
  object_shape
    [ "schema_version", D.Shape.value
    ; ( "source"
      , value_fields [ "script_id"; "source_sha256"; "generation"; "installation_epoch" ]
      )
    ; "head_ordinal", D.Shape.value
    ; "next_ordinal", D.Shape.value
    ; "entries", rows
    ]
;;

let validate_encoded_capacity = bounded
