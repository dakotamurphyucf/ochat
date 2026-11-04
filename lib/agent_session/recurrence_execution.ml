open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module Intent = Recurrence_execution_intent
module Request = Recurrence_execution_attempt
module Attempt = Recurrence_execution_record

type projection =
  { intent : Intent.t
  ; attempts : Attempt.t list
  }

type t =
  { projection : projection
  ; json : Jsonaf.t
  }

let intent t = t.projection.intent
let attempts t = t.projection.attempts
let current_attempt t = List.last_exn (attempts t)
let equal a b = D.Json.equal a.json b.json
let to_json t = t.json
let is_pending t = Option.is_none (Attempt.terminal (current_attempt t))
let invalid message = Error (P.Error.invalid_request message)

let protocol result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create ~max_bytes:(512 * 1024) ~max_depth:48 ~max_fields:8192 ~max_nodes:16384
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

let decode json =
  let open Result.Let_syntax in
  let%bind fields = X.object_ json in
  let%bind intent = X.required fields "intent" Intent.of_json in
  let%bind attempts =
    X.required fields "attempts" (function
      | `Array rows
        when (not (List.is_empty rows)) && List.length rows <= Request.maximum_count ->
        List.map rows ~f:(fun json -> Attempt.of_json json ~intent) |> Result.all
      | _ -> invalid "execution requires one through eight bounded attempts")
  in
  let contiguous =
    List.for_alli attempts ~f:(fun number attempt ->
      Int.equal number (Request.number (Attempt.request attempt)))
  in
  let prior_resolved =
    List.drop_last_exn attempts |> List.for_all ~f:Attempt.is_resolved
  in
  let keys =
    List.map attempts ~f:(fun attempt -> Request.key (Attempt.request attempt))
  in
  if
    contiguous
    && prior_resolved
    && not (List.contains_dup keys ~compare:P.Idempotency_key.compare)
  then Ok { intent; attempts }
  else invalid "execution attempt order, retirement or key identity is invalid"
;;

let encode p =
  `Object
    [ "intent", Intent.to_json p.intent
    ; "attempts", `Array (List.map p.attempts ~f:Attempt.to_json)
    ]
;;

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.recurrence_execution"
    ~shape:(X.fields_shape [ "intent"; "attempts" ])
    ~decode
    ~encode:(fun p -> Ok (encode p))
;;

let reserved_growth t = Attempt.reserved_growth (current_attempt t)

let of_json json =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect ~limits json |> protocol in
  let%bind carrier = D.Domain_codec.decode codec document |> protocol in
  let t = { projection = D.Extension_carrier.value carrier; json } in
  let%bind growth = reserved_growth t in
  let%bind remaining = Capacity_growth.remaining_limits growth limits in
  let%map () = D.Json.validate ~limits:remaining json |> protocol in
  t
;;

let create ~intent ~attempt =
  let open Result.Let_syntax in
  let%bind attempt = Attempt.create ~intent attempt in
  let%bind document =
    D.Domain_codec.encode
      codec
      (D.Extension_carrier.of_authored_value { intent; attempts = [ attempt ] })
    |> protocol
  in
  of_json (D.Document.json document)
;;

let set json name value =
  match json with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (key, old) ->
         key, if String.equal key name then value else old))
  | _ -> assert false
;;

let with_attempts t attempts =
  match D.Json.field t.json ~name:"payload" with
  | Value payload ->
    let payload =
      set payload "attempts" (`Array (List.map attempts ~f:Attempt.to_json))
    in
    of_json (set t.json "payload" payload)
  | Absent | Null -> assert false
;;

let update_current t ~f =
  let%bind.Result current = f (current_attempt t) in
  with_attempts t (List.drop_last_exn (attempts t) @ [ current ])
;;

let record_rejection t ~code =
  update_current t ~f:(fun current -> Attempt.record_rejection current ~code)
;;

let mark_uncertain t ~code =
  update_current t ~f:(fun current -> Attempt.mark_uncertain current ~code)
;;

let resolve_retry t resolution =
  update_current t ~f:(fun current ->
    Attempt.resolve_retry current ~intent:(intent t) resolution)
;;

let associate t origin =
  update_current t ~f:(fun current -> Attempt.associate current ~intent:(intent t) origin)
;;

let settle t reference =
  update_current t ~f:(fun current -> Attempt.settle current reference)
;;

let append_attempt t attempt =
  let open Result.Let_syntax in
  let count = List.length (attempts t) in
  if
    (not (Attempt.is_resolved (current_attempt t)))
    || count >= Request.maximum_count
    || (not (Int.equal (Request.number attempt) count))
    || List.exists (attempts t) ~f:(fun old ->
      P.Idempotency_key.equal (Request.key (Attempt.request old)) (Request.key attempt))
  then invalid "next attempt requires exact resolved predecessor, ordinal and fresh key"
  else (
    let%bind next = Attempt.create ~intent:(intent t) attempt in
    with_attempts t (attempts t @ [ next ]))
;;

let unknown json names =
  match json with
  | `Object fields ->
    `Object
      (List.filter fields ~f:(fun (name, _) ->
         not (List.mem names name ~equal:String.equal)))
  | _ -> assert false
;;

let payload t =
  match D.Json.field t.json ~name:"payload" with
  | Value json -> json
  | Absent | Null -> assert false
;;

let retains_unknown previous incoming names =
  match unknown previous names, unknown incoming names with
  | `Object old, `Object next ->
    List.for_all old ~f:(fun (name, value) ->
      Option.value_map
        (List.Assoc.find next name ~equal:String.equal)
        ~default:false
        ~f:(D.Json.equal value))
  | _ -> assert false
;;

let validate_transition t ~incoming =
  let open Result.Let_syntax in
  let previous_count = List.length (attempts t) in
  let incoming_count = List.length (attempts incoming) in
  if
    (not (Intent.equal (intent t) (intent incoming)))
    || (not (retains_unknown t.json incoming.json [ "payload" ]))
    || (not (retains_unknown (payload t) (payload incoming) [ "intent"; "attempts" ]))
    || incoming_count < previous_count
    || incoming_count > previous_count + 1
  then invalid "execution identity, compatible metadata or attempt history changed"
  else (
    let%bind () =
      List.foldi (attempts t) ~init:(Ok ()) ~f:(fun index result old ->
        let%bind () = result in
        Attempt.validate_transition old ~incoming:(List.nth_exn (attempts incoming) index))
    in
    if
      Int.equal previous_count incoming_count
      || Attempt.is_resolved (List.nth_exn (attempts incoming) (previous_count - 1))
    then Ok ()
    else invalid "execution appended without resolving the previous attempt")
;;

let capacity_projection t =
  let current = current_attempt t in
  let attempts =
    List.map (List.drop_last_exn (attempts t)) ~f:Attempt.to_json
    @ [ Attempt.capacity_projection current ]
  in
  match D.Json.field t.json ~name:"payload" with
  | Value payload -> set t.json "payload" (set payload "attempts" (`Array attempts))
  | Absent | Null -> assert false
;;
