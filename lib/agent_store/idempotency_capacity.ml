open! Core
module D = Document_schema

(* Mandatory record_id alone charges78 compact bytes. Even with nullable fields
   omitted, each admitted row has6 direct fields +3 key fields +1 outcome tag,
   and11 value/container nodes. Envelope charges only tighten the record bound.
   Per-row compatibility growth: reference121 + sequence54 + nullable18+18 +
   created timestamp normalization10; fields3+1+1+1, nodes4+1+1+1. *)
module Bound = struct
  let maximum_depth = 256
  let minimum_record_bytes = 78
  let minimum_record_fields = 10
  let minimum_record_nodes = 11
  let growth_bytes = 221
  let growth_fields = 6
  let growth_nodes = 7
  let original_metadata_bytes = 16 * 1024 * 1024

  (* Outcome owner admits at most two independently bounded16MiB components.
     The shared artifact reference therefore permits32MiB, still8 decimal digits.
     This is not the fresh receipt-metadata admission budget. *)
  let maximum_outcome_bytes = 32 * 1024 * 1024
end

type mode =
  | Fresh
  | Existing
[@@deriving sexp]

type budget =
  { bytes : int
  ; fields : int
  ; nodes : int
  }

type t =
  { fresh : D.Limits.t
  ; existing : D.Limits.t
  ; fresh_budget : budget
  ; existing_budget : budget
  }

let expanded ~base ~count ~growth =
  if count > (Int.max_value - base) / growth
  then Error (D.Error.Invalid_configuration "receipt compatibility capacity overflows")
  else Ok (base + (count * growth))
;;

let create ~max_bytes ~max_fields ~max_nodes =
  let open Result.Let_syntax in
  let fresh_budget = { bytes = max_bytes; fields = max_fields; nodes = max_nodes } in
  let%bind fresh =
    D.Limits.create ~max_bytes ~max_fields ~max_nodes ~max_depth:Bound.maximum_depth
  in
  let count =
    Int.min
      (max_bytes / Bound.minimum_record_bytes)
      (Int.min
         (max_fields / Bound.minimum_record_fields)
         (max_nodes / Bound.minimum_record_nodes))
  in
  let%bind max_bytes = expanded ~base:max_bytes ~count ~growth:Bound.growth_bytes in
  let%bind max_fields = expanded ~base:max_fields ~count ~growth:Bound.growth_fields in
  let%bind max_nodes = expanded ~base:max_nodes ~count ~growth:Bound.growth_nodes in
  let%map existing =
    D.Limits.create ~max_bytes ~max_fields ~max_nodes ~max_depth:Bound.maximum_depth
  in
  { fresh
  ; existing
  ; fresh_budget
  ; existing_budget = { bytes = max_bytes; fields = max_fields; nodes = max_nodes }
  }
;;

let default =
  match
    create
      ~max_bytes:Bound.original_metadata_bytes
      ~max_fields:1_000_000
      ~max_nodes:2_000_000
  with
  | Ok profile -> profile
  | Error error -> raise_s [%sexp "invalid static receipt capacity", (error : D.Error.t)]
;;

let limits t ~mode =
  match mode with
  | Fresh -> t.fresh
  | Existing -> t.existing
;;

let invalid path reason = Error (D.Error.Invalid_field { path; reason })

let object_fields json ~path =
  match json with
  | `Object fields -> Ok fields
  | _ -> invalid path "expected object"
;;

let required json name ~path =
  match D.Json.field json ~name with
  | Value value -> Ok value
  | Absent | Null -> invalid (path @ [ name ]) "required field is absent or null"
;;

let string json ~path =
  match json with
  | `String value -> Ok value
  | _ -> invalid path "expected string"
;;

let required_string json name ~path =
  Result.bind (required json name ~path) ~f:(fun value ->
    string value ~path:(path @ [ name ]))
;;

let put fields name value =
  if List.Assoc.mem fields name ~equal:String.equal
  then
    List.map fields ~f:(fun (key, previous) ->
      key, if String.equal key name then value else previous)
  else fields @ [ name, value ]
;;

let digest json name ~path =
  let open Result.Let_syntax in
  let%bind value = required_string json name ~path in
  if
    String.length value = 64
    && String.for_all value ~f:(fun ch ->
      Char.is_digit ch || Char.(ch >= 'a' && ch <= 'f'))
  then Ok ()
  else invalid (path @ [ name ]) "expected lowercase SHA-256 digest"
;;

let timestamp json ~path =
  Agent_protocol.Timestamp.of_json json
  |> Result.map ~f:(fun timestamp ->
    let canonical = Agent_protocol.Timestamp.to_json timestamp in
    match json, canonical with
    | `String original, `String normalized
      when String.length original >= String.length normalized -> json
    | _ -> canonical)
  |> Result.map_error ~f:(fun _ ->
    D.Error.Invalid_field { path; reason = "invalid timestamp" })
;;

let reserve_sequence json ~path =
  match D.Json.field json ~name:"accepted_transaction_sequence" with
  | Absent | Null -> Ok ()
  | Value (`String encoded) ->
    (match Int64.of_string_opt encoded with
     | Some value when Int64.(value >= 0L) && String.equal encoded (Int64.to_string value)
       -> Ok ()
     | _ ->
       invalid
         (path @ [ "accepted_transaction_sequence" ])
         "expected canonical nonnegative decimal")
  | Value _ ->
    invalid (path @ [ "accepted_transaction_sequence" ]) "expected string or null"
;;

let maximum_reference =
  `Object
    [ "tag", `String "terminal"
    ; "digest", `String (String.make 64 'f')
    ; "encoded_bytes", `String (Int.to_string Bound.maximum_outcome_bytes)
    ]
;;

let reserve_outcome json ~path =
  let open Result.Let_syntax in
  let%bind fields = object_fields json ~path in
  let%bind tag = required_string json "tag" ~path in
  match tag with
  | "pending" -> Ok json
  | "terminal" ->
    let%bind () = digest json "digest" ~path in
    let%bind encoded = required_string json "encoded_bytes" ~path in
    let%bind () =
      match Int.of_string_opt encoded with
      | Some size
        when size > 0
             && size <= Bound.maximum_outcome_bytes
             && String.equal encoded (Int.to_string size) -> Ok ()
      | _ -> invalid (path @ [ "encoded_bytes" ]) "invalid outcome byte count"
    in
    Ok
      (`Object
          (put
             fields
             "encoded_bytes"
             (`String (Int.to_string Bound.maximum_outcome_bytes))))
  | _ -> invalid (path @ [ "tag" ]) "expected pending or terminal reference"
;;

let reserve_record index json =
  let open Result.Let_syntax in
  let path = [ "payload"; "records"; Int.to_string index ] in
  let%bind fields = object_fields json ~path in
  let%bind () = digest json "record_id" ~path in
  let%bind _ = required_string json "request_digest" ~path in
  let%bind retention = required_string json "retention" ~path in
  let%bind () =
    if String.equal retention "standard" || String.equal retention "protected"
    then Ok ()
    else invalid (path @ [ "retention" ]) "invalid retention policy"
  in
  let%bind key = required json "key" ~path in
  let key_path = path @ [ "key" ] in
  let%bind key_fields = object_fields key ~path:key_path in
  let%bind _ = required_string key "principal_id" ~path:key_path in
  let%bind _ = required_string key "method_name" ~path:key_path in
  let%bind _ = required_string key "idempotency_key" ~path:key_path in
  let key_fields =
    match D.Json.field key ~name:"session_id" with
    | Absent -> put key_fields "session_id" `Null
    | Null | Value _ -> key_fields
  in
  let%bind created = required json "created_at" ~path in
  let%bind created = timestamp created ~path:(path @ [ "created_at" ]) in
  let%bind expires =
    match D.Json.field json ~name:"expires_at" with
    | Absent | Null -> Ok `Null
    | Value value -> timestamp value ~path:(path @ [ "expires_at" ])
  in
  let%bind () = reserve_sequence json ~path in
  let%bind outcome = required json "outcome" ~path in
  let%map outcome = reserve_outcome outcome ~path:(path @ [ "outcome" ]) in
  `Object
    (fields
     |> fun fields ->
     put fields "key" (`Object key_fields)
     |> fun fields ->
     put fields "created_at" created
     |> fun fields ->
     put fields "expires_at" expires
     |> fun fields ->
     put
       fields
       "accepted_transaction_sequence"
       (`String (Int64.to_string Int64.max_value))
     |> fun fields -> put fields "outcome" outcome)
;;

let reserved json =
  let open Result.Let_syntax in
  let%bind fields = object_fields json ~path:[] in
  let%bind payload = required json "payload" ~path:[] in
  let%bind payload_fields = object_fields payload ~path:[ "payload" ] in
  let%bind records = required payload "records" ~path:[ "payload" ] in
  let%bind records =
    match records with
    | `Array records -> Ok records
    | _ -> invalid [ "payload"; "records" ] "expected array"
  in
  let%map records = Result.all (List.mapi records ~f:reserve_record) in
  `Object (put fields "payload" (`Object (put payload_fields "records" (`Array records))))
;;

(* Input is validated by the shared schema walker before this private count.
   Recursion follows nesting only (at most256); list folds are tail recursive. *)
let rec structure = function
  | `Object fields ->
    List.fold
      fields
      ~init:{ bytes = 0; fields = List.length fields; nodes = 1 }
      ~f:(fun total (_, value) ->
        let child = structure value in
        { total with
          fields = total.fields + child.fields
        ; nodes = total.nodes + child.nodes
        })
  | `Array values ->
    List.fold values ~init:{ bytes = 0; fields = 0; nodes = 1 } ~f:(fun total value ->
      let child = structure value in
      { total with
        fields = total.fields + child.fields
      ; nodes = total.nodes + child.nodes
      })
  | `String _ | `Number _ | `True | `False | `Null -> { bytes = 0; fields = 0; nodes = 1 }
;;

let measure json ~limits =
  let open Result.Let_syntax in
  let%map bytes = D.Json.validate_and_measure ~limits json in
  { (structure json) with bytes }
;;

let pending_outcomes json =
  match D.Json.field json ~name:"payload" with
  | Value payload ->
    (match D.Json.field payload ~name:"records" with
     | Value (`Array records) ->
       List.filter_map records ~f:(fun record ->
         match D.Json.field record ~name:"outcome" with
         | Value outcome ->
           (match D.Json.field outcome ~name:"tag" with
            | Value (`String "pending") -> Some outcome
            | Absent | Null | Value _ -> None)
         | Absent | Null -> None)
     | Absent | Null | Value _ -> [])
  | Absent | Null -> []
;;

let add_remaining total current maximum ~budget =
  let add current maximum total limit dimension =
    let growth = Int.max 0 (maximum - current) in
    if growth > limit - total
    then Error (D.Error.Limit_exceeded dimension)
    else Ok (total + growth)
  in
  let open Result.Let_syntax in
  let%bind bytes = add current.bytes maximum.bytes total.bytes budget.bytes "bytes" in
  let%bind fields =
    add current.fields maximum.fields total.fields budget.fields "fields"
  in
  let%map nodes = add current.nodes maximum.nodes total.nodes budget.nodes "nodes" in
  { bytes; fields; nodes }
;;

let check t json ~mode =
  let open Result.Let_syntax in
  let document_limits = limits t ~mode in
  let budget =
    match mode with
    | Fresh -> t.fresh_budget
    | Existing -> t.existing_budget
  in
  let%bind () = D.Json.validate ~limits:document_limits json in
  let%bind projected = reserved json in
  let%bind baseline = measure projected ~limits:document_limits in
  let%bind maximum = measure maximum_reference ~limits:(limits default ~mode:Existing) in
  let%map _ =
    List.fold_result (pending_outcomes projected) ~init:baseline ~f:(fun total outcome ->
      let%bind current = measure outcome ~limits:document_limits in
      add_remaining total current maximum ~budget)
  in
  ()
;;
