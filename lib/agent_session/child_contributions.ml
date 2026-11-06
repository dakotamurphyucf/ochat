open! Core
module P = Agent_protocol
module D = Document_schema
module C = D.Extension_carrier
module X = Persistence_codec
module R = Agent_store.Delegation_store
module S = Child_usage_snapshot

module Inventory = struct
  type t =
    | Complete_inventory
    | Historical_children_unknown
  [@@deriving equal, sexp_of]
end

type slot =
  { reference : R.Reference.t
  ; snapshot : S.t option
  }

type projection =
  { inventory : Inventory.t
  ; max_children : int
  ; max_encoded_bytes : int
  ; slots : slot list
  }

type t = projection C.t

type disposition =
  | Added
  | Replaced
  | Duplicate
  | Stale
[@@deriving equal, sexp_of]

let invalid message = Error (P.Error.invalid_request message)

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  X.limits ~max_bytes:(64 * 1024 * 1024)
  |> domain
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let id slot = slot.reference.R.Reference.child_session_id
let inventory t = (C.value t).inventory
let references t = List.map (C.value t).slots ~f:(fun slot -> slot.reference)

let find t child =
  List.find (C.value t).slots ~f:(fun slot -> P.Id.Session.equal child (id slot))
;;

let inventory_to_json = function
  | Inventory.Complete_inventory -> `String "complete_inventory"
  | Historical_children_unknown -> `String "historical_children_unknown"
;;

let encode value =
  Ok
    (`Object
        [ "inventory", inventory_to_json value.inventory
        ; "max_children", X.integer_json value.max_children
        ; "max_encoded_bytes", X.integer_json value.max_encoded_bytes
        ; ( "children"
          , `Array
              (List.map value.slots ~f:(fun slot ->
                 `Object
                   [ "child_session_id", P.Id.Session.to_json (id slot)
                   ; "reference", R.reference_to_jsonaf slot.reference
                   ; "snapshot", X.option_json S.to_json slot.snapshot
                   ])) )
        ])
;;

let validate_projection value =
  let open Result.Let_syntax in
  let%bind () =
    if
      value.max_children <= 0
      || value.max_encoded_bytes <= 0
      || value.max_encoded_bytes > 64 * 1024 * 1024
      || List.length value.slots > value.max_children
    then invalid "child contribution profile exceeded"
    else Ok ()
  in
  let%map _ =
    List.fold_result value.slots ~init:(None, []) ~f:(fun (previous, keys) slot ->
      let%bind () =
        R.validate_reference slot.reference
        |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
      in
      let%bind () =
        if
          Option.exists previous ~f:(fun previous ->
            P.Id.Session.compare previous (id slot) >= 0)
          || List.exists keys ~f:(R.Key.equal slot.reference.key)
        then invalid "child contribution identities must be unique and ordered"
        else Ok ()
      in
      let%map () =
        match slot.snapshot with
        | None -> Ok ()
        | Some snapshot ->
          if R.Reference.equal slot.reference (S.reference snapshot)
          then Ok ()
          else invalid "child snapshot wrapper differs"
      in
      Some (id slot), slot.reference.key :: keys)
  in
  ()
;;

let decode raw =
  let open Result.Let_syntax in
  let%bind fields = X.object_ raw in
  let%bind inventory =
    X.required fields "inventory" (fun raw ->
      let%bind.Result name = P.Json_codec.string raw in
      match name with
      | "complete_inventory" -> Ok Inventory.Complete_inventory
      | "historical_children_unknown" -> Ok Inventory.Historical_children_unknown
      | _ -> invalid "unknown child inventory coverage")
  in
  let%bind max_children = X.required fields "max_children" X.integer in
  let%bind max_encoded_bytes = X.required fields "max_encoded_bytes" X.integer in
  let%bind slots =
    X.required
      fields
      "children"
      (X.list (fun raw ->
         let%bind fields = X.object_ raw in
         let%bind expected = X.required fields "child_session_id" P.Id.Session.of_json in
         let%bind reference =
           X.required fields "reference" (fun raw ->
             R.reference_of_jsonaf raw
             |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error)
         in
         let%bind snapshot = X.required fields "snapshot" (X.nullable S.of_json) in
         if P.Id.Session.equal expected reference.child_session_id
         then Ok { reference; snapshot }
         else invalid "child contribution wrapper differs"))
  in
  let value = { inventory; max_children; max_encoded_bytes; slots } in
  let%map () = validate_projection value in
  value
;;

let shape =
  X.shape_exn
    [ "inventory", D.Shape.value
    ; "max_children", D.Shape.value
    ; "max_encoded_bytes", D.Shape.value
    ; ( "children"
      , X.array_shape_exn
          ~identity_field:"child_session_id"
          (X.shape_exn
             [ "child_session_id", D.Shape.value
             ; "reference", R.reference_shape
             ; "snapshot", D.Shape.nullable S.shape
             ]) )
    ]
;;

let codec =
  X.codec_exn ~version:1 ~limits ~kind:"ochat.child_contributions" ~shape ~decode ~encode
;;

let probe_codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.child_contributions"
    ~shape
    ~decode:(fun raw -> Ok raw)
    ~encode:(fun raw -> Ok raw)
;;

let reserved_growth_of_document t current =
  let open Result.Let_syntax in
  let%bind raw = encode (C.value t) in
  let future =
    match raw with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (key, value) ->
           ( key
           , if String.equal key "children"
             then
               `Array
                 (List.map (C.value t).slots ~f:(fun slot ->
                    `Object
                      [ "child_session_id", P.Id.Session.to_json (id slot)
                      ; "reference", R.reference_to_jsonaf slot.reference
                      ; "snapshot", S.capacity_projection ~reference:slot.reference
                      ]))
             else value )))
    | `Null | `False | `True | `String _ | `Number _ | `Array _ -> assert false
  in
  let%bind carrier = D.Domain_codec.decode probe_codec current |> domain in
  let%bind future =
    D.Domain_codec.encode probe_codec (C.with_value carrier future) |> domain
  in
  let%bind before = Capacity_growth.measure (D.Document.json current) ~limits in
  let%map after = Capacity_growth.measure (D.Document.json future) ~limits in
  Capacity_growth.positive_difference after ~previous:before
;;

let reserved_growth t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec t |> domain in
  reserved_growth_of_document t document
;;

let validate_capacity t ~current document =
  let open Result.Let_syntax in
  let%bind growth = reserved_growth_of_document t current in
  let%bind remaining = Capacity_growth.remaining_limits growth limits in
  let%bind () = D.Document.validate document ~limits:remaining |> domain in
  let%bind measured = Capacity_growth.measure (D.Document.json document) ~limits in
  let size = Capacity_growth.encoded_bytes measured in
  let growth_bytes = Capacity_growth.encoded_bytes growth in
  if
    size > (C.value t).max_encoded_bytes
    || growth_bytes > (C.value t).max_encoded_bytes - size
  then
    Error
      (P.Error.create
         Resource_limit
         ~message:"child contribution byte capacity exhausted"
         ~retryable:false
         ())
  else Ok ()
;;

let to_jsonaf t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec t |> domain in
  let%map () = validate_capacity t ~current:document document in
  D.Document.json document
;;

let of_jsonaf raw =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect raw ~limits |> domain in
  let%bind t = D.Domain_codec.decode codec document |> domain in
  (* Growth uses the canonical re-encoding; the raw input must independently
     fit that reservation, including all retained forward data. *)
  let%bind current = D.Domain_codec.encode codec t |> domain in
  let%map () = validate_capacity t ~current document in
  t
;;

let admit t value =
  let open Result.Let_syntax in
  let%bind () = validate_projection value in
  let t = C.with_value t value in
  let%map _ = to_jsonaf t in
  t
;;

let create ~inventory ~max_children ~max_encoded_bytes =
  let value = { inventory; max_children; max_encoded_bytes; slots = [] } in
  admit (C.of_authored_value value) value
;;

let register t ~reference =
  let open Result.Let_syntax in
  match find t reference.R.Reference.child_session_id with
  | Some slot ->
    if R.Reference.equal reference slot.reference
    then Ok t
    else invalid "child admission identity differs"
  | None ->
    let value = C.value t in
    let%bind () =
      if List.length value.slots >= value.max_children
      then
        Error
          (P.Error.create
             Resource_limit
             ~message:"child contribution count capacity exhausted"
             ~retryable:false
             ())
      else Ok ()
    in
    let slots =
      List.sort
        ({ reference; snapshot = None } :: value.slots)
        ~compare:(fun left right -> P.Id.Session.compare (id left) (id right))
    in
    admit t { value with slots }
;;

let replace t ~snapshot =
  let open Result.Let_syntax in
  let reference = S.reference snapshot in
  let%bind slot =
    Result.of_option
      (find t reference.child_session_id)
      ~error:
        (P.Error.invalid_request
           "child snapshot has no actual registered contribution slot")
  in
  let%bind () =
    if R.Reference.equal reference slot.reference
    then Ok ()
    else invalid "child snapshot admission changed"
  in
  let disposition =
    match slot.snapshot with
    | None -> Added
    | Some previous ->
      if Int64.(S.revision snapshot < S.revision previous)
      then Stale
      else if Int64.equal (S.revision snapshot) (S.revision previous)
      then Duplicate
      else Replaced
  in
  match disposition with
  | Stale -> Ok (t, Stale)
  | Duplicate ->
    let%map () =
      Option.value_map slot.snapshot ~default:(Ok ()) ~f:(fun previous ->
        S.validate_update previous ~incoming:snapshot)
    in
    t, Duplicate
  | Added | Replaced ->
    let%bind () =
      Option.value_map slot.snapshot ~default:(Ok ()) ~f:(fun previous ->
        S.validate_update previous ~incoming:snapshot)
    in
    let value = C.value t in
    let%map candidate =
      admit
        t
        { value with
          slots =
            List.map value.slots ~f:(fun old ->
              if P.Id.Session.equal (id slot) (id old)
              then { old with snapshot = Some snapshot }
              else old)
        }
    in
    candidate, disposition
;;

let known_partition t =
  List.fold_result (C.value t).slots ~init:S.Partition.empty ~f:(fun total slot ->
    match slot.snapshot with
    | None -> Ok total
    | Some snapshot ->
      let%bind.Result contribution = S.total snapshot in
      S.Partition.add_disjoint total contribution)
;;

let observed_coverage t =
  let open Result.Let_syntax in
  let%bind initial =
    S.Coverage.create
      ~historical_children_unknown:
        (match inventory t with
         | Complete_inventory -> false
         | Historical_children_unknown -> true)
      ~unobserved_children:0L
  in
  List.fold_result (C.value t).slots ~init:initial ~f:(fun total slot ->
    let%bind next =
      match slot.snapshot with
      | Some snapshot -> Ok (S.coverage snapshot)
      | None ->
        S.Coverage.create ~historical_children_unknown:false ~unobserved_children:1L
    in
    S.Coverage.add_disjoint total next)
;;

let aggregate t =
  let open Result.Let_syntax in
  let%bind partition = known_partition t in
  let%map coverage = observed_coverage t in
  S.Descendants.create ~partition ~coverage
;;

let adopt previous ~incoming =
  let open Result.Let_syntax in
  let before = C.value previous
  and after = C.value incoming in
  let%bind () =
    if
      (not (Inventory.equal before.inventory after.inventory))
      || not
           (Int.equal before.max_children after.max_children
            && Int.equal before.max_encoded_bytes after.max_encoded_bytes)
    then invalid "child inventory history or admitted profile changed"
    else Ok ()
  in
  let%bind () =
    List.fold_result before.slots ~init:() ~f:(fun () old ->
      match find incoming (id old) with
      | None -> invalid "child contribution slot cannot be discarded"
      | Some next ->
        if not (R.Reference.equal old.reference next.reference)
        then invalid "retained child admission changed"
        else (
          match old.snapshot, next.snapshot with
          | None, (None | Some _) -> Ok ()
          | Some _, None -> invalid "observed child contribution cannot become unobserved"
          | Some old, Some next -> S.validate_update old ~incoming:next))
  in
  let%bind candidate = D.Domain_codec.adopt codec ~previous ~incoming |> domain in
  let%map _ = to_jsonaf candidate in
  candidate
;;
