open! Core
module P = Agent_protocol
module D = Document_schema
module C = D.Extension_carrier
module X = Persistence_codec
module R = Agent_store.Delegation_store
module S = Child_usage_snapshot
module Origin = Agent_store.Delegation_goal_origin

module Inventory = struct
  type t =
    | Complete_inventory
    | Historical_children_unknown
  [@@deriving equal, sexp_of]
end

type slot =
  { reference : R.Reference.t
  ; origin : Origin.t option
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
let max_children t = (C.value t).max_children
let max_encoded_bytes t = (C.value t).max_encoded_bytes
let references t = List.map (C.value t).slots ~f:(fun slot -> slot.reference)

let find t child =
  List.find (C.value t).slots ~f:(fun slot -> P.Id.Session.equal child (id slot))
;;

let find_reference t ~reference =
  match find t reference.R.Reference.child_session_id with
  | Some slot when R.Reference.equal reference slot.reference -> Ok slot
  | Some _ -> invalid "child admission identity differs"
  | None -> invalid "child has no registered contribution slot"
;;

let origin t ~reference =
  Result.map (find_reference t ~reference) ~f:(fun slot -> slot.origin)
;;

let snapshot t ~reference =
  Result.map (find_reference t ~reference) ~f:(fun slot -> slot.snapshot)
;;

let has_goal_origins t =
  List.exists (C.value t).slots ~f:(fun slot -> Option.is_some slot.origin)
;;

let slot_json slot ~snapshot =
  `Object
    ([ "child_session_id", P.Id.Session.to_json (id slot)
     ; "reference", R.reference_to_jsonaf slot.reference
     ; "snapshot", snapshot
     ]
     @ Option.to_list
         (Option.map slot.origin ~f:(fun origin -> "goal_origin", Origin.to_json origin))
    )
;;

let validate_snapshot_origin slot snapshot =
  match slot.origin, S.goal_scopes snapshot with
  | None, None -> Ok ()
  | None, Some _ -> invalid "historical child slot cannot acquire a scoped origin"
  | Some origin, _ -> S.validate_origin snapshot ~origin
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
                 slot_json slot ~snapshot:(X.option_json S.to_json slot.snapshot))) )
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
          then validate_snapshot_origin slot snapshot
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
         let%bind origin =
           match D.Json.field raw ~name:"goal_origin" with
           | Absent -> Ok None
           | Null -> invalid "child Goal origin must be omitted or an explicit origin"
           | Value raw ->
             Origin.of_json ~max_scopes:Int.max_value ~limits raw
             |> Result.map ~f:Option.some
         in
         if P.Id.Session.equal expected reference.child_session_id
         then Ok { reference; origin; snapshot }
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
             ; "goal_origin", Origin.shape
             ; "snapshot", D.Shape.nullable S.shape
             ]) )
    ]
;;

let goal_semantic = P.Session_storage_semantics.goal_descendants

let create_codec ~decode ~encode =
  match
    D.Domain_codec.create
      ~version:2
      ~limits
      ~kind:"ochat.child_contributions"
      ~shape
      ~supported_semantics:[ goal_semantic ]
      ~decode:(fun raw -> X.document_result (decode raw))
      ~encode:(fun value -> X.document_result (encode value))
  with
  | Ok codec -> codec
  | Error error -> raise_s [%sexp "invalid child contribution codec", (error : D.Error.t)]
;;

let codec = create_codec ~decode ~encode
let probe_codec = create_codec ~decode:(fun raw -> Ok raw) ~encode:(fun raw -> Ok raw)

let encode_document t =
  let open Result.Let_syntax in
  let%bind document = D.Domain_codec.encode codec t |> domain in
  let semantics = D.Document.required_semantics document in
  if has_goal_origins t && not (List.mem semantics goal_semantic ~equal:String.equal)
  then (
    match D.Document.json document with
    | `Object fields ->
      D.Document.inspect
        ~limits
        (`Object
            (List.Assoc.add
               fields
               ~equal:String.equal
               "required_semantics"
               (`Array
                   (List.map (semantics @ [ goal_semantic ]) ~f:(fun name -> `String name)))))
      |> domain
    | `Null | `False | `True | `String _ | `Number _ | `Array _ ->
      invalid "child contribution document must be an object")
  else Ok document
;;

let raw_snapshots document =
  let open Result.Let_syntax in
  let%bind payload = X.object_ (D.Document.payload document) in
  let%bind children = X.required payload "children" (X.list X.raw) in
  List.fold_result children ~init:String.Map.empty ~f:(fun snapshots raw ->
    let%bind fields = X.object_ raw in
    let%bind child = X.required fields "child_session_id" P.Id.Session.of_json in
    let%bind snapshot = X.required fields "snapshot" (X.nullable X.raw) in
    match Map.add snapshots ~key:(P.Id.Session.to_string child) ~data:snapshot with
    | `Ok snapshots -> Ok snapshots
    | `Duplicate -> invalid "duplicate preserved child snapshot")
;;

type capacity =
  { growth : Capacity_growth.t
  ; required_depth : int
  }

let capacity_of_document t current =
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
                    let snapshot =
                      match slot.origin with
                      | None -> S.capacity_projection ~reference:slot.reference
                      | Some origin ->
                        Child_usage_capacity.projection
                          ~reference:slot.reference
                          ~origin
                          ~snapshot:slot.snapshot
                    in
                    slot_json slot ~snapshot))
             else value )))
    | `Null | `False | `True | `String _ | `Number _ | `Array _ -> assert false
  in
  let%bind carrier = D.Domain_codec.decode probe_codec current |> domain in
  let%bind future =
    D.Domain_codec.encode probe_codec (C.with_value carrier future) |> domain
  in
  let%bind before = Capacity_growth.measure (D.Document.json current) ~limits in
  let%bind after = Capacity_growth.measure (D.Document.json future) ~limits in
  let%bind raw = raw_snapshots current in
  let%bind before_depth = Capacity_depth.measure (D.Document.json current) ~limits in
  let%bind after_depth = Capacity_depth.measure (D.Document.json future) ~limits in
  let%bind capacity =
    List.fold_result
      (C.value t).slots
      ~init:
        { growth = Capacity_growth.positive_difference after ~previous:before
        ; required_depth = Int.max before_depth after_depth
        }
      ~f:(fun capacity slot ->
        match slot.origin with
        | None -> Ok capacity
        | Some origin ->
          let%bind raw_snapshot =
            Map.find raw (P.Id.Session.to_string (id slot))
            |> Result.of_option
                 ~error:(P.Error.invalid_request "preserved child snapshot is missing")
          in
          let%bind scoped =
            Child_usage_capacity.capture ~origin ~snapshot:slot.snapshot ~raw_snapshot
          in
          let%map growth =
            Capacity_growth.add capacity.growth (Child_usage_capacity.extra scoped)
          in
          (* Actual array path: payload/children/<slot>/snapshot/goal_scopes. *)
          { growth
          ; required_depth =
              Int.max
                capacity.required_depth
                (5 + Child_usage_capacity.required_depth scoped)
          })
  in
  let%map () = Capacity_depth.validate capacity.required_depth ~ancestors:0 ~limits in
  capacity
;;

let reserved_growth_of_document t current =
  let%map.Result capacity = capacity_of_document t current in
  capacity.growth
;;

let required_depth t =
  let open Result.Let_syntax in
  let%bind current = encode_document t in
  let%map capacity = capacity_of_document t current in
  capacity.required_depth
;;

let reserved_growth t =
  let open Result.Let_syntax in
  let%bind document = encode_document t in
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
  let%bind document = encode_document t in
  let%map () = validate_capacity t ~current:document document in
  D.Document.json document
;;

let of_jsonaf raw =
  let open Result.Let_syntax in
  let%bind document = D.Document.inspect raw ~limits |> domain in
  let%bind step =
    D.Conversion.Step.of_function
      ~kind:"ochat.child_contributions"
      ~from_version:1
      ~f:Result.return
    |> domain
  in
  let%bind conversion =
    D.Conversion.create
      ~limits
      ~targets:[ "ochat.child_contributions", 2 ]
      ~max_steps:1
      ~max_operations:100_000
      ~steps:[ step ]
    |> domain
  in
  let%bind document = D.Conversion.upgrade conversion document |> domain in
  let%bind t = D.Domain_codec.decode codec document |> domain in
  let%bind () =
    if
      has_goal_origins t
      && not
           (List.mem
              (D.Document.required_semantics document)
              goal_semantic
              ~equal:String.equal)
    then invalid "scoped child contributions require their storage semantics"
    else Ok ()
  in
  (* Growth uses the canonical re-encoding; the raw input must independently
     fit that reservation, including all retained forward data. *)
  let%bind current = encode_document t in
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

let register_origin t ~reference ~origin =
  let open Result.Let_syntax in
  match find t reference.R.Reference.child_session_id with
  | Some slot ->
    if
      R.Reference.equal reference slot.reference
      && Option.equal Origin.equal origin slot.origin
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
        ({ reference; origin; snapshot = None } :: value.slots)
        ~compare:(fun left right -> P.Id.Session.compare (id left) (id right))
    in
    admit t { value with slots }
;;

let register t ~reference = register_origin t ~reference ~origin:None

let register_with_origin t ~reference ~origin =
  register_origin t ~reference ~origin:(Some origin)
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
  let%bind () = validate_snapshot_origin slot snapshot in
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

let aggregate_selected t ~select =
  let open Result.Let_syntax in
  let%bind initial_coverage =
    S.Coverage.create
      ~historical_children_unknown:
        (Inventory.equal (inventory t) Historical_children_unknown)
      ~unobserved_children:0L
  in
  let%bind unknown =
    S.Coverage.create ~historical_children_unknown:false ~unobserved_children:1L
  in
  let%map partition, coverage =
    List.fold_result
      (C.value t).slots
      ~init:(S.Partition.empty, initial_coverage)
      ~f:(fun (partition, coverage) slot ->
        match slot.origin with
        | None ->
          let%map coverage = S.Coverage.add_disjoint coverage unknown in
          partition, coverage
        | Some origin ->
          let%bind selected = select (Origin.scopes origin) in
          (match selected with
           | None -> Ok (partition, coverage)
           | Some scope ->
             (match slot.snapshot with
              | None ->
                let%map coverage = S.Coverage.add_disjoint coverage unknown in
                partition, coverage
              | Some snapshot ->
                let%bind () = S.validate_origin snapshot ~origin in
                let%bind projected =
                  Option.bind (S.goal_scopes snapshot) ~f:(fun scopes ->
                    List.find scopes ~f:(fun projected ->
                      Origin.Scope.equal scope (S.Goal_scope.scope projected)))
                  |> Result.of_option
                       ~error:
                         (P.Error.invalid_request
                            "child snapshot lacks its frozen Goal scope")
                in
                let%bind contribution = S.Goal_scope.total projected in
                let%bind partition = S.Partition.add_disjoint partition contribution in
                let%map coverage =
                  S.Coverage.add_disjoint coverage (S.Goal_scope.coverage projected)
                in
                partition, coverage)))
  in
  S.Descendants.create ~partition ~coverage
;;

let aggregate_scope t ~scope =
  aggregate_selected t ~select:(fun scopes ->
    match
      List.find scopes ~f:(fun other ->
        Int.equal (Origin.Scope.compare_root scope other) 0)
    with
    | None -> Ok None
    | Some other ->
      if Origin.Scope.equal scope other
      then Ok (Some other)
      else invalid "inherited Goal scope attribution differs")
;;

let aggregate_root t ~owner ~goal_id =
  aggregate_selected t ~select:(fun scopes ->
    Ok
      (List.find scopes ~f:(fun scope ->
         P.Session_ref.equal owner (Origin.Scope.owner scope)
         && P.Id.Goal.equal goal_id (Origin.Scope.goal_id scope))))
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
        else if not (Option.equal Origin.equal old.origin next.origin)
        then invalid "retained child Goal origin changed"
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

let validate_ordinary_update previous ~incoming =
  let open Result.Let_syntax in
  match previous, incoming with
  | None, None -> Ok ()
  | Some _, None -> invalid "ordinary update discarded child contributions"
  | None, Some incoming ->
    if has_goal_origins incoming
    then invalid "known child origins require a paired descendant Update"
    else (
      match inventory incoming with
      | Complete_inventory -> invalid "historical child inventory cannot become complete"
      | Historical_children_unknown ->
        let%map _ = to_jsonaf incoming in
        ())
  | Some previous, Some incoming ->
    let%bind merged = adopt previous ~incoming in
    let%bind merged_json = to_jsonaf merged in
    let%bind incoming_json = to_jsonaf incoming in
    let%bind () =
      if D.Json.equal merged_json incoming_json
      then Ok ()
      else invalid "ordinary child update omitted preserved data"
    in
    List.fold_result (C.value incoming).slots ~init:() ~f:(fun () slot ->
      match slot.origin with
      | None -> Ok ()
      | Some origin ->
        (match find previous (id slot) with
         | Some old
           when Option.equal Origin.equal old.origin (Some origin)
                && Option.equal
                     (fun before after -> String.equal (S.digest before) (S.digest after))
                     old.snapshot
                     slot.snapshot -> Ok ()
         | Some _ | None ->
           invalid "known child movement requires a paired descendant Update"))
;;
