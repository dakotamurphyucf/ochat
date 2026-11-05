open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module E = Recurrence_entry
module F = Recurrence_definition
module I = Recurrence_identity
module R = Recurrence_command_receipt

let invalid message = Error (P.Error.invalid_request message)
let full message = Error (P.Error.create Resource_limit ~message ~retryable:false ())

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let limits =
  D.Limits.create
    ~max_bytes:(32 * 1024 * 1024)
    ~max_depth:224
    ~max_fields:524288
    ~max_nodes:1048576
  |> Result.map_error ~f:(fun error -> Sexp.to_string_hum (D.Error.sexp_of_t error))
  |> Result.ok_or_failwith
;;

(* The child owns its full universal carrier; caching this admitted serialization
   avoids reencoding unchanged captured artifacts for catalog lookups. *)
module Stored = struct
  type t =
    { value : E.t
    ; encoded : Jsonaf.t
    }

  let create value =
    let%map.Result document = E.to_document value |> domain in
    { value; encoded = D.Document.json document }
  ;;

  let to_json t =
    `Object [ "id", P.Id.Recurrence.to_json (E.id t.value); "document", t.encoded ]
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = X.object_ json in
    let%bind id = X.required f "id" P.Id.Recurrence.of_json in
    let%bind encoded = X.required f "document" X.raw in
    let%bind document = D.Document.inspect encoded ~limits |> domain in
    let%bind value = E.of_document document |> domain in
    if P.Id.Recurrence.equal id (E.id value)
    then Ok { value; encoded }
    else invalid "recurrence wrapper identity disagrees"
  ;;

  let shape = X.shape_exn [ "id", D.Shape.value; "document", D.Shape.value ]
end

type t =
  { revision : int64
  ; entries : Stored.t list
  ; receipts : R.t list
  }

let empty = { revision = 0L; entries = []; receipts = [] }
let revision t = t.revision
let entries t = List.map t.entries ~f:(fun e -> e.Stored.value)
let receipts t = t.receipts

let stored t id =
  List.find t.entries ~f:(fun e -> P.Id.Recurrence.equal (E.id e.Stored.value) id)
;;

let find t id = Option.map (stored t id) ~f:(fun e -> e.Stored.value)

let same_key (a : R.t) (b : R.t) =
  P.Id.Principal.equal a.principal_id b.principal_id
  && P.Idempotency_key.equal a.key b.key
;;

let receipt t ~principal_id ~key ~request_sha256 =
  match
    List.find t.receipts ~f:(fun r ->
      P.Id.Principal.equal r.R.principal_id principal_id
      && P.Idempotency_key.equal r.key key)
  with
  | None -> Ok None
  | Some r ->
    if String.equal r.request_sha256 request_sha256
    then Ok (Some r)
    else
      Error
        (P.Error.create
           Conflict
           ~message:"recurrence command key has another input"
           ~retryable:false
           ())
;;

let receipt_id (r : R.t) =
  Jsonaf.to_string
    (`Array [ P.Id.Principal.to_json r.principal_id; P.Idempotency_key.to_json r.key ])
;;

let receipt_json r = `Object [ "id", `String (receipt_id r); "receipt", R.to_json r ]

let raw_json t =
  `Object
    [ "revision", X.int64_json t.revision
    ; "entries", `Array (List.map t.entries ~f:Stored.to_json)
    ; "receipts", `Array (List.map t.receipts ~f:receipt_json)
    ]
;;

module Reservation = struct
  type t =
    { growth : Capacity_growth.t
    ; required_transition_steps : int
    }

  let growth t = t.growth
  let required_transition_steps t = t.required_transition_steps
end

let reservation t =
  let open Result.Let_syntax in
  let steps =
    List.fold t.entries ~init:0 ~f:(fun n e ->
      Int.max
        n
        (E.required_transition_steps e.Stored.value
         - E.pending_child_associations e.Stored.value
         - E.pending_execution_steps e.Stored.value))
  in
  let steps =
    steps
    + List.sum
        (module Int)
        t.entries
        ~f:(fun e ->
          E.pending_child_associations e.Stored.value
          + E.pending_execution_steps e.Stored.value)
  in
  let%bind growth =
    if steps = 0
    then Ok Capacity_growth.zero
    else
      Capacity_growth.create
        ~bytes:(19 - String.length (Int64.to_string t.revision))
        ~fields:0
        ~nodes:0
  in
  let%map growth =
    List.fold_result t.entries ~init:growth ~f:(fun growth e ->
      let%bind child = E.reserved_growth e.Stored.value in
      Capacity_growth.add growth child)
  in
  { Reservation.growth; required_transition_steps = steps }
;;

let validate_encoded_capacity t json =
  let open Result.Let_syntax in
  let%bind actual = Capacity_growth.measure json ~limits in
  let%bind reserve = reservation t in
  let%bind complete = Capacity_growth.add actual reserve.growth in
  if
    Capacity_growth.encoded_bytes complete > D.Limits.max_bytes limits
    || Capacity_growth.fields complete > D.Limits.max_fields limits
    || Capacity_growth.nodes complete > D.Limits.max_nodes limits
  then full "recurrence catalog lacks reserved durable control capacity"
  else Ok ()
;;

let definition_revision entry = I.Definition.revision (F.key (E.current entry))

let validate_structure t =
  let open Result.Let_syntax in
  if List.length t.entries > 16 || List.length t.receipts > 256 || Int64.(t.revision < 0L)
  then full "recurrence catalog retention capacity exhausted"
  else (
    let%bind () =
      let rows =
        List.concat_map t.entries ~f:(fun entry ->
          match E.frontier entry.Stored.value with
          | Error _ -> []
          | Ok frontier -> Recurrence_frontier.protected_children frontier)
      in
      if
        List.contains_dup
          (List.map rows ~f:Recurrence_child_reservation.child_session_id)
          ~compare:P.Id.Session.compare
        || List.contains_dup
             (List.map rows ~f:Recurrence_child_reservation.transaction_id)
             ~compare:P.Id.Transaction.compare
      then invalid "protected creation identities are not unique"
      else Ok ()
    in
    let%bind reserve = reservation t in
    let%bind () =
      if Int64.(t.revision > max_value - of_int reserve.required_transition_steps)
      then full "recurrence catalog has no reserved safety frontier"
      else Ok ()
    in
    let%bind _ =
      List.fold_result t.entries ~init:([], None) ~f:(fun (seen, birth) e ->
        let entry = e.Stored.value in
        if List.exists seen ~f:(P.Id.Recurrence.equal (E.id entry))
        then invalid "duplicate recurrence identity"
        else (
          let next_birth =
            match E.control entry with
            | Retired _ -> Ok birth
            | Enabled | Paused _ ->
              let current = F.birth_lifecycle_revision (E.current entry) in
              (match birth with
               | None -> Ok (Some current)
               | Some old ->
                 if P.Session_lifecycle.Revision.equal old current
                 then Ok birth
                 else invalid "nonretired recurrences span lifecycle epochs")
          in
          let%map birth = next_birth in
          E.id entry :: seen, birth))
    in
    let%bind _ =
      List.fold_result t.receipts ~init:[] ~f:(fun seen (r : R.t) ->
        let%bind entry =
          Result.of_option
            (find t r.recurrence_id)
            ~error:
              (P.Error.invalid_request "recurrence receipt refers to missing identity")
        in
        let definition =
          List.find (E.definitions entry) ~f:(fun d ->
            Int64.equal (I.Definition.revision (F.key d)) r.definition_revision)
        in
        let%bind definition =
          Result.of_option
            definition
            ~error:(P.Error.invalid_request "receipt definition is not retained")
        in
        let metadata_matches =
          match r.kind with
          | Create | Edit | Enable ->
            Option.equal P.Run_source.equal r.source (Some (F.source definition))
            && Option.equal
                 P.Session_lifecycle.Revision.equal
                 r.birth_lifecycle_revision
                 (Some (F.birth_lifecycle_revision definition))
          | Pause | Retire ->
            Option.is_none r.source && Option.is_none r.birth_lifecycle_revision
        in
        if
          List.exists seen ~f:(fun old ->
            same_key r old || Int64.equal r.catalog_revision old.R.catalog_revision)
          || Int64.(
               r.catalog_revision > t.revision
               || r.control_revision > E.control_revision entry)
          || not metadata_matches
        then invalid "inconsistent or duplicate recurrence receipt"
        else Ok (r :: seen))
    in
    validate_encoded_capacity t (raw_json t))
;;

let to_jsonaf t =
  let%map.Result () = validate_structure t in
  raw_json t
;;

let of_jsonaf json =
  let open Result.Let_syntax in
  let%bind () = D.Json.validate ~limits json |> domain in
  let%bind f = X.object_ json in
  let%bind revision =
    X.required f "revision" (function
      | `String text when String.length text <= 19 -> X.nonnegative_int64 (`String text)
      | _ -> invalid "invalid recurrence catalog revision")
  in
  let bounded_list bound decode = function
    | `Array xs when List.length xs <= bound -> List.map xs ~f:decode |> Result.all
    | _ -> full "recurrence catalog array exceeds retention bound"
  in
  let%bind entries = X.required f "entries" (bounded_list 16 Stored.of_json) in
  let%bind receipts =
    X.required
      f
      "receipts"
      (bounded_list 256 (fun json ->
         let%bind f = X.object_ json in
         let%bind id = X.required f "id" P.Json_codec.string in
         let%bind r = X.required f "receipt" R.of_json in
         if String.equal id (receipt_id r)
         then Ok r
         else invalid "recurrence receipt wrapper identity disagrees"))
  in
  let t = { revision; entries; receipts } in
  let%bind () = validate_structure t in
  let%map () = validate_encoded_capacity t json in
  t
;;

let sexp_of_t t = Jsonaf.sexp_of_t (raw_json t)

let t_of_sexp sexp =
  match of_jsonaf (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> raise_s [%sexp "invalid recurrence catalog", (error : P.Error.t)]
;;

let shape =
  X.shape_exn
    [ "revision", D.Shape.value
    ; "entries", X.array_shape_exn ~identity_field:"id" Stored.shape
    ; ( "receipts"
      , X.array_shape_exn
          ~identity_field:"id"
          (X.shape_exn [ "id", D.Shape.value; "receipt", R.shape ]) )
    ]
;;

let validate t ~session_id ~generation =
  let open Result.Let_syntax in
  let%bind () = validate_structure t in
  List.fold_result t.entries ~init:() ~f:(fun () e ->
    let entry = e.Stored.value in
    if
      P.Id.Session.equal (P.Session_ref.session_id (E.controller entry)) session_id
      && (F.source (E.current entry)).generation <= generation
    then Ok ()
    else invalid "recurrence catalog contains foreign or future definition")
;;

let same_entry a b = D.Json.equal a.Stored.encoded b.Stored.encoded

let unchanged_except_control previous incoming =
  List.equal
    (fun a b -> String.equal (F.digest a) (F.digest b))
    (E.definitions previous)
    (E.definitions incoming)
  && Recurrence_cursor.equal (E.cursor previous) (E.cursor incoming)
;;

let validate_command previous incoming (receipt : R.t) =
  let open Result.Let_syntax in
  let%bind entry =
    Result.of_option
      (find incoming receipt.recurrence_id)
      ~error:(P.Error.invalid_request "command receipt has no exact entry")
  in
  let%bind () =
    if
      Int64.equal receipt.catalog_revision incoming.revision
      && Int64.equal receipt.control_revision (E.control_revision entry)
      && Int64.equal receipt.definition_revision (definition_revision entry)
    then Ok ()
    else invalid "recurrence command receipt does not match exact candidate"
  in
  let%bind () =
    List.fold_result incoming.entries ~init:() ~f:(fun () candidate ->
      if
        Option.is_some (stored previous (E.id candidate.Stored.value))
        || (R.equal_kind receipt.kind Create
            && P.Id.Recurrence.equal (E.id candidate.value) receipt.recurrence_id)
      then Ok ()
      else invalid "new recurrence identity has no matching creation receipt")
  in
  let old = find previous receipt.recurrence_id in
  let valid =
    match receipt.kind, old with
    | Create, None ->
      Int64.equal (definition_revision entry) 0L
      && Int64.equal (E.control_revision entry) 0L
      && E.equal_control (E.control entry) (Paused Operator_pause)
      && P.Id.Principal.equal (F.creator (E.current entry)) receipt.principal_id
      && Recurrence_cursor.equal (E.cursor entry) (F.initial_cursor (E.current entry))
    | Edit, Some old ->
      Int64.(definition_revision old < max_value)
      && Int64.equal (definition_revision entry) (Int64.succ (definition_revision old))
      && E.equal_control (E.control entry) (Paused Operator_pause)
    | Enable, Some old ->
      unchanged_except_control old entry && E.equal_control (E.control entry) Enabled
    | Pause, Some old ->
      unchanged_except_control old entry
      && E.equal_control (E.control entry) (Paused Operator_pause)
    | Retire, Some old ->
      unchanged_except_control old entry
      &&
        (match E.control old, E.control entry with
        | Retired before, Retired after -> E.Retirement.equal before after
        | (Enabled | Paused _), Retired Operator_retire -> true
        | (Enabled | Paused _), (Enabled | Paused _ | Retired Lifecycle_changed)
        | Retired _, (Enabled | Paused _) -> false)
    | (Edit | Enable | Pause | Retire), None | Create, Some _ -> false
  in
  if not valid
  then invalid "recurrence command kind disagrees with exact transition"
  else
    List.fold_result previous.entries ~init:() ~f:(fun () old ->
      if P.Id.Recurrence.equal (E.id old.Stored.value) receipt.recurrence_id
      then Ok ()
      else (
        match stored incoming (E.id old.value) with
        | Some next when same_entry old next -> Ok ()
        | None | Some _ -> invalid "one recurrence command changed another identity"))
;;

(* Receipt-free changes are whole-owner dispositions. No arbitrary safe-looking
   subset can consume the frontier reserved for a later bulk pause/retirement. *)
let validate_internal previous incoming =
  let disposition reason =
    List.for_all previous.entries ~f:(fun old ->
      match stored incoming (E.id old.Stored.value) with
      | None -> false
      | Some next ->
        (match E.control old.value with
         | Paused _ | Retired _ -> same_entry old next
         | Enabled -> E.equal_control (E.control next.value) (Paused reason)))
  in
  let retirement =
    List.for_all previous.entries ~f:(fun old ->
      match stored incoming (E.id old.Stored.value) with
      | None -> false
      | Some next ->
        (match E.control old.value with
         | Retired _ -> same_entry old next
         | Enabled | Paused _ ->
           E.equal_control (E.control next.value) (Retired Lifecycle_changed)))
  in
  let due =
    let changed =
      List.filter previous.entries ~f:(fun old ->
        Option.for_all
          (stored incoming (E.id old.Stored.value))
          ~f:(fun next -> not (same_entry old next)))
    in
    match changed with
    | [ old ] ->
      (match stored incoming (E.id old.value) with
       | Some next ->
         E.equal_control (E.control old.value) Enabled
         && E.equal_control (E.control next.value) Enabled
         && Int64.equal (E.control_revision old.value) (E.control_revision next.value)
       | None -> false)
    | [] | _ :: _ :: _ -> false
  in
  if List.length previous.entries <> List.length incoming.entries
  then invalid "internal recurrence disposition changed identity inventory"
  else if due || disposition Controller_stop || disposition Source_review || retirement
  then Ok ()
  else invalid "receipt-free recurrence update is not a complete safety disposition"
;;

let validate_transition previous ~incoming =
  let open Result.Let_syntax in
  let%bind () = validate_structure incoming in
  let%bind () =
    List.fold_result previous.entries ~init:() ~f:(fun () old ->
      match stored incoming (E.id old.Stored.value) with
      | None -> invalid "retained recurrence identity cannot disappear"
      | Some next -> E.validate_transition old.value ~incoming:next.value)
  in
  let rec retained old next =
    match old, next with
    | [], rest -> Ok rest
    | a :: old, b :: next when R.equal a b -> retained old next
    | _ -> invalid "immutable recurrence command receipts disappeared or changed"
  in
  let%bind added = retained previous.receipts incoming.receipts in
  let%bind () =
    if D.Json.equal (raw_json previous) (raw_json incoming)
    then Ok ()
    else if
      Int64.equal previous.revision Int64.max_value
      || not (Int64.equal incoming.revision (Int64.succ previous.revision))
    then invalid "recurrence catalog must advance exactly once"
    else (
      match added with
      | [] -> validate_internal previous incoming
      | [ receipt ] ->
        if List.exists previous.receipts ~f:(same_key receipt)
        then invalid "recurrence command key is already retained"
        else validate_command previous incoming receipt
      | _ -> invalid "one recurrence metadata transition has multiple new receipts")
  in
  (* Stable array identity order is part of the initial retained-history policy. *)
  let old_ids = List.map previous.entries ~f:(fun e -> E.id e.Stored.value) in
  let next_ids =
    List.take incoming.entries (List.length old_ids)
    |> List.map ~f:(fun e -> E.id e.Stored.value)
  in
  if
    List.equal P.Id.Recurrence.equal old_ids next_ids
    && List.length incoming.entries <= List.length previous.entries + 1
  then Ok ()
  else invalid "recurrence identity history was reordered or expanded"
;;

let commit t ~entry ~(receipt : R.t) =
  let open Result.Let_syntax in
  if List.length t.receipts >= 256 || Int64.equal t.revision Int64.max_value
  then full "recurrence command receipt or revision capacity exhausted"
  else (
    let%bind next = Stored.create entry in
    let entries =
      match stored t (E.id entry) with
      | None -> t.entries @ [ next ]
      | Some _ ->
        List.map t.entries ~f:(fun old ->
          if P.Id.Recurrence.equal (E.id old.Stored.value) (E.id entry) then next else old)
    in
    let incoming =
      { revision = Int64.succ t.revision; entries; receipts = t.receipts @ [ receipt ] }
    in
    let%map () = validate_transition t ~incoming in
    incoming)
;;

let apply_due t ~recurrence_id plan =
  let open Result.Let_syntax in
  let%bind old =
    Result.of_option
      (find t recurrence_id)
      ~error:(P.Error.invalid_request "due recurrence identity is not retained")
  in
  let%bind entry = E.apply_due old plan in
  let%bind next = Stored.create entry in
  if Int64.equal t.revision Int64.max_value
  then full "recurrence due catalog frontier exhausted"
  else (
    let incoming =
      { t with
        revision = Int64.succ t.revision
      ; entries =
          List.map t.entries ~f:(fun old ->
            if P.Id.Recurrence.equal (E.id old.Stored.value) recurrence_id
            then next
            else old)
      }
    in
    let%map () = validate_transition t ~incoming in
    incoming)
;;

let reserve_child
      t
      ~recurrence_id
      ~principal_id
      ~child_session_id
      ~transaction_id
      ~occurrence
  =
  let open Result.Let_syntax in
  let%bind old =
    Result.of_option
      (find t recurrence_id)
      ~error:(P.Error.invalid_request "creation recurrence identity is not retained")
  in
  let%bind entry, row =
    E.reserve_child old ~principal_id ~child_session_id ~transaction_id ~occurrence
  in
  let%bind next = Stored.create entry in
  match stored t recurrence_id with
  | Some previous when same_entry previous next -> Ok (t, row)
  | Some _ | None ->
    if Int64.equal t.revision Int64.max_value
    then full "recurrence creation catalog frontier exhausted"
    else (
      let incoming =
        { t with
          revision = Int64.succ t.revision
        ; entries =
            List.map t.entries ~f:(fun previous ->
              if P.Id.Recurrence.equal (E.id previous.Stored.value) recurrence_id
              then next
              else previous)
        }
      in
      let%map () = validate_transition t ~incoming in
      incoming, row)
;;

let link_child t association =
  let open Result.Let_syntax in
  let recurrence_id =
    Recurrence_child_association.occurrence association
    |> Recurrence_identity.Occurrence.definition
    |> Recurrence_identity.Definition.recurrence_id
  in
  let%bind old =
    Result.of_option
      (find t recurrence_id)
      ~error:(P.Error.invalid_request "linked recurrence identity is not retained")
  in
  let%bind entry = E.link_child old association in
  let%bind next = Stored.create entry in
  match stored t recurrence_id with
  | Some previous when same_entry previous next -> Ok t
  | Some _ | None ->
    if Int64.equal t.revision Int64.max_value
    then full "recurrence linkage catalog frontier exhausted"
    else (
      let incoming =
        { t with
          revision = Int64.succ t.revision
        ; entries =
            List.map t.entries ~f:(fun previous ->
              if P.Id.Recurrence.equal (E.id previous.Stored.value) recurrence_id
              then next
              else previous)
        }
      in
      let%map () = validate_transition t ~incoming in
      incoming)
;;

let record_execution t ~intent ~attempt =
  let open Result.Let_syntax in
  let recurrence_id =
    Recurrence_execution_intent.occurrence intent
    |> Recurrence_identity.Occurrence.definition
    |> Recurrence_identity.Definition.recurrence_id
  in
  let%bind old =
    Result.of_option
      (find t recurrence_id)
      ~error:(P.Error.invalid_request "execution recurrence identity is not retained")
  in
  let%bind entry, row = E.record_execution old ~intent ~attempt in
  let%bind next = Stored.create entry in
  match stored t recurrence_id with
  | Some previous when same_entry previous next -> Ok (t, row)
  | Some _ | None ->
    if Int64.equal t.revision Int64.max_value
    then full "recurrence execution catalog frontier exhausted"
    else (
      let incoming =
        { t with
          revision = Int64.succ t.revision
        ; entries =
            List.map t.entries ~f:(fun previous ->
              if P.Id.Recurrence.equal (E.id previous.Stored.value) recurrence_id
              then next
              else previous)
        }
      in
      let%map () = validate_transition t ~incoming in
      incoming, row)
;;

let rotate_execution t ~intent ~previous ~resolution ~next =
  let open Result.Let_syntax in
  let recurrence_id =
    Recurrence_execution_intent.occurrence intent
    |> Recurrence_identity.Occurrence.definition
    |> Recurrence_identity.Definition.recurrence_id
  in
  let%bind old =
    Result.of_option
      (find t recurrence_id)
      ~error:(P.Error.invalid_request "retry recurrence identity is not retained")
  in
  let%bind entry, row = E.rotate_execution old ~intent ~previous ~resolution ~next in
  let%bind next = Stored.create entry in
  match stored t recurrence_id with
  | Some previous when same_entry previous next -> Ok (t, row)
  | Some _ | None ->
    if Int64.equal t.revision Int64.max_value
    then full "recurrence retry catalog frontier exhausted"
    else (
      let incoming =
        { t with
          revision = Int64.succ t.revision
        ; entries =
            List.map t.entries ~f:(fun previous ->
              if P.Id.Recurrence.equal (E.id previous.Stored.value) recurrence_id
              then next
              else previous)
        }
      in
      let%map () = validate_transition t ~incoming in
      incoming, row)
;;

let import_execution t ~origin ~terminal ~automatic =
  let open Result.Let_syntax in
  let recurrence_id =
    Recurrence_run_origin.occurrence origin
    |> Recurrence_identity.Occurrence.definition
    |> Recurrence_identity.Definition.recurrence_id
  in
  let%bind old =
    Result.of_option
      (find t recurrence_id)
      ~error:(P.Error.invalid_request "execution recurrence identity is not retained")
  in
  let%bind entry, row = E.import_execution old ~origin ~terminal ~automatic in
  let%bind next = Stored.create entry in
  match stored t recurrence_id with
  | Some previous when same_entry previous next -> Ok (t, row)
  | Some _ | None ->
    if Int64.equal t.revision Int64.max_value
    then full "recurrence execution catalog frontier exhausted"
    else (
      let incoming =
        { t with
          revision = Int64.succ t.revision
        ; entries =
            List.map t.entries ~f:(fun previous ->
              if P.Id.Recurrence.equal (E.id previous.Stored.value) recurrence_id
              then next
              else previous)
        }
      in
      let%map () = validate_transition t ~incoming in
      incoming, row)
;;

let disposition t ~f =
  let open Result.Let_syntax in
  let%bind changed, entries =
    List.fold_result t.entries ~init:(false, []) ~f:(fun (changed, acc) old ->
      let%bind next = f old.Stored.value in
      match next with
      | None -> Ok (changed, old :: acc)
      | Some next ->
        let%map next = Stored.create next in
        true, next :: acc)
  in
  if not changed
  then Ok None
  else if Int64.equal t.revision Int64.max_value
  then full "recurrence safety frontier exhausted"
  else (
    let incoming =
      { t with revision = Int64.succ t.revision; entries = List.rev entries }
    in
    let%map () = validate_transition t ~incoming in
    Some incoming)
;;

let pause_for_stop t =
  disposition t ~f:(fun entry ->
    match E.control entry with
    | Enabled -> Result.map (E.pause entry ~reason:Controller_stop) ~f:Option.some
    | Paused _ | Retired _ -> Ok None)
;;

let hold_for_source t = disposition t ~f:E.hold_for_review

let retire_stale_births t ~current_revision ~archived =
  disposition t ~f:(fun entry ->
    let%bind.Result effective = E.effective_control entry ~current_revision ~archived in
    match E.control entry, effective with
    | (Enabled | Paused _), Retired Lifecycle_changed -> E.retire_lifecycle entry
    | (Enabled | Paused _), (Enabled | Paused _) | Retired _, Retired _ -> Ok None
    | (Enabled | Paused _), Retired Operator_retire | Retired _, (Enabled | Paused _) ->
      invalid "inconsistent effective recurrence retirement")
;;

let capacity_projection t =
  let open Result.Let_syntax in
  let%bind reserve = reservation t in
  let%map entries =
    List.map t.entries ~f:(fun e ->
      let%map encoded = E.capacity_projection e.Stored.value |> domain in
      `Object [ "id", P.Id.Recurrence.to_json (E.id e.value); "document", encoded ])
    |> Result.all
  in
  `Object
    [ ( "revision"
      , X.int64_json
          (if reserve.required_transition_steps = 0 then t.revision else Int64.max_value)
      )
    ; "entries", `Array entries
    ; "receipts", `Array (List.map t.receipts ~f:receipt_json)
    ]
;;
