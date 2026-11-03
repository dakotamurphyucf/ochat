open! Core
module P = Agent_protocol
module G = P.Goal
module D = Document_schema
module X = Persistence_codec
module R = Goal_command_receipt

let invalid message = Error (P.Error.invalid_request message)

let domain result =
  Result.map_error result ~f:(fun error ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t error)))
;;

let full message = Error (P.Error.create Resource_limit ~message ~retryable:false ())

let limits =
  match
    D.Limits.create
      ~max_bytes:(8 * 1024 * 1024)
      ~max_depth:72
      ~max_fields:140000
      ~max_nodes:280000
  with
  | Ok limits -> limits
  | Error error -> raise_s [%sexp "invalid catalog limits", (error : D.Error.t)]
;;

module Entry = struct
  type t =
    { goal : Goal_document.t
    ; encoded : Jsonaf.t
    }

  let value t = Goal_document.value t.goal

  let create goal =
    let%map.Result document = Goal_document.to_document goal |> domain in
    { goal; encoded = D.Document.json document }
  ;;

  let of_json json =
    let open Result.Let_syntax in
    let%bind f = X.object_ json in
    let%bind id = X.required f "id" P.Id.Goal.of_json in
    let%bind raw = X.required f "document" X.raw in
    let%bind document = D.Document.inspect raw ~limits:Goal_document.limits |> domain in
    let%bind goal = Goal_document.of_document document |> domain in
    if P.Id.Goal.equal id (G.id (Goal_document.value goal))
    then Ok { goal; encoded = raw }
    else invalid "catalog goal wrapper identity disagrees"
  ;;

  let to_json t =
    `Object [ "id", P.Id.Goal.to_json (G.id (value t)); "document", t.encoded ]
  ;;

  let shape = X.shape_exn [ "id", D.Shape.value; "document", D.Shape.value ]
end

type t =
  { revision : int64
  ; entries : Entry.t list
  ; receipts : R.t list
  }

let empty = { revision = 0L; entries = []; receipts = [] }
let revision t = t.revision
let goals t = List.map t.entries ~f:Entry.value
let receipts t = t.receipts

let entry t id =
  List.find t.entries ~f:(fun e -> P.Id.Goal.equal (G.id (Entry.value e)) id)
;;

let find t id = Option.map (entry t id) ~f:Entry.value
let document t id = Option.map (entry t id) ~f:(fun e -> e.Entry.goal)

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
           ~message:"goal command key has another input"
           ~retryable:false
           ())
;;

let receipt_id r =
  Jsonaf.to_string
    (`Array [ P.Id.Principal.to_json r.R.principal_id; P.Idempotency_key.to_json r.key ])
;;

let receipt_json r = `Object [ "id", `String (receipt_id r); "receipt", R.to_json r ]

let raw_json t =
  `Object
    [ "revision", X.int64_json t.revision
    ; "goals", `Array (List.map t.entries ~f:Entry.to_json)
    ; "receipts", `Array (List.map t.receipts ~f:receipt_json)
    ]
;;

let validate_capacity t raw ~limits =
  let open Result.Let_syntax in
  let%bind bytes = D.Json.validate_and_measure ~limits raw |> domain in
  let rec counts = function
    | `Object fields ->
      List.fold
        fields
        ~init:(List.length fields, 1)
        ~f:(fun (f, n) (_, json) ->
          let cf, cn = counts json in
          f + cf, n + cn)
    | `Array values ->
      List.fold values ~init:(0, 1) ~f:(fun (f, n) json ->
        let cf, cn = counts json in
        f + cf, n + cn)
    | `Null | `True | `False | `Number _ | `String _ -> 0, 1
  in
  let fields, nodes = counts raw in
  let%bind bytes, fields, nodes =
    List.fold_result
      t.entries
      ~init:(bytes + 19 - String.length (Int64.to_string t.revision), fields, nodes)
      ~f:(fun (bytes, fields, nodes) e ->
        let goal = Entry.value e in
        let%bind current = G.Capacity.measure (G.to_json goal) in
        let%bind reserved = G.reserved_capacity goal in
        let%map held =
          match G.state goal with
          | Open Enabled ->
            let%bind held = G.hold goal G.Hold.Source_review in
            let%bind document = Goal_document.update e.goal held |> domain in
            let%bind _ = Goal_document.to_document document |> domain in
            G.Capacity.measure (G.to_json held)
          | Open (Held _) | Closed _ -> Ok current
        in
        let growth get = Int.max 0 (Int.max (get reserved) (get held) - get current) in
        ( bytes + growth G.Capacity.encoded_bytes
        , fields + growth G.Capacity.fields
        , nodes + growth G.Capacity.nodes ))
  in
  if
    bytes > D.Limits.max_bytes limits
    || fields > D.Limits.max_fields limits
    || nodes > D.Limits.max_nodes limits
  then full "goal catalog lacks reserved child settlement capacity"
  else Ok ()
;;

let validate_encoded_capacity t raw = validate_capacity t raw ~limits
let validate_host_capacity t raw ~limits = validate_capacity t raw ~limits

let validate_structure t =
  let open Result.Let_syntax in
  if Int64.(t.revision < 0L) || List.length t.entries > 16 || List.length t.receipts > 256
  then full "goal catalog or retained command receipt capacity exhausted"
  else if
    List.count t.entries ~f:(fun e ->
      match G.state (Entry.value e) with
      | Open Enabled -> true
      | Open (Held _) | Closed _ -> false)
    > 1
  then invalid "only one foreground goal may be enabled"
  else (
    let%bind _ =
      List.fold_result t.entries ~init:[] ~f:(fun seen e ->
        let id = G.id (Entry.value e) in
        if List.exists seen ~f:(P.Id.Goal.equal id)
        then invalid "duplicate goal identity"
        else Ok (id :: seen))
    in
    let%bind _ =
      List.fold_result t.receipts ~init:[] ~f:(fun seen r ->
        let%bind goal =
          Result.of_option
            (find t r.R.goal_id)
            ~error:(P.Error.invalid_request "goal receipt refers to missing goal")
        in
        if
          List.exists seen ~f:(fun old ->
            same_key r old || Int64.equal r.catalog_revision old.R.catalog_revision)
          || Int64.(r.catalog_revision > t.revision || r.goal_revision > G.revision goal)
        then invalid "duplicate or inconsistent goal receipt"
        else Ok (r :: seen))
    in
    validate_encoded_capacity t (raw_json t))
;;

let to_jsonaf t =
  let%map.Result () = validate_structure t in
  raw_json t
;;

let of_jsonaf raw =
  let open Result.Let_syntax in
  let%bind () = D.Json.validate ~limits raw |> domain in
  let%bind f = X.object_ raw in
  let%bind revision = X.required f "revision" X.nonnegative_int64 in
  let bounded_list max decode = function
    | `Array xs when List.length xs <= max -> List.map xs ~f:decode |> Result.all
    | _ -> full "goal catalog array bound exceeded"
  in
  let%bind entries = X.required f "goals" (bounded_list 16 Entry.of_json) in
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
         else invalid "goal receipt wrapper identity disagrees"))
  in
  let t = { revision; entries; receipts } in
  let%bind () = validate_structure t in
  let%map () = validate_encoded_capacity t raw in
  t
;;

let sexp_of_t t = Jsonaf.sexp_of_t (raw_json t)

let t_of_sexp sexp =
  match of_jsonaf (Jsonaf.t_of_sexp sexp) with
  | Ok t -> t
  | Error error -> raise_s [%sexp "invalid goal catalog", (error : P.Error.t)]
;;

let shape =
  X.shape_exn
    [ "revision", D.Shape.value
    ; "goals", X.array_shape_exn ~identity_field:"id" Entry.shape
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
    let goal = Entry.value e in
    if
      P.Id.Session.equal (P.Session_ref.session_id (G.session goal)) session_id
      && (G.source goal).generation <= generation
    then Ok ()
    else invalid "goal catalog contains foreign or future goal")
;;

let validate_initial_goal goal ~(receipt : R.t) ~catalog_revision =
  if
    Int64.equal (G.revision goal) 0L
    && P.Id.Goal.equal (G.id goal) receipt.goal_id
    && P.Id.Principal.equal (G.creator goal) receipt.principal_id
    && Int64.equal receipt.goal_revision 0L
    && Int64.equal receipt.catalog_revision catalog_revision
    && Option.equal P.Run_source.equal receipt.source (Some (G.source goal))
    &&
    match G.state goal with
    | Open (Held Source_review) -> true
    | Open (Held _) | Open Enabled | Closed _ -> false
  then Ok ()
  else invalid "new goal must retain its held initial provenance and command receipt"
;;

let validate_transition t ~incoming =
  let open Result.Let_syntax in
  let%bind () = validate_structure incoming in
  let%bind () =
    List.fold_result t.entries ~init:() ~f:(fun () old ->
      match entry incoming (G.id (Entry.value old)) with
      | None -> invalid "goal evidence cannot be discarded"
      | Some next ->
        let%bind adopted = Goal_document.adopt old.goal ~incoming:next.goal |> domain in
        let%bind encoded = Goal_document.to_document adopted |> domain in
        if D.Json.equal (D.Document.json encoded) next.encoded
        then Ok ()
        else invalid "goal update discarded protected fields")
  in
  let%bind () =
    List.fold_result t.receipts ~init:() ~f:(fun () old ->
      if List.exists incoming.receipts ~f:(R.equal old)
      then Ok ()
      else invalid "goal command receipt is immutable")
  in
  let%bind () =
    List.fold_result incoming.entries ~init:() ~f:(fun () next ->
      let goal = Entry.value next in
      match entry t (G.id goal) with
      | Some _ -> Ok ()
      | None ->
        let%bind receipt =
          Result.of_option
            (List.find incoming.receipts ~f:(fun receipt ->
               P.Id.Goal.equal receipt.R.goal_id (G.id goal)
               && Int64.equal receipt.catalog_revision incoming.revision
               && not (List.exists t.receipts ~f:(same_key receipt))))
            ~error:(P.Error.invalid_request "new goal has no new command receipt")
        in
        validate_initial_goal goal ~receipt ~catalog_revision:incoming.revision)
  in
  if D.Json.equal (raw_json t) (raw_json incoming)
  then Ok ()
  else if
    Int64.equal t.revision Int64.max_value
    || not (Int64.equal incoming.revision (Int64.succ t.revision))
  then invalid "goal catalog revision must advance exactly once"
  else Ok ()
;;

let commit t ~goal ~(receipt : R.t) =
  let open Result.Let_syntax in
  let value = Goal_document.value goal in
  let%bind () =
    if List.length t.receipts >= 256 || Int64.equal t.revision Int64.max_value
    then
      full
        "goal command receipt capacity exhausted; existing controls require a retained \
         command key or session stop"
    else if
      (not (P.Id.Goal.equal receipt.goal_id (G.id value)))
      || (not (Int64.equal receipt.goal_revision (G.revision value)))
      || (not (Int64.equal receipt.catalog_revision (Int64.succ t.revision)))
      || List.exists t.receipts ~f:(same_key receipt)
    then invalid "goal command receipt does not match exact transition"
    else Ok ()
  in
  let%bind goal =
    match entry t (G.id value) with
    | Some old -> Goal_document.adopt old.goal ~incoming:goal |> domain
    | None ->
      if List.length t.entries >= 16
      then full "goal catalog is full; retained goals cannot be evicted"
      else (
        let%map () =
          validate_initial_goal value ~receipt ~catalog_revision:(Int64.succ t.revision)
        in
        goal)
  in
  let%bind e = Entry.create goal in
  let entries =
    match entry t (G.id value) with
    | None -> t.entries @ [ e ]
    | Some _ ->
      List.map t.entries ~f:(fun old ->
        if P.Id.Goal.equal (G.id (Entry.value old)) (G.id value) then e else old)
  in
  let incoming =
    { revision = Int64.succ t.revision; entries; receipts = t.receipts @ [ receipt ] }
  in
  let%map () = validate_transition t ~incoming in
  incoming
;;

let hold_for_review t =
  let open Result.Let_syntax in
  let changed =
    List.exists t.entries ~f:(fun e ->
      match G.state (Entry.value e) with
      | Open Enabled -> true
      | Open (Held _) | Closed _ -> false)
  in
  if not changed
  then Ok None
  else if Int64.equal t.revision Int64.max_value
  then full "goal catalog revision exhausted during hold"
  else (
    let%bind entries =
      List.map t.entries ~f:(fun e ->
        match G.state (Entry.value e) with
        | Open Enabled ->
          let%bind value = G.hold (Entry.value e) G.Hold.Source_review in
          let%bind goal = Goal_document.update e.goal value |> domain in
          Entry.create goal
        | Open (Held _) | Closed _ -> Ok e)
      |> Result.all
    in
    let incoming = { t with revision = Int64.succ t.revision; entries } in
    let%map () = validate_transition t ~incoming in
    Some incoming)
;;
