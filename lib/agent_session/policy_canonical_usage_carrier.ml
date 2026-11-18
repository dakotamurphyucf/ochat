open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module C = D.Extension_carrier
module Data = Policy_canonical_usage_data
module Cause = Policy_cause_data
module Member = Cause.Member
module Work = Cause.Allocation_work
module Target = Cause.Goal_target
module Coverage = Cause.Coverage
module Context = Cause.Context
module L = Inference_ledger
module F = Canonical_usage_fold
module Canonical = Goal_descendants.Canonical
module Occurrence = Goal_descendant_occurrence
module U = Goal_usage

let invalid = Data.invalid
let domain = Data.domain
let limits = Data.limits
let max_causes = Data.max_causes
let max_memberships = Data.max_memberships
let max_streams = Data.max_streams
let max_bindings = Data.max_bindings

module Choice = Data.Choice
module Binding = Data.Binding
module Stream = Data.Stream

let codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.policy_canonical_usage"
    ~shape:Data.shape
    ~decode:Data.of_json
    ~encode:(fun value ->
      let%map.Result () = Data.validate value in
      Data.to_json value)
;;

let probe_codec =
  X.codec_exn
    ~version:1
    ~limits
    ~kind:"ochat.policy_canonical_usage"
    ~shape:Data.shape
    ~decode:(fun raw -> Ok raw)
    ~encode:(fun raw -> Ok raw)
;;

type t = Data.t C.t

let value (t : t) : Data.t = C.value t
let bindings t = (value t).Data.bindings
let encoded t = D.Domain_codec.encode codec t |> domain

let equal_encoded left right =
  let open Result.Let_syntax in
  let%bind left = encoded left in
  let%map right = encoded right in
  D.Json.equal (D.Document.json left) (D.Document.json right)
;;

let marks ledger = F.Marks.of_ledger ledger |> Data.fold

let same_ledger left right =
  let open Result.Let_syntax in
  let%bind left = L.to_document left |> Data.ledger in
  let%map right = L.to_document right |> Data.ledger in
  D.Json.equal (D.Document.json left) (D.Document.json right)
;;

let replace raw key replacement =
  match raw with
  | `Object fields ->
    `Object
      (List.map fields ~f:(fun (name, old) ->
         name, if String.equal name key then replacement else old))
  | _ -> assert false
;;

let partition_projection (row : Data.Partition.t) =
  let raw = Data.Partition.to_json row in
  let raw = replace raw "settled" (U.Persistence.capacity_projection row.settled) in
  let raw = replace raw "admitted_attempts" (X.int64_json Int64.max_value) in
  match row.head with
  | None -> raw
  | Some head ->
    let canonical = Canonical.Fact.to_json head.canonical in
    let occurrence =
      Occurrence.capacity_projection_for (Canonical.Fact.occurrence head.canonical)
    in
    let head_raw = Data.Head.to_json head in
    replace
      raw
      "head"
      (replace head_raw "canonical" (replace canonical "occurrence" occurrence))
;;

let projection t =
  let raw = Data.to_json (value t) in
  let raw = replace raw "ledger_revision" (X.int64_json Int64.max_value) in
  let maximum : F.Marks.t =
    { attempt = Int64.max_value
    ; turn = Int64.max_value
    ; untracked_attempts = Int64.max_value
    ; untracked_turns = Int64.max_value
    }
  in
  let raw = replace raw "marks" (Data.marks_json maximum) in
  replace raw "streams" (`Array (List.map (value t).streams ~f:partition_projection))
;;

(* One immutable pair per capacity operation. The current document is the
   exact complete export of [source], including retained unknown children. Only
   local callers supply it, immediately after [encoded source]. *)
module Capacity_documents = struct
  type carrier = t

  type t =
    { source : carrier
    ; current : D.Document.t
    ; future : D.Document.t
    }

  let create source ~current =
    let open Result.Let_syntax in
    let projected = projection source in
    let%bind carrier = D.Domain_codec.decode probe_codec current |> domain in
    let%map future =
      D.Domain_codec.encode probe_codec (C.with_value carrier projected) |> domain
    in
    { source; current; future }
  ;;

  let prepare source =
    let%bind.Result current = encoded source in
    create source ~current
  ;;

  (* Source envelopes are immutable parts of Stream/Frame identity. Their actual
     bytes/fields/nodes/depth, including the Installed wrapper, are charged by
     these complete current/future documents, not known-only source sizes. *)
  let reserved_growth t =
    let open Result.Let_syntax in
    let%bind before = Capacity_growth.measure (D.Document.json t.current) ~limits in
    let%bind after = Capacity_growth.measure (D.Document.json t.future) ~limits in
    let growth = Capacity_growth.positive_difference after ~previous:before in
    (* A future head can name another genuine work family. Reserve the largest
     complete known work form independently; compatible unknown children remain
     in their current carrier and can still prohibit a semantic kind change.
     Model and Async attempt tags have equal encoded length and child shape. *)
    let maximum_work =
      `Object
        [ "kind", `String "model_job_attempt"
        ; "id", `String (String.make 96 'x')
        ; "attempt", X.host_counter_to_json Int.max_value
        ]
    in
    let%bind maximum = Capacity_growth.measure maximum_work ~limits in
    List.fold_result (value t.source).streams ~init:growth ~f:(fun growth row ->
      match row.Data.Partition.head with
      | None -> invalid "policy stream head absent"
      | Some head ->
        let%bind current = Capacity_growth.measure (Work.to_json head.work) ~limits in
        Capacity_growth.add
          growth
          (Capacity_growth.positive_difference maximum ~previous:current))
  ;;

  let required_depth t = Capacity_depth.measure (D.Document.json t.future) ~limits

  let validate t raw =
    let open Result.Let_syntax in
    let%bind growth = reserved_growth t in
    let%bind remaining = Capacity_growth.remaining_limits growth limits in
    let%bind () = D.Json.validate raw ~limits:remaining |> domain in
    (* Keep depth measurement after growth and the actual supplied raw check. *)
    let%bind depth = required_depth t in
    Capacity_depth.validate depth ~ancestors:0 ~limits
  ;;
end

let reserved_growth t =
  let%bind.Result documents = Capacity_documents.prepare t in
  Capacity_documents.reserved_growth documents
;;

let required_depth t =
  let%bind.Result documents = Capacity_documents.prepare t in
  Capacity_documents.required_depth documents
;;

let validate_encoded_capacity t raw =
  let%bind.Result documents = Capacity_documents.prepare t in
  Capacity_documents.validate documents raw
;;

(* Preflight original child bytes before the structured carrier projects them.
   Enclosing bounds alone do not enforce the smaller descriptor profile. *)
let validate_original_children raw =
  let open Result.Let_syntax in
  let%bind fields = X.object_ raw in
  let%bind () =
    X.required
      fields
      "causes"
      (X.list (fun raw -> Cause.Source_child.of_json raw |> Result.map ~f:ignore))
    |> Result.map ~f:ignore
  in
  X.required
    fields
    "streams"
    (X.list (fun raw ->
       let%bind fields = X.object_ raw in
       X.required fields "stream" (fun stream ->
         Stream.of_json stream |> Result.map ~f:ignore)))
  |> Result.map ~f:ignore
;;

let raw_identity fields array_name identity =
  let open Result.Let_syntax in
  let%bind rows = X.required fields array_name (X.list X.raw) in
  Result.of_option
    (List.find rows ~f:(fun raw ->
       match D.Json.field raw ~name:"id" with
       | Value (`String actual) -> String.equal identity actual
       | Absent | Null | Value _ -> false))
    ~error:(P.Error.invalid_request "policy complete child identity absent")
;;

let projection_raw
      accounting
      document
      (partition : Data.Partition.t)
      ~session_revision
      ~usage
  =
  let open Result.Let_syntax in
  let%bind fields = X.object_ (D.Document.payload document) in
  let%bind raw_partition = raw_identity fields "streams" partition.stream.id in
  let%bind partition_fields = X.object_ raw_partition in
  let%bind stream = X.required partition_fields "stream" X.raw in
  let%bind head =
    Result.of_option
      partition.head
      ~error:(P.Error.invalid_request "policy projection has no head")
  in
  let%bind cause =
    Result.of_option
      (Data.find_cause (value accounting) head.cause)
      ~error:(P.Error.invalid_request "policy projection head cause absent")
  in
  let%bind raw_cause = raw_identity fields "causes" (Cause.Key.to_string head.cause) in
  let%bind cause_fields = X.object_ raw_cause in
  let%bind parent_event = X.required cause_fields "parent_event" X.raw in
  let%bind caller_origin = X.required cause_fields "caller_origin" X.raw in
  let%bind child_invocation = X.required cause_fields "child_invocation" X.raw in
  let%bind admission_sha256 = X.required cause_fields "admission_sha256" X.raw in
  let%bind source_child = Cause.Source_child.of_json raw_cause in
  let full_cause = Cause.Source_child.value source_child in
  let%bind forwarding =
    match partition.stream.forwarding_route, Cause.forwarding full_cause with
    | None, _ -> Ok None
    | Some route, Some forwarded ->
      let%bind witness =
        Cause.Forwarding.for_target forwarded ~target:partition.stream.target
      in
      if Cause.Forwarding.Route.equal route (Cause.Forwarding.Witness.route witness)
      then Ok (Some witness)
      else invalid "policy raw projection forwarding route differs"
    | Some _, None -> invalid "policy raw projection forwarding cause absent"
  in
  let witness =
    `Object
      ([ "cause", Cause.Key.to_json (Cause.key cause)
       ; "parent_event", parent_event
       ; "caller_origin", caller_origin
       ; "child_invocation", child_invocation
       ; "admission_sha256", admission_sha256
       ]
       @ Option.to_list
           (Option.map forwarding ~f:(fun witness ->
              "forwarding", Cause.Forwarding.Witness.to_json witness)))
  in
  let%map head_raw = X.required partition_fields "head" X.raw in
  let head_raw =
    match head_raw with
    | `Object fields -> `Object (fields @ [ "witness", witness ])
    | _ -> assert false
  in
  `Object
    [ "session_revision", X.int64_json session_revision
    ; "stream", stream
    ; "admitted_attempts", X.int64_json partition.admitted_attempts
    ; "usage", usage
    ; "head", head_raw
    ]
;;

let witness_shape =
  X.shape_exn
    [ "cause", D.Shape.value
    ; "parent_event", D.Shape.value
    ; ( "caller_origin"
      , D.Shape.nullable
          (X.tagged_shape_exn
             ~discriminator:"type"
             [ "unbound", X.fields_shape [ "schema_version"; "type" ]
             ; "run", X.fields_shape [ "schema_version"; "type"; "run_id" ]
             ]) )
    ; "child_invocation", D.Shape.value
    ; "admission_sha256", D.Shape.value
    ; "forwarding", Cause.Forwarding.Witness.shape
    ]
;;

let head_shape =
  X.shape_exn
    [ "cause", D.Shape.value
    ; "work", Work.shape
    ; "canonical", Canonical.Fact.shape
    ; "witness", witness_shape
    ]
;;

let projection_shape =
  X.shape_exn
    [ "session_revision", D.Shape.value
    ; "stream", Stream.shape
    ; "admitted_attempts", D.Shape.value
    ; "usage", U.Persistence.shape
    ; "head", head_shape
    ]
;;

let projection_probe_codec =
  X.codec_exn
    ~version:1
    ~limits:Data.projection_limits
    ~kind:"ochat.policy_usage_projection"
    ~shape:projection_shape
    ~decode:(fun raw -> Ok raw)
    ~encode:(fun raw -> Ok raw)
;;

let projection_future raw (partition : Data.Partition.t) =
  let max_id = `String (String.make 96 'x') in
  let raw = replace raw "session_revision" (X.int64_json Int64.max_value) in
  let raw = replace raw "admitted_attempts" (X.int64_json Int64.max_value) in
  let raw = replace raw "usage" (U.Persistence.capacity_projection partition.settled) in
  match D.Json.field raw ~name:"head", partition.head with
  | Value head, Some known ->
    (* The equal-length Async attempt has this same maximum child shape. *)
    let head =
      replace
        head
        "work"
        (`Object
            [ "kind", `String "model_job_attempt"
            ; "id", max_id
            ; "attempt", X.host_counter_to_json Int.max_value
            ])
    in
    let canonical = Canonical.Fact.to_json known.canonical in
    let canonical =
      replace
        canonical
        "occurrence"
        (Occurrence.capacity_projection_for (Canonical.Fact.occurrence known.canonical))
    in
    let head = replace head "canonical" canonical in
    let witness =
      match D.Json.field head ~name:"witness" with
      | Value witness -> witness
      | Absent | Null -> assert false
    in
    let witness = replace witness "parent_event" max_id in
    let witness = replace witness "child_invocation" max_id in
    let witness =
      replace
        witness
        "caller_origin"
        (`Object
            [ "schema_version", `Number "1"; "type", `String "run"; "run_id", max_id ])
    in
    replace raw "head" (replace head "witness" witness)
  | Absent, _ | Null, _ | Value _, None -> assert false
;;

let validate_projections_capacity accounting document =
  List.fold_result (value accounting).streams ~init:() ~f:(fun () partition ->
    let open Result.Let_syntax in
    let%bind raw =
      projection_raw
        accounting
        document
        partition
        ~session_revision:Int64.max_value
        ~usage:(U.Persistence.capacity_projection partition.settled)
    in
    let%bind document =
      D.Document.create
        ~limits:Data.projection_limits
        ~kind:"ochat.policy_usage_projection"
        ~version:1
        ~payload:raw
      |> domain
    in
    let%bind probe = D.Domain_codec.decode projection_probe_codec document |> domain in
    let future = projection_future (C.value probe) partition in
    let%map (_ : D.Document.t) =
      D.Domain_codec.encode projection_probe_codec (C.with_value probe future) |> domain
    in
    ())
;;

module Export = struct
  type source = t

  type t =
    { document : D.Document.t
    ; capacity : Capacity_documents.t
    }

  let prepare (source : source) =
    let open Result.Let_syntax in
    let%bind document = encoded source in
    let%bind () = validate_original_children (D.Document.payload document) in
    let%bind () = validate_projections_capacity source document in
    let%bind capacity = Capacity_documents.create source ~current:document in
    let%map () = Capacity_documents.validate capacity (D.Document.json document) in
    { document; capacity }
  ;;

  let document t = t.document
  let capacity t = t.capacity
end

let to_document t = Result.map (Export.prepare t) ~f:Export.document

let of_document document =
  let open Result.Let_syntax in
  let%bind () = validate_original_children (D.Document.payload document) in
  let%bind t = D.Domain_codec.decode codec document |> domain in
  let%bind () = validate_encoded_capacity t (D.Document.json document) in
  let%map (_ : D.Document.t) = to_document t in
  t
;;

let admit t =
  let%map.Result (_ : D.Document.t) = to_document t in
  t
;;

let with_value t data = admit (C.with_value t data)

let adopt t ~incoming =
  let open Result.Let_syntax in
  if not (Data.equal (value t) (value incoming))
  then invalid "policy movement requires a closed Update"
  else (
    let%bind candidate = D.Domain_codec.adopt codec ~previous:t ~incoming |> domain in
    admit candidate)
;;

let exact_current t ~(context : Context.t) ~ledger =
  let open Result.Let_syntax in
  let data = value t in
  let%bind () = Data.validate data in
  let%bind () =
    L.validate
      ledger
      ~limits:L.Limits.default
      ~session_id:context.session_id
      ~generation:context.generation
    |> Data.ledger
  in
  let%bind actual_marks = marks ledger in
  if
    (not (P.Id.Session.equal context.session_id (P.Session_ref.session_id data.callee)))
    || (not (Int64.equal data.ledger_revision (L.revision ledger)))
    || not (F.Marks.equal actual_marks data.marks)
  then invalid "policy component differs from actual canonical frontier"
  else Ok ()
;;

let validate t ~context ~ledger =
  let open Result.Let_syntax in
  let data = value t in
  let%bind () = exact_current t ~context ~ledger in
  let%bind () =
    List.fold_result data.memberships ~init:() ~f:(fun () row ->
      let%bind cause =
        Result.of_option
          (Data.find_cause data row.Data.Membership.cause)
          ~error:(P.Error.invalid_request "policy member cause absent")
      in
      let%bind current =
        Data.Source_work.capture ~memberships:data.memberships context cause row.member
      in
      match current with
      | None -> Ok ()
      | Some current ->
        if Data.Source_work.matches row current
        then Ok ()
        else invalid "policy immutable source-work witness changed")
  in
  List.fold_result data.bindings ~init:() ~f:(fun () binding ->
    match L.find ledger ~ordinal:binding.Data.Binding.ordinal with
    | None -> invalid "policy binding lost canonical row before folding"
    | Some row ->
      let h = L.Row.handle row in
      let occurrence = Canonical.Fact.occurrence binding.canonical in
      let frame = Occurrence.frame occurrence in
      (match Occurrence.kind occurrence, Canonical.Fact.tracking binding.canonical with
       | Attempt { ordinal; operation_id; invocation_id }, L.Tracked ->
         if
           Int64.equal ordinal (L.Handle.ordinal h)
           && P.Id.Session.equal
                (P.Session_ref.session_id (Occurrence.Frame.session frame))
                (L.Handle.session_id h)
           && Int.equal (Occurrence.Frame.generation frame) (L.Handle.generation h)
           && Option.equal P.Id.Operation.equal operation_id (L.Handle.operation_id h)
           && Option.equal P.Id.Invocation.equal invocation_id (L.Handle.invocation_id h)
         then Ok ()
         else invalid "policy binding canonical identity changed"
       | Host_turn _, _ | Attempt _, Untracked _ ->
         invalid "policy binding is not a retained Attempt"))
;;

let protected_ordinals t ~ledger =
  let open Result.Let_syntax in
  let%bind original = encoded t in
  let%bind carrier = D.Domain_codec.decode probe_codec original |> domain in
  List.fold_result (value t).bindings ~init:Int64.Set.empty ~f:(fun protected binding ->
    let ordinal = binding.Data.Binding.ordinal in
    let%bind () =
      if Option.is_some (L.find ledger ~ordinal)
      then Ok ()
      else invalid "policy protected binding row absent"
    in
    let raw = Data.to_json (value t) in
    let raw =
      replace
        raw
        "bindings"
        (`Array
            (List.filter_map (value t).bindings ~f:(fun row ->
               if Int64.equal row.Data.Binding.ordinal ordinal
               then None
               else Some (Data.Binding.to_json row))))
    in
    let raw =
      replace
        raw
        "streams"
        (`Array
            (List.map (value t).streams ~f:(fun row ->
               let current = Data.Partition.to_json row in
               if Data.selected (value t) binding row.Data.Partition.stream
               then
                 replace current "settled" (U.Persistence.capacity_projection row.settled)
               else current)))
    in
    match D.Domain_codec.encode probe_codec (C.with_value carrier raw) with
    | Error (D.Error.Extension_conflict _) -> Ok (Set.add protected ordinal)
    | Error error -> domain (Error error)
    | Ok _ -> Ok protected)
;;

(* Copy complete foreign children only through the SAME existing domain codec.
   Owned Data.to_json is known-only; the resulting carrier must have exactly the
   planned native value. Existing adopt enforces protected extension conflicts. *)
let retain_children candidate ~cause =
  let open Result.Let_syntax in
  let%bind document = to_document candidate in
  let payload = D.Document.payload document in
  let%bind fields = X.object_ payload in
  let%bind causes = X.required fields "causes" (X.list X.raw) in
  let causes =
    match cause with
    | None -> causes
    | Some cause ->
      List.map causes ~f:(fun raw ->
        match D.Json.field raw ~name:"id" with
        | Value (`String id) when String.equal id (Cause.Key.to_string (Cause.key cause))
          -> Cause.to_json cause
        | Absent | Null | Value _ -> raw)
  in
  let payload = replace payload "causes" (`Array causes) in
  let%bind fields = X.object_ payload in
  let%bind streams = X.required fields "streams" (X.list X.raw) in
  let%bind streams =
    List.map streams ~f:(fun raw ->
      let%bind f = X.object_ raw in
      let%bind id = X.required f "id" P.Json_codec.string in
      let%bind partition =
        List.find (value candidate).streams ~f:(fun row ->
          String.equal row.Data.Partition.stream.id id)
        |> Result.of_option
             ~error:(P.Error.invalid_request "raw stream has no planned native partition")
      in
      let%bind head =
        Result.of_option
          partition.head
          ~error:(P.Error.invalid_request "raw stream has no planned Head")
      in
      let%bind raw_cause =
        raw_identity fields "causes" (Cause.Key.to_string head.cause)
      in
      let%bind full_cause = Cause.of_json raw_cause in
      let stream = Stream.of_cause full_cause partition.stream.target in
      let%bind () =
        if Stream.equal stream partition.stream
        then Ok ()
        else invalid "copied raw stream changes known planned descriptor"
      in
      Ok (replace raw "stream" (Stream.to_json stream)))
    |> Result.all
  in
  let payload = replace payload "streams" (`Array streams) in
  let raw = replace (D.Document.json document) "payload" payload in
  let%bind document = D.Document.inspect raw ~limits |> domain in
  let%bind incoming = of_document document in
  adopt candidate ~incoming
;;
