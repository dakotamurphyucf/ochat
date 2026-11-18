open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module R = Agent_store.Delegation_store
module O = Agent_store.Delegation_goal_origin
module Leaf = Policy_cause_leaf
module Key = Leaf.Key
module Caller_source = Policy_caller_source
module Goal_target = Leaf.Goal_target
module Member = Leaf.Member
module Allocation_work = Leaf.Allocation_work
module Coverage = Leaf.Coverage
module Bounds = Policy_forwarding.Bounds
module Seed = Policy_forwarding.Seed
module Forwarding = Policy_forwarding

let invalid = Leaf.invalid
let domain = Leaf.domain
let limits = Leaf.limits
let digest = Leaf.digest
let object_ = X.object_
let required = X.required
let session_shape = Leaf.session_shape
let source_shape = Leaf.source_shape
let origin_shape = Leaf.origin_shape
let key_for = Leaf.key_for
let validate_path = Leaf.validate_path

type t =
  { key : Key.t
  ; parent_event : P.Id.Moderator_execution.t
  ; callee : P.Session_ref.t
  ; callee_source : P.Run_source.t
  ; caller : P.Session_ref.t
  ; caller_source : Policy_caller_source.t
  ; caller_origin : P.Inference_origin.t option
  ; child_invocation : P.Id.Invocation.t
  ; admission_sha256 : string
  ; path : R.Reference.t list
  ; targets : Goal_target.t list
  ; max_inherited_scopes : int
  ; forwarding : Forwarding.t option
  ; raw : Jsonaf.t option
  }
[@@deriving sexp_of]

let key t = t.key
let parent_event t = t.parent_event
let callee t = t.callee
let callee_source t = t.callee_source
let caller t = t.caller
let caller_source t = t.caller_source
let caller_origin t = t.caller_origin
let child_invocation t = t.child_invocation
let admission_sha256 t = t.admission_sha256
let path t = t.path
let targets t = t.targets
let max_inherited_scopes t = t.max_inherited_scopes

let known_json t =
  `Object
    ([ "id", Key.to_json t.key
     ; "parent_event", P.Id.Moderator_execution.to_json t.parent_event
     ; "callee", P.Session_ref.to_json t.callee
     ; "callee_source", P.Run_source.to_json t.callee_source
     ; "caller", P.Session_ref.to_json t.caller
     ; "caller_source", Policy_caller_source.to_json t.caller_source
     ; "caller_origin", X.option_json P.Inference_origin.to_json t.caller_origin
     ; "child_invocation", P.Id.Invocation.to_json t.child_invocation
     ; "admission_sha256", `String t.admission_sha256
     ; "path", `Array (List.map t.path ~f:R.reference_to_jsonaf)
     ; "targets", `Array (List.map t.targets ~f:Goal_target.to_json)
     ; "max_inherited_scopes", X.host_counter_to_json t.max_inherited_scopes
     ]
     @ Option.to_list
         (Option.map t.forwarding ~f:(fun value ->
            "forwarding", Forwarding.known_json value)))
;;

(* A complete raw carrier needs no reconstruction of its known children. *)
let to_json t =
  match t.raw with
  | Some raw -> raw
  | None -> known_json t
;;

let equal a b = D.Json.equal (known_json a) (known_json b)
let forwarding t = t.forwarding

let seed t =
  let bounds = Bounds.create ~max_inherited_scopes:t.max_inherited_scopes in
  Result.bind bounds ~f:(fun bounds ->
    Seed.create
      ~key:t.key
      ~parent_event:t.parent_event
      ~caller:t.caller
      ~caller_source:t.caller_source
      ~caller_origin:t.caller_origin
      ~callee:t.callee
      ~callee_source:t.callee_source
      ~child_invocation:t.child_invocation
      ~admission_sha256:t.admission_sha256
      ~path:t.path
      ~bounds)
  |> Result.map_error ~f:(fun error -> error.P.Error.message)
  |> Result.ok_or_failwith
;;

let validate_direct_target t target =
  match Goal_target.view target with
  | Inherited _ -> Ok ()
  | Local { goal_id = _; episode } ->
    if
      P.Session_ref.equal (Goal_target.owner target) t.caller
      && Policy_caller_source.matches_installed
           t.caller_source
           (Goal_target.source target)
      && Option.equal
           P.Inference_origin.equal
           t.caller_origin
           (Some (Run (P.Goal_episode.admission episode).run_id))
    then Ok ()
    else invalid "caller local Goal episode/source/origin differs"
;;

let validate_targets t =
  let open Result.Let_syntax in
  let incoming = Option.value_map t.forwarding ~default:[] ~f:Forwarding.targets in
  let%bind () =
    match t.forwarding with
    | None -> Ok ()
    | Some forwarded ->
      let%bind bounds = Bounds.create ~max_inherited_scopes:t.max_inherited_scopes in
      let%bind () =
        if Bounds.equal bounds (Forwarding.bounds forwarded)
        then Ok ()
        else invalid "forwarded cause depth differs"
      in
      let%bind () =
        Forwarding.check_next
          forwarded
          ~caller:t.caller
          ~caller_source:t.caller_source
          ~callee:t.callee
          ~callee_source:t.callee_source
          ~path:t.path
      in
      List.fold_result incoming ~init:() ~f:(fun () target ->
        if List.exists t.targets ~f:(Goal_target.equal target)
        then Ok ()
        else invalid "forwarded cause omitted or rewrote incoming Goal")
  in
  let direct =
    List.filter t.targets ~f:(fun target ->
      not (List.exists incoming ~f:(Goal_target.equal target)))
  in
  let%bind () =
    if List.length direct <= t.max_inherited_scopes + 1
    then Ok ()
    else invalid "direct Goal introduction capacity exceeded"
  in
  let%bind () =
    List.fold_result direct ~init:() ~f:(fun () target -> validate_direct_target t target)
  in
  if
    List.count direct ~f:(fun target ->
      match Goal_target.view target with
      | Local _ -> true
      | Inherited _ -> false)
    <= 1
  then Ok ()
  else invalid "multiple direct local Goal introductions"
;;

let validate t =
  let open Result.Let_syntax in
  let%bind () = P.Run_source.validate t.callee_source in
  let%bind () =
    Option.value_map t.caller_origin ~default:(Ok ()) ~f:P.Inference_origin.validate
  in
  let%bind (_ : Bounds.t) = Bounds.create ~max_inherited_scopes:t.max_inherited_scopes in
  let%bind () =
    if
      Key.equal
        t.key
        (key_for
           ~parent_event:t.parent_event
           ~callee:t.callee
           ~generation:t.callee_source.generation)
      && (List.length t.targets
          <=
          match t.forwarding with
          | None -> t.max_inherited_scopes + 1
          | Some forwarded -> Bounds.max_forwarded_targets (Forwarding.bounds forwarded))
      && List.length t.path <= t.max_inherited_scopes
      && not (P.Session_ref.equal t.callee t.caller)
    then Ok ()
    else invalid "policy cause identity or scope bound differs"
  in
  let%bind (_ : Key.t) = Key.of_json (`String t.admission_sha256) in
  let%bind (_ : string option) =
    List.fold_result t.targets ~init:None ~f:(fun previous target ->
      let root = Goal_target.root target in
      match previous with
      | Some previous when String.compare previous root >= 0 ->
        invalid "governing Goal vector is not strictly normalized"
      | None | Some _ -> Ok (Some root))
  in
  let%bind () = validate_targets t in
  let%bind () =
    validate_path
      ~caller:t.caller
      ~callee:t.callee
      ~callee_source:t.callee_source
      ~path:t.path
  in
  let%bind () = D.Json.validate (to_json t) ~limits |> domain in
  Ok ()
;;

let matches_event t (event : P.Moderator_execution.t) =
  let c = event.context in
  P.Id.Moderator_execution.equal t.parent_event c.id
  && P.Id.Session.equal (P.Session_ref.session_id t.callee) c.session_id
  && Int.equal t.callee_source.generation c.generation
  && P.Moderator_observer.equal t.callee_source.observer c.source
  && P.Moderator_execution.equal_phase c.phase Pre_tool_call
  && Option.is_none c.operation_id
  && Option.is_none c.job
  && Option.exists event.delegation ~f:(fun delegation ->
    P.Id.Session.equal delegation.child_session_id (P.Session_ref.session_id t.caller)
    && Int.equal
         delegation.child_generation
         (Policy_caller_source.generation t.caller_source)
    && P.Id.Invocation.equal delegation.child_invocation_id t.child_invocation
    && String.equal delegation.admission_sha256 t.admission_sha256)
;;

let create
      ~parent
      ~callee
      ~callee_source
      ~caller
      ~caller_source
      ~caller_origin
      ~path
      ~targets
      ~max_inherited_scopes
  =
  let targets =
    List.sort targets ~compare:(fun left right ->
      String.compare (Goal_target.root left) (Goal_target.root right))
  in
  match parent.P.Moderator_execution.delegation, parent.status with
  | Some delegation, Running ->
    let t =
      { key =
          key_for
            ~parent_event:parent.context.id
            ~callee
            ~generation:callee_source.P.Run_source.generation
      ; parent_event = parent.context.id
      ; callee
      ; callee_source
      ; caller
      ; caller_source
      ; caller_origin
      ; child_invocation = delegation.child_invocation_id
      ; admission_sha256 = delegation.admission_sha256
      ; path
      ; targets
      ; max_inherited_scopes
      ; forwarding = None
      ; raw = None
      }
    in
    let%bind.Result () = validate t in
    if matches_event t parent
    then Ok t
    else invalid "policy cause differs from actual delegated Event"
  | (None | Some _), (Completed _ | Failed _ | Interrupted _) | None, Running ->
    invalid "policy cause requires a new Running delegated Event"
;;

let validate_raw_targets forwarding fields =
  let open Result.Let_syntax in
  match forwarding with
  | None -> Ok ()
  | Some forwarded ->
    let%bind raw_targets = required fields "targets" (X.list (fun raw -> Ok raw)) in
    let%bind candidates =
      List.map raw_targets ~f:(fun raw ->
        let%map value = Goal_target.of_json raw in
        value, raw)
      |> Result.all
    in
    List.fold_result (Forwarding.targets forwarded) ~init:() ~f:(fun () target ->
      let%bind witness = Forwarding.for_target forwarded ~target in
      let%bind intro = object_ (Forwarding.Witness.to_json witness) in
      let%bind retained = required intro "target" (fun raw -> Ok raw) in
      match
        List.find candidates ~f:(fun (value, _) -> Goal_target.equal value target)
      with
      | Some (_, raw) when D.Json.equal retained raw -> Ok ()
      | Some _ | None ->
        invalid "forwarded cause target raw conflicts with retained introduction")
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind () = D.Json.validate raw ~limits |> domain in
  let%bind f = object_ raw in
  let%bind key = required f "id" Key.of_json in
  let%bind parent_event = required f "parent_event" P.Id.Moderator_execution.of_json in
  let%bind callee = required f "callee" P.Session_ref.of_json in
  let%bind callee_source = required f "callee_source" P.Run_source.of_json in
  let%bind caller = required f "caller" P.Session_ref.of_json in
  let%bind caller_source = required f "caller_source" Policy_caller_source.of_json in
  let%bind caller_origin =
    required f "caller_origin" (X.nullable P.Inference_origin.of_json)
  in
  let%bind child_invocation = required f "child_invocation" P.Id.Invocation.of_json in
  let%bind admission_sha256 = required f "admission_sha256" P.Json_codec.string in
  let%bind path =
    required
      f
      "path"
      (X.list (fun raw ->
         R.reference_of_jsonaf raw
         |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error))
  in
  let%bind targets = required f "targets" (X.list Goal_target.of_json) in
  let%bind max_inherited_scopes =
    required f "max_inherited_scopes" X.host_counter_of_json
  in
  let%bind forwarding =
    match P.Json_codec.optional f "forwarding" with
    | None -> Ok None
    | Some value -> Result.map (Forwarding.of_json value) ~f:Option.some
  in
  let t =
    { key
    ; parent_event
    ; callee
    ; callee_source
    ; caller
    ; caller_source
    ; caller_origin
    ; child_invocation
    ; admission_sha256
    ; path
    ; targets
    ; max_inherited_scopes
    ; forwarding
    ; raw = Some raw
    }
  in
  let%bind () = validate t in
  let%map () = validate_raw_targets forwarding f in
  t
;;

let shape =
  X.shape_exn
    [ "id", D.Shape.value
    ; "parent_event", D.Shape.value
    ; "callee", session_shape
    ; "callee_source", source_shape
    ; "caller", session_shape
    ; "caller_source", Policy_caller_source.shape
    ; "caller_origin", origin_shape
    ; "child_invocation", D.Shape.value
    ; "admission_sha256", D.Shape.value
    ; "path", X.array_shape_exn R.reference_shape
    ; "targets", X.array_shape_exn Goal_target.shape
    ; "max_inherited_scopes", D.Shape.value
    ; "forwarding", Forwarding.shape
    ]
;;

module Source_child = struct
  type cause = t

  type t =
    { value : cause
    ; raw : Jsonaf.t
    }

  let of_json raw =
    let%map.Result value = of_json raw in
    { value; raw }
  ;;

  let value t = t.value
  let raw t = t.raw
end

let child_field fields name = required fields name (fun raw -> Ok raw)

let append_forwarding child ~work ~member_projection_sha256 =
  let open Result.Let_syntax in
  let value = Source_child.value child in
  let%bind fields = object_ (Source_child.raw child) in
  let selected =
    [ "id"
    ; "parent_event"
    ; "caller"
    ; "caller_source"
    ; "caller_origin"
    ; "callee"
    ; "callee_source"
    ; "child_invocation"
    ; "admission_sha256"
    ; "path"
    ]
  in
  let raw_seed =
    `Object
      (List.filter (P.Json_codec.to_alist fields) ~f:(fun (key, _) ->
         List.mem selected key ~equal:String.equal)
       @ [ "bounds", Bounds.to_json (seed value).Seed.bounds ])
  in
  let%bind raw_targets = required fields "targets" (X.list (fun raw -> Ok raw)) in
  let%bind targets =
    List.map raw_targets ~f:(fun raw ->
      let%map target = Goal_target.of_json raw in
      target, raw)
    |> Result.all
  in
  Forwarding.append
    value.forwarding
    ~seed:(seed value)
    ~raw_seed
    ~targets
    ~work
    ~member_projection_sha256
    ~cause_known_sha256:(digest (known_json value))
;;

let create_forwarded
      ~parent
      ~callee
      ~callee_source
      ~caller
      ~caller_source
      ~caller_origin
      ~path
      ~direct_targets
      ~forwarded
      ~max_inherited_scopes
  =
  let open Result.Let_syntax in
  (* Existing direct constructor validates actual immediate Event, source, route
     and each direct local descriptor before any incoming alias is normalized. *)
  let%bind direct =
    create
      ~parent
      ~callee
      ~callee_source
      ~caller
      ~caller_source
      ~caller_origin
      ~path
      ~targets:direct_targets
      ~max_inherited_scopes
  in
  let incoming = Forwarding.targets forwarded in
  let%bind targets =
    List.fold_result direct_targets ~init:incoming ~f:(fun targets target ->
      match
        List.find targets ~f:(fun existing ->
          String.equal (Goal_target.root existing) (Goal_target.root target))
      with
      | None -> Ok (target :: targets)
      | Some existing when Goal_target.equal existing target -> Ok targets
      | Some existing ->
        (match Goal_target.view existing, Goal_target.view target with
         | Inherited _, Local _
           when P.Session_ref.equal (Goal_target.owner existing) caller -> Ok targets
         | (Local _ | Inherited _), (Local _ | Inherited _) ->
           invalid "incompatible governing Goal branch descriptors"))
  in
  let targets =
    List.sort targets ~compare:(fun a b ->
      String.compare (Goal_target.root a) (Goal_target.root b))
  in
  let candidate = { direct with targets; forwarding = Some forwarded; raw = None } in
  let%bind () = validate candidate in
  let%bind target_children =
    List.map targets ~f:(fun target ->
      if List.exists incoming ~f:(Goal_target.equal target)
      then (
        let%bind witness = Forwarding.for_target forwarded ~target in
        let%bind fields = object_ (Forwarding.Witness.to_json witness) in
        child_field fields "target")
      else Ok (Goal_target.to_json target))
    |> Result.all
  in
  let%bind fields = object_ (known_json candidate) in
  let raw =
    `Object
      (List.map (P.Json_codec.to_alist fields) ~f:(fun (key, value) ->
         ( key
         , if String.equal key "forwarding"
           then Forwarding.to_json forwarded
           else if String.equal key "targets"
           then `Array target_children
           else value )))
  in
  of_json raw
;;

module Context = struct
  type t =
    { session_id : P.Id.Session.t
    ; generation : int
    ; current_source : P.Run_source.t option
    ; events : P.Moderator_execution.t list
    ; invocations : P.Invocation.t list
    ; jobs : P.Job.t list
    ; runs : Run_state.t option
    ; accounts : Goal_accounts.t option
    ; catalog : Goal_catalog.t option
    ; descendants : Goal_descendants.t option
    }

  let create
        ~session_id
        ~generation
        ~current_source
        ~events
        ~invocations
        ~jobs
        ~runs
        ~accounts
        ~catalog
        ~descendants
    =
    if
      generation < 0
      || Option.exists current_source ~f:(fun source ->
        not (Int.equal generation source.P.Run_source.generation))
    then invalid "policy context generation/source differs"
    else
      let open Result.Let_syntax in
      let%bind () =
        Option.value_map current_source ~default:(Ok ()) ~f:P.Run_source.validate
      in
      let unique ids ~compare = Option.is_none (List.find_a_dup ids ~compare) in
      let%bind () =
        if
          unique
            (List.map events ~f:(fun row -> row.P.Moderator_execution.context.id))
            ~compare:P.Id.Moderator_execution.compare
          && unique
               (List.map invocations ~f:(fun row -> row.P.Invocation.context.id))
               ~compare:P.Id.Invocation.compare
          && unique (List.map jobs ~f:(fun row -> row.P.Job.id)) ~compare:P.Id.Job.compare
        then Ok ()
        else invalid "duplicate policy source-work identity"
      in
      let belongs id row_generation =
        P.Id.Session.equal session_id id
        && row_generation >= 0
        && row_generation <= generation
      in
      if
        List.for_all events ~f:(fun row ->
          belongs row.P.Moderator_execution.context.session_id row.context.generation)
        && List.for_all invocations ~f:(fun row ->
          belongs row.P.Invocation.context.session_id row.context.generation)
        && List.for_all jobs ~f:(fun row -> belongs row.P.Job.session_id row.generation)
      then
        Ok
          { session_id
          ; generation
          ; current_source
          ; events
          ; invocations
          ; jobs
          ; runs
          ; accounts
          ; catalog
          ; descendants
          }
      else invalid "policy source-work session/generation differs"
  ;;
end
