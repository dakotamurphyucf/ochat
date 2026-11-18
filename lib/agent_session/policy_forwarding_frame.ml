open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module R = Agent_store.Delegation_store
module L = Policy_cause_leaf
module Target = L.Goal_target

let invalid = L.invalid
let required = X.required
let object_ = X.object_

module Bounds = struct
  type t =
    { max_inherited_scopes : int
    ; max_direct_targets : int
    ; max_forwarded_targets : int
    }
  [@@deriving equal, sexp_of]

  let create ~max_inherited_scopes =
    let d = max_inherited_scopes in
    if d < 0 || d = Int.max_value
    then invalid "policy forwarding capacity overflow"
    else (
      let direct = d + 1 in
      if direct > Int.max_value / direct
      then invalid "policy forwarding capacity overflow"
      else
        Ok
          { max_inherited_scopes = d
          ; max_direct_targets = direct
          ; max_forwarded_targets = direct * direct
          })
  ;;

  let max_inherited_scopes t = t.max_inherited_scopes
  let max_direct_targets t = t.max_direct_targets
  let max_forwarded_targets t = t.max_forwarded_targets

  let to_json t =
    `Object [ "max_inherited_scopes", X.host_counter_to_json t.max_inherited_scopes ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind fields = object_ raw in
    let%bind max_inherited_scopes =
      required fields "max_inherited_scopes" X.host_counter_of_json
    in
    create ~max_inherited_scopes
  ;;

  let shape = X.fields_shape [ "max_inherited_scopes" ]
end

module Seed = struct
  type t =
    { key : L.Key.t
    ; parent_event : P.Id.Moderator_execution.t
    ; caller : P.Session_ref.t
    ; caller_source : Policy_caller_source.t
    ; caller_origin : P.Inference_origin.t option
    ; callee : P.Session_ref.t
    ; callee_source : P.Run_source.t
    ; child_invocation : P.Id.Invocation.t
    ; admission_sha256 : string
    ; path : R.Reference.t list
    ; bounds : Bounds.t
    }
  [@@deriving equal, sexp_of]

  let key t = t.key
  let caller t = t.caller
  let caller_source t = t.caller_source
  let callee t = t.callee
  let callee_source t = t.callee_source

  let to_json t =
    `Object
      [ "id", L.Key.to_json t.key
      ; "parent_event", P.Id.Moderator_execution.to_json t.parent_event
      ; "caller", P.Session_ref.to_json t.caller
      ; "caller_source", Policy_caller_source.to_json t.caller_source
      ; "caller_origin", X.option_json P.Inference_origin.to_json t.caller_origin
      ; "callee", P.Session_ref.to_json t.callee
      ; "callee_source", P.Run_source.to_json t.callee_source
      ; "child_invocation", P.Id.Invocation.to_json t.child_invocation
      ; "admission_sha256", `String t.admission_sha256
      ; "path", `Array (List.map t.path ~f:R.reference_to_jsonaf)
      ; "bounds", Bounds.to_json t.bounds
      ]
  ;;

  let validate t =
    let open Result.Let_syntax in
    let%bind () = P.Run_source.validate t.callee_source in
    let%bind () =
      Option.value_map t.caller_origin ~default:(Ok ()) ~f:P.Inference_origin.validate
    in
    let%bind _ = L.Key.of_json (`String t.admission_sha256) in
    let%bind () =
      if
        L.Key.equal
          t.key
          (L.key_for
             ~parent_event:t.parent_event
             ~callee:t.callee
             ~generation:t.callee_source.generation)
        && List.length t.path <= Bounds.max_inherited_scopes t.bounds
      then Ok ()
      else invalid "forwarded seed identity or path capacity differs"
    in
    L.validate_path
      ~caller:t.caller
      ~callee:t.callee
      ~callee_source:t.callee_source
      ~path:t.path
  ;;

  let create
        ~key
        ~parent_event
        ~caller
        ~caller_source
        ~caller_origin
        ~callee
        ~callee_source
        ~child_invocation
        ~admission_sha256
        ~path
        ~bounds
    =
    let t =
      { key
      ; parent_event
      ; caller
      ; caller_source
      ; caller_origin
      ; callee
      ; callee_source
      ; child_invocation
      ; admission_sha256
      ; path
      ; bounds
      }
    in
    let%map.Result () = validate t in
    t
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = object_ raw in
    let%bind key = required f "id" L.Key.of_json in
    let%bind parent_event = required f "parent_event" P.Id.Moderator_execution.of_json in
    let%bind caller = required f "caller" P.Session_ref.of_json in
    let%bind caller_source = required f "caller_source" Policy_caller_source.of_json in
    let%bind caller_origin =
      required f "caller_origin" (X.nullable P.Inference_origin.of_json)
    in
    let%bind callee = required f "callee" P.Session_ref.of_json in
    let%bind callee_source = required f "callee_source" P.Run_source.of_json in
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
    let%bind bounds = required f "bounds" Bounds.of_json in
    create
      ~key
      ~parent_event
      ~caller
      ~caller_source
      ~caller_origin
      ~callee
      ~callee_source
      ~child_invocation
      ~admission_sha256
      ~path
      ~bounds
  ;;

  let shape =
    X.shape_exn
      [ "id", D.Shape.value
      ; "parent_event", D.Shape.value
      ; "caller", L.session_shape
      ; "caller_source", Policy_caller_source.shape
      ; "caller_origin", L.origin_shape
      ; "callee", L.session_shape
      ; "callee_source", L.source_shape
      ; "child_invocation", D.Shape.value
      ; "admission_sha256", D.Shape.value
      ; "path", X.array_shape_exn R.reference_shape
      ; "bounds", Bounds.shape
      ]
  ;;
end

module Frame = struct
  type t =
    { frame_sha256 : string
    ; seed : Seed.t
    ; work : L.Allocation_work.t
    ; member_projection_sha256 : string
    ; cause_known_sha256 : string
    ; raw : Jsonaf.t
    }

  let frame_sha256 t = t.frame_sha256
  let seed t = t.seed
  let work t = t.work

  (* A keyed frame must identify ALL its immutable known occurrence coordinates.
     Otherwise a new Work/attempt could inherit an old frame's unknown children. *)
  let body_json ~seed ~work ~member_projection_sha256 ~cause_known_sha256 =
    `Object
      [ "seed", Seed.to_json seed
      ; "work", L.Allocation_work.to_json work
      ; "member_projection_sha256", `String member_projection_sha256
      ; "cause_known_sha256", `String cause_known_sha256
      ]
  ;;

  let known_json t =
    match
      body_json
        ~seed:t.seed
        ~work:t.work
        ~member_projection_sha256:t.member_projection_sha256
        ~cause_known_sha256:t.cause_known_sha256
    with
    | `Object fields -> `Object (("frame_sha256", `String t.frame_sha256) :: fields)
    | _ -> assert false
  ;;

  let to_json t = t.raw

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = object_ raw in
    let%bind frame_sha256 =
      required f "frame_sha256" L.Key.of_json |> Result.map ~f:L.Key.to_string
    in
    let%bind seed = required f "seed" Seed.of_json in
    let%bind work = required f "work" L.Allocation_work.of_json in
    let%bind member_projection_sha256 =
      required f "member_projection_sha256" P.Json_codec.string
    in
    let%bind cause_known_sha256 = required f "cause_known_sha256" P.Json_codec.string in
    let%bind _ = L.Key.of_json (`String member_projection_sha256) in
    let%bind _ = L.Key.of_json (`String cause_known_sha256) in
    let%bind _ =
      L.Allocation_work.member work ~generation:seed.callee_source.generation
    in
    let%bind () =
      if
        String.equal
          frame_sha256
          (L.digest (body_json ~seed ~work ~member_projection_sha256 ~cause_known_sha256))
      then Ok ()
      else invalid "forwarded Frame identity differs from complete known body"
    in
    Ok { frame_sha256; seed; work; member_projection_sha256; cause_known_sha256; raw }
  ;;

  let create ~seed ~work ~member_projection_sha256 ~cause_known_sha256 ~raw_seed =
    let frame_sha256 =
      L.digest (body_json ~seed ~work ~member_projection_sha256 ~cause_known_sha256)
    in
    of_json
      (`Object
          [ "frame_sha256", `String frame_sha256
          ; "seed", raw_seed
          ; "work", L.Allocation_work.to_json work
          ; "member_projection_sha256", `String member_projection_sha256
          ; "cause_known_sha256", `String cause_known_sha256
          ])
    |> Result.bind ~f:(fun frame ->
      if Seed.equal seed frame.seed
      then Ok frame
      else invalid "source seed raw differs from known seed")
  ;;

  let shape =
    X.shape_exn
      [ "frame_sha256", D.Shape.value
      ; "seed", Seed.shape
      ; "work", L.Allocation_work.shape
      ; "member_projection_sha256", D.Shape.value
      ; "cause_known_sha256", D.Shape.value
      ]
  ;;
end

let validate_chain ~bounds seeds =
  let open Result.Let_syntax in
  let%bind () =
    if List.is_empty seeds || List.length seeds > Bounds.max_inherited_scopes bounds
    then invalid "forwarding frame capacity differs"
    else Ok ()
  in
  let%bind _ =
    List.fold_result seeds ~init:None ~f:(fun previous (seed : Seed.t) ->
      let%bind () =
        if Bounds.equal seed.bounds bounds
        then Ok ()
        else invalid "forwarding captured depth differs"
      in
      let%bind () =
        match previous with
        | None -> Ok ()
        | Some (previous : Seed.t) ->
          if
            P.Session_ref.equal previous.callee seed.caller
            && Policy_caller_source.matches_installed
                 seed.caller_source
                 previous.callee_source
          then Ok ()
          else invalid "forwarding source-qualified legs are discontinuous"
      in
      Ok (Some seed))
  in
  let first = List.hd_exn seeds
  and last = List.last_exn seeds in
  let path = List.concat_map seeds ~f:(fun (seed : Seed.t) -> seed.path) in
  let%bind () =
    if List.length path <= Bounds.max_inherited_scopes bounds
    then Ok ()
    else invalid "forwarding route exceeds captured depth"
  in
  L.validate_path
    ~caller:first.caller
    ~callee:last.callee
    ~callee_source:last.callee_source
    ~path
;;

let validate_local target (seed : Seed.t) =
  match Target.view target with
  | Inherited _ -> Ok ()
  | Local { goal_id = _; episode } ->
    if
      P.Session_ref.equal (Target.owner target) seed.caller
      && Policy_caller_source.matches_installed seed.caller_source (Target.source target)
      && Option.equal
           P.Inference_origin.equal
           seed.caller_origin
           (Some (Run (P.Goal_episode.admission episode).run_id))
    then Ok ()
    else invalid "forwarded local target introduction differs"
;;

let validate_frames ~bounds frames =
  let open Result.Let_syntax in
  let%bind () = validate_chain ~bounds (List.map frames ~f:Frame.seed) in
  if List.contains_dup (List.map frames ~f:Frame.frame_sha256) ~compare:String.compare
  then invalid "forwarded immutable Frame identities are duplicated"
  else Ok ()
;;
