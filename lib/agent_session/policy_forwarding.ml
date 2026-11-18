open! Core
module P = Agent_protocol
module D = Document_schema
module X = Persistence_codec
module R = Agent_store.Delegation_store
module L = Policy_cause_leaf
module Target = L.Goal_target

let invalid = L.invalid
let domain = L.domain
let required = X.required
let object_ = X.object_
let json_equal = D.Json.equal
let digest = L.digest

let raw_child raw name =
  let%bind.Result fields = object_ raw in
  required fields name (fun child -> Ok child)
;;

let checked raw = D.Json.validate raw ~limits:L.limits |> domain

module F = Policy_forwarding_frame
module Bounds = F.Bounds
module Seed = F.Seed
module Frame = F.Frame

let validate_frames = F.validate_frames
let validate_local = F.validate_local

module Route = Policy_forwarding_route.Route
module Witness = Policy_forwarding_route.Witness

module Introduction = struct
  type t =
    { target : Target.t
    ; introduced_at : int
    ; raw : Jsonaf.t
    }

  let known_json t =
    `Object
      [ "target", Target.to_json t.target
      ; "introduced_at", X.host_counter_to_json t.introduced_at
      ]
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind f = object_ raw in
    let%bind target = required f "target" Target.of_json in
    let%map introduced_at = required f "introduced_at" X.host_counter_of_json in
    { target; introduced_at; raw }
  ;;

  let shape = X.shape_exn [ "target", Target.shape; "introduced_at", D.Shape.value ]
end

type t =
  { bounds : Bounds.t
  ; frames : Frame.t list
  ; introductions : Introduction.t list
  ; raw : Jsonaf.t
  }

let targets t =
  List.map t.introductions ~f:(fun introduction -> introduction.Introduction.target)
;;

let frames t = t.frames
let issuing_session t = (Frame.seed (List.last_exn t.frames)).callee
let issuing_source t = (Frame.seed (List.last_exn t.frames)).callee_source

let known_json t =
  `Object
    [ "bounds", Bounds.to_json t.bounds
    ; "frames", `Array (List.map t.frames ~f:Frame.known_json)
    ; "introductions", `Array (List.map t.introductions ~f:Introduction.known_json)
    ]
;;

let to_json t = t.raw
let equal a b = json_equal (known_json a) (known_json b)
let sexp_of_t t = [%sexp (known_json t : Jsonaf.t)]
let bounds t = t.bounds

let shape =
  X.shape_exn
    [ "bounds", Bounds.shape
    ; "frames", X.array_shape_exn ~identity_field:"frame_sha256" Frame.shape
    ; "introductions", X.array_shape_exn Introduction.shape
    ]
;;

let of_json raw =
  let open Result.Let_syntax in
  let%bind () = checked raw in
  let%bind f = object_ raw in
  let%bind bounds = required f "bounds" Bounds.of_json in
  let%bind frames = required f "frames" (X.list Frame.of_json) in
  let%bind introductions = required f "introductions" (X.list Introduction.of_json) in
  let%bind () = validate_frames ~bounds frames in
  let%bind () =
    if List.length introductions <= Bounds.max_forwarded_targets bounds
    then Ok ()
    else invalid "forwarded Goal capacity exceeded"
  in
  let%bind _ =
    List.fold_result introductions ~init:None ~f:(fun previous introduction ->
      let root = Target.root introduction.Introduction.target in
      match previous with
      | Some prior when String.compare prior root >= 0 ->
        invalid "forwarded Goal roots are not strictly normalized"
      | None | Some _ -> Ok (Some root))
  in
  let%bind () =
    List.fold_result introductions ~init:() ~f:(fun () introduction ->
      match List.nth frames introduction.Introduction.introduced_at with
      | None -> invalid "forwarded target introduction is outside frames"
      | Some frame -> validate_local introduction.target (Frame.seed frame))
  in
  let%bind () =
    List.fold_result
      (List.mapi frames ~f:(fun i frame -> i, frame))
      ~init:()
      ~f:(fun () (index, _) ->
        let bucket =
          List.filter introductions ~f:(fun introduction ->
            Int.equal introduction.Introduction.introduced_at index)
        in
        if
          List.length bucket <= Bounds.max_direct_targets bounds
          && List.count bucket ~f:(fun introduction ->
               match Target.view introduction.Introduction.target with
               | Local _ -> true
               | Inherited _ -> false)
             <= 1
        then Ok ()
        else invalid "forwarded introduction bucket exceeds direct bounds")
  in
  Ok { bounds; frames; introductions; raw }
;;

let for_target t ~target =
  let open Result.Let_syntax in
  let%bind introduction =
    List.find t.introductions ~f:(fun introduction ->
      Target.equal introduction.Introduction.target target)
    |> Result.of_option ~error:(P.Error.invalid_request "forwarded target is absent")
  in
  let frames = List.drop t.frames introduction.introduced_at in
  let%bind target_raw = raw_child introduction.raw "target" in
  let%bind bounds_raw = raw_child t.raw "bounds" in
  Witness.of_json
    (`Object
        [ "bounds", bounds_raw
        ; "target", target_raw
        ; "frames", `Array (List.map frames ~f:Frame.to_json)
        ])
;;

let check_next t ~caller ~caller_source ~callee ~callee_source ~path =
  let open Result.Let_syntax in
  let%bind () =
    if
      P.Session_ref.equal (issuing_session t) caller
      && Policy_caller_source.matches_installed caller_source (issuing_source t)
    then Ok ()
    else invalid "forwarding issuer differs from immediate caller"
  in
  let%bind () = L.validate_path ~caller ~callee ~callee_source ~path in
  let first = Frame.seed (List.hd_exn t.frames) in
  let all = List.concat_map t.frames ~f:(fun frame -> (Frame.seed frame).path) @ path in
  let%bind () =
    if List.length all <= Bounds.max_inherited_scopes t.bounds
    then Ok ()
    else invalid "forwarding next route exceeds captured depth"
  in
  L.validate_path ~caller:first.caller ~callee ~callee_source ~path:all
;;

(* Private constructor input: Cause.Source_child supplies these projections from
   one validated actual child, never separately from a caller-selected prefix. *)
let append
      previous
      ~seed
      ~raw_seed
      ~targets
      ~work
      ~member_projection_sha256
      ~cause_known_sha256
  =
  let open Result.Let_syntax in
  let%bind (_ : string option) =
    List.fold_result targets ~init:None ~f:(fun previous (target, _) ->
      let root = Target.root target in
      match previous with
      | Some prior when String.compare prior root >= 0 ->
        invalid "source target roots are not strictly normalized"
      | None | Some _ -> Ok (Some root))
  in
  let%bind () =
    List.fold_result targets ~init:() ~f:(fun () (target, raw) ->
      let%bind decoded = Target.of_json raw in
      if Target.equal target decoded
      then Ok ()
      else invalid "source target raw differs from known target")
  in
  let%bind () =
    match previous with
    | None ->
      if List.length targets <= Bounds.max_direct_targets seed.Seed.bounds
      then Ok ()
      else invalid "direct source target bound exceeded"
    | Some previous ->
      let%bind () =
        if Bounds.equal previous.bounds seed.Seed.bounds
        then Ok ()
        else invalid "forwarding depth changed"
      in
      let%bind () =
        check_next
          previous
          ~caller:seed.caller
          ~caller_source:seed.caller_source
          ~callee:seed.callee
          ~callee_source:seed.callee_source
          ~path:seed.path
      in
      List.fold_result previous.introductions ~init:() ~f:(fun () intro ->
        match
          List.find targets ~f:(fun (target, _) ->
            Target.equal intro.Introduction.target target)
        with
        | None -> invalid "source cause omitted or rewrote an incoming target"
        | Some (_, raw) ->
          let%bind retained = raw_child intro.raw "target" in
          if json_equal retained raw
          then Ok ()
          else invalid "source target raw conflicts with retained introduction")
  in
  let%bind bounds_raw =
    match previous with
    | None -> raw_child raw_seed "bounds"
    | Some prior -> raw_child prior.raw "bounds"
  in
  let%bind frame =
    Frame.create ~seed ~raw_seed ~work ~member_projection_sha256 ~cause_known_sha256
  in
  let prior_frames, prior_introductions =
    match previous with
    | None -> [], []
    | Some prior -> prior.frames, prior.introductions
  in
  let added =
    List.filter targets ~f:(fun (target, _) ->
      not
        (List.exists prior_introductions ~f:(fun intro ->
           String.equal (Target.root intro.Introduction.target) (Target.root target))))
  in
  let%bind () =
    if List.length added <= Bounds.max_direct_targets seed.bounds
    then Ok ()
    else invalid "new target bucket exceeds direct bound"
  in
  let introductions =
    List.map prior_introductions ~f:(fun intro -> intro.Introduction.target, intro.raw)
    @ List.map added ~f:(fun (target, raw) ->
      ( target
      , `Object
          [ "target", raw
          ; "introduced_at", X.host_counter_to_json (List.length prior_frames)
          ] ))
    |> List.sort ~compare:(fun (a, _) (b, _) ->
      String.compare (Target.root a) (Target.root b))
  in
  let changed =
    [ "bounds", bounds_raw
    ; "frames", `Array (List.map (prior_frames @ [ frame ]) ~f:Frame.to_json)
    ; "introductions", `Array (List.map introductions ~f:snd)
    ]
  in
  let%bind retained =
    match previous with
    | None -> Ok []
    | Some { raw = `Object fields; _ } -> Ok fields
    | Some _ -> invalid "forwarding raw wrapper is not an object"
  in
  let raw =
    `Object
      (List.filter retained ~f:(fun (key, _) ->
         not (List.Assoc.mem changed key ~equal:String.equal))
       @ changed)
  in
  of_json raw
;;
