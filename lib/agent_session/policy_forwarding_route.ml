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
let checked raw = D.Json.validate raw ~limits:L.limits |> domain

module F = Policy_forwarding_frame
module Bounds = F.Bounds
module Seed = F.Seed
module Frame = F.Frame

let validate_frames = F.validate_frames
let validate_local = F.validate_local

module Route = struct
  module Leg = struct
    type t =
      { caller : P.Session_ref.t
      ; caller_source : Policy_caller_source.t
      ; callee : P.Session_ref.t
      ; callee_source : P.Run_source.t
      ; path : R.Reference.t list
      }
    [@@deriving equal]

    let of_seed (s : Seed.t) =
      { caller = s.caller
      ; caller_source = s.caller_source
      ; callee = s.callee
      ; callee_source = s.callee_source
      ; path = s.path
      }
    ;;

    let to_json t =
      `Object
        [ "caller", P.Session_ref.to_json t.caller
        ; "caller_source", Policy_caller_source.to_json t.caller_source
        ; "callee", P.Session_ref.to_json t.callee
        ; "callee_source", P.Run_source.to_json t.callee_source
        ; "path", `Array (List.map t.path ~f:R.reference_to_jsonaf)
        ]
    ;;

    let of_json raw =
      let open Result.Let_syntax in
      let%bind f = object_ raw in
      let%bind caller = required f "caller" P.Session_ref.of_json in
      let%bind caller_source = required f "caller_source" Policy_caller_source.of_json in
      let%bind callee = required f "callee" P.Session_ref.of_json in
      let%bind callee_source = required f "callee_source" P.Run_source.of_json in
      let%bind path =
        required
          f
          "path"
          (X.list (fun raw ->
             R.reference_of_jsonaf raw
             |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error))
      in
      let%bind () = P.Run_source.validate callee_source in
      let%map () = L.validate_path ~caller ~callee ~callee_source ~path in
      { caller; caller_source; callee; callee_source; path }
    ;;

    let shape =
      X.shape_exn
        [ "caller", L.session_shape
        ; "caller_source", Policy_caller_source.shape
        ; "callee", L.session_shape
        ; "callee_source", L.source_shape
        ; "path", X.array_shape_exn R.reference_shape
        ]
    ;;
  end

  type t =
    { bounds : Bounds.t
    ; target : Target.t
    ; prefix : Leg.t list
    ; raw : Jsonaf.t
    }

  let target t = t.target

  let known_json t =
    `Object
      [ "bounds", Bounds.to_json t.bounds
      ; "target", Target.to_json t.target
      ; "prefix", `Array (List.map t.prefix ~f:Leg.to_json)
      ]
  ;;

  let to_json t = t.raw
  let equal a b = json_equal (known_json a) (known_json b)

  let validate t =
    let open Result.Let_syntax in
    let%bind () =
      if
        List.is_empty t.prefix
        || List.length t.prefix > Bounds.max_inherited_scopes t.bounds
      then invalid "forwarding route capacity differs"
      else Ok ()
    in
    let%bind _ =
      List.fold_result t.prefix ~init:None ~f:(fun previous (leg : Leg.t) ->
        let%bind () =
          match previous with
          | None -> Ok ()
          | Some (prior : Leg.t) ->
            if
              P.Session_ref.equal prior.callee leg.caller
              && Policy_caller_source.matches_installed
                   leg.caller_source
                   prior.callee_source
            then Ok ()
            else invalid "forwarding route source differs"
        in
        Ok (Some leg))
    in
    let first = List.hd_exn t.prefix
    and last = List.last_exn t.prefix in
    let%bind () =
      match Target.view t.target with
      | Inherited _ -> Ok ()
      | Local _ ->
        if
          P.Session_ref.equal (Target.owner t.target) first.caller
          && Policy_caller_source.matches_installed
               first.caller_source
               (Target.source t.target)
        then Ok ()
        else invalid "forwarding local route introduction differs"
    in
    let path = List.concat_map t.prefix ~f:(fun (leg : Leg.t) -> leg.path) in
    let%bind () =
      if List.length path <= Bounds.max_inherited_scopes t.bounds
      then Ok ()
      else invalid "forwarding route depth exceeded"
    in
    L.validate_path
      ~caller:first.caller
      ~callee:last.callee
      ~callee_source:last.callee_source
      ~path
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = checked raw in
    let%bind f = object_ raw in
    let%bind bounds = required f "bounds" Bounds.of_json in
    let%bind target = required f "target" Target.of_json in
    let%bind prefix = required f "prefix" (X.list Leg.of_json) in
    let t = { bounds; target; prefix; raw } in
    let%map () = validate t in
    t
  ;;

  let shape =
    X.shape_exn
      [ "bounds", Bounds.shape
      ; "target", Target.shape
      ; "prefix", X.array_shape_exn Leg.shape
      ]
  ;;

  let check_next t ~caller ~caller_source ~callee ~callee_source ~path =
    let open Result.Let_syntax in
    let last = List.last_exn t.prefix in
    let%bind () =
      if
        P.Session_ref.equal last.callee caller
        && Policy_caller_source.matches_installed caller_source last.callee_source
      then Ok ()
      else invalid "forwarded stream caller endpoint differs"
    in
    let all = List.concat_map t.prefix ~f:(fun (leg : Leg.t) -> leg.path) @ path in
    let%bind () =
      if List.length all <= Bounds.max_inherited_scopes t.bounds
      then Ok ()
      else invalid "forwarded stream depth exceeded"
    in
    let%bind () = L.validate_path ~caller ~callee ~callee_source ~path in
    L.validate_path ~caller:(List.hd_exn t.prefix).caller ~callee ~callee_source ~path:all
  ;;
end

module Witness = struct
  type t =
    { bounds : Bounds.t
    ; target : Target.t
    ; frames : Frame.t list
    ; raw : Jsonaf.t
    }

  let frames t = t.frames

  let known_json t =
    `Object
      [ "bounds", Bounds.to_json t.bounds
      ; "target", Target.to_json t.target
      ; "frames", `Array (List.map t.frames ~f:Frame.known_json)
      ]
  ;;

  let to_json t = t.raw
  let known_sha256 t = digest (known_json t)

  let route t =
    let prefix =
      List.map t.frames ~f:(fun frame -> Route.Leg.of_seed (Frame.seed frame))
    in
    (* Copy the whole target child; path/source extension children remain in each
       raw frame seed and are copied into the stable leg below. *)
    let raw_legs =
      List.map t.frames ~f:(fun frame ->
        match Frame.to_json frame with
        | `Object fields ->
          (match List.Assoc.find fields "seed" ~equal:String.equal with
           | Some (`Object seed) ->
             `Object
               (List.filter seed ~f:(fun (key, _) ->
                  List.mem
                    [ "caller"; "caller_source"; "callee"; "callee_source"; "path" ]
                    key
                    ~equal:String.equal))
           | Some _ | None -> assert false)
        | _ -> assert false)
    in
    let bounds_raw =
      match t.raw with
      | `Object fields -> List.Assoc.find_exn fields "bounds" ~equal:String.equal
      | _ -> assert false
    in
    let target_raw =
      match t.raw with
      | `Object fields -> List.Assoc.find_exn fields "target" ~equal:String.equal
      | _ -> assert false
    in
    { Route.bounds = t.bounds
    ; target = t.target
    ; prefix
    ; raw =
        `Object [ "bounds", bounds_raw; "target", target_raw; "prefix", `Array raw_legs ]
    }
  ;;

  let of_json raw =
    let open Result.Let_syntax in
    let%bind () = checked raw in
    let%bind f = object_ raw in
    let%bind bounds = required f "bounds" Bounds.of_json in
    let%bind target = required f "target" Target.of_json in
    let%bind frames = required f "frames" (X.list Frame.of_json) in
    let%bind () = validate_frames ~bounds frames in
    let%bind () = validate_local target (Frame.seed (List.hd_exn frames)) in
    Ok { bounds; target; frames; raw }
  ;;

  let shape =
    X.shape_exn
      [ "bounds", Bounds.shape
      ; "target", Target.shape
      ; "frames", X.array_shape_exn ~identity_field:"frame_sha256" Frame.shape
      ]
  ;;
end
