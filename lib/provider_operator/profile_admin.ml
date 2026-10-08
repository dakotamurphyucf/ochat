open! Core
module P = Agent_protocol
module DTO = P.Provider_operator
module M = Credential_registry_model
module C = Credential_registry
module Bridge = Inference_host.Credential_bridge
module S = Private_storage

module Error = struct
  type t =
    | Invalid_template
    | Missing_profile
    | Conflict
    | Missing_setup
    | Busy
    | Publication_uncertain
    | Storage of S.Error.t
    | Registry of C.Error.t
    | Bridge of Bridge.Error.t
  [@@deriving sexp_of]
end

module Template = struct
  type authentication =
    | Api_key
    | Direct_codex
  [@@deriving equal, sexp_of]

  type t =
    { profile : DTO.Profile_id.t
    ; binding : M.Id.t
    ; revision : DTO.Revision.t
    ; authentication : authentication
    ; expectation : M.Expectation.t
    ; expected_account : string option
    ; mapping : M.Identity.t -> (Bridge.Mapping.t, Bridge.Error.t) Result.t
    }

  let create
        ~profile
        ~binding
        ~revision
        ~authentication
        ~expectation
        ~expected_account
        ~mapping
    =
    if
      Option.exists expected_account ~f:(fun account ->
        String.is_empty account || String.length account > 256)
    then Error Error.Invalid_template
    else
      Ok
        { profile
        ; binding
        ; revision
        ; authentication
        ; expectation
        ; expected_account
        ; mapping
        }
  ;;

  let profile t = t.profile
  let binding t = t.binding
  let expectation t = t.expectation
  let authentication t = t.authentication

  let descriptor t =
    `Object
      [ "profile", DTO.Profile_id.to_json t.profile
      ; "binding", `String (M.Id.to_string t.binding)
      ; ( "authentication"
        , `String
            (match t.authentication with
             | Api_key -> "api_key"
             | Direct_codex -> "direct_codex") )
      ; "revision", DTO.Revision.to_json t.revision
      ; ( "account"
        , match t.expected_account with
          | None -> `Null
          | Some account -> `String account )
      ]
  ;;
end

let name value =
  match S.Name.create value with
  | Ok name -> name
  | Error _ -> failwith "invalid static profile metadata filename"
;;

let metadata_name = name "operator-profiles.json"
let lock_name = name "operator-profiles.lock"

let storage error =
  if S.Error.equal_code (S.Error.code error) S.Error.Busy
  then Error.Busy
  else (
    match S.Error.publication error with
    | Some Published_durability_unknown -> Publication_uncertain
    | _ -> Storage error)
;;

let valid_templates templates =
  (not (List.is_empty templates))
  && List.length templates <= 128
  && Option.is_none
       (List.find_a_dup
          (List.map templates ~f:(fun t -> DTO.Profile_id.to_string t.Template.profile))
          ~compare:String.compare)
  && Option.is_none
       (List.find_a_dup
          (List.map templates ~f:(fun t -> M.Id.to_string t.Template.binding))
          ~compare:String.compare)
;;

let descriptors templates =
  List.map templates ~f:Template.descriptor |> fun values -> `Array values
;;

module Selection_proofs = struct
  type proof =
    { operation : M.Id.t
    ; principal : P.Id.Principal.t
    ; request : DTO.Select_request.t
    ; result : DTO.Selection_result.t
    }

  type t = proof list

  let maximum = 64
  let empty = []
  let length = List.length

  let same_json a b =
    match P.Json_codec.canonical_string a, P.Json_codec.canonical_string b with
    | Ok a, Ok b -> String.equal a b
    | _ -> false
  ;;

  let lookup t ~principal ~operation request =
    match List.find t ~f:(fun proof -> M.Id.equal proof.operation operation) with
    | None -> Ok None
    | Some proof ->
      if
        P.Id.Principal.equal proof.principal principal
        && same_json
             (DTO.Select_request.to_json proof.request)
             (DTO.Select_request.to_json request)
      then Ok (Some proof.result)
      else Error Error.Conflict
  ;;

  let add t ~principal ~operation request result =
    let open Result.Let_syntax in
    let%bind existing = lookup t ~principal ~operation request in
    match existing with
    | Some previous ->
      if
        same_json
          (DTO.Selection_result.to_json previous)
          (DTO.Selection_result.to_json result)
      then Ok t
      else Error Error.Conflict
    | None ->
      if
        (not (DTO.Profile_id.equal request.profile result.DTO.Selection_result.profile))
        || DTO.Revision.equal request.expected_revision result.revision
      then Error Error.Conflict
      else (
        let retained = if List.length t = maximum then List.tl_exn t else t in
        Ok (retained @ [ { operation; principal; request; result } ]))
  ;;

  let proof_json p =
    `Object
      [ "operation", `String (M.Id.to_string p.operation)
      ; "principal", P.Id.Principal.to_json p.principal
      ; "request", DTO.Select_request.to_json p.request
      ; "result", DTO.Selection_result.to_json p.result
      ]
  ;;

  let to_json t = `Array (List.map t ~f:proof_json)

  let of_json json =
    let open Result.Let_syntax in
    let invalid () = Error Error.Invalid_template in
    let decode json =
      let%bind fields =
        match json with
        | `Object f -> Ok f
        | _ -> invalid ()
      in
      let get key =
        List.Assoc.find fields key ~equal:String.equal
        |> Result.of_option ~error:Error.Invalid_template
      in
      let%bind op = get "operation" in
      let%bind operation =
        match op with
        | `String v ->
          M.Id.create v |> Result.map_error ~f:(fun _ -> Error.Invalid_template)
        | _ -> invalid ()
      in
      let decode get_decoder key =
        Result.bind (get key) ~f:(fun value ->
          get_decoder value |> Result.map_error ~f:(fun _ -> Error.Invalid_template))
      in
      let%bind principal = decode P.Id.Principal.of_json "principal" in
      let%bind request = decode DTO.Select_request.of_json "request" in
      let%bind result = decode DTO.Selection_result.of_json "result" in
      let proof = { operation; principal; request; result } in
      if
        same_json json (proof_json proof)
        && DTO.Profile_id.equal request.profile result.profile
        && not (DTO.Revision.equal request.expected_revision result.revision)
      then Ok proof
      else invalid ()
    in
    let%bind values =
      match json with
      | `Array values when List.length values <= maximum -> Ok values
      | _ -> invalid ()
    in
    let%bind proofs = List.map values ~f:decode |> Result.all in
    if
      Option.is_some
        (List.find_a_dup
           (List.map proofs ~f:(fun p -> M.Id.to_string p.operation))
           ~compare:String.compare)
    then invalid ()
    else Ok proofs
  ;;
end

let encode ~incarnation ~templates ~selection ~proofs =
  `Object
    [ "version", `Number "2"
    ; "incarnation", `String (M.Id.to_string incarnation)
    ; "templates", descriptors templates
    ; "selection", DTO.Selection_result.to_json selection
    ; "proofs", Selection_proofs.to_json proofs
    ]
;;

let bounded_metadata json =
  let encoded = Jsonaf.to_string json in
  if String.length encoded > 256 * 1024
  then Error Error.Invalid_template
  else Ok (Bytes.of_string encoded)
;;

let initialize directory ~incarnation ~templates ~default_profile ~initial_revision =
  if
    (not (valid_templates templates))
    || not
         (List.exists templates ~f:(fun t ->
            DTO.Profile_id.equal t.Template.profile default_profile))
  then Error Error.Invalid_template
  else
    let open Result.Let_syntax in
    let%bind encoded =
      bounded_metadata
        (encode
           ~incarnation
           ~templates
           ~proofs:Selection_proofs.empty
           ~selection:
             { DTO.Selection_result.profile = default_profile
             ; revision = initial_revision
             })
    in
    S.Directory.create_immutable directory metadata_name encoded
    |> Result.map_error ~f:storage
;;

type t =
  { directory : S.Directory.t
  ; incarnation : M.Id.t
  ; registry : C.t
  ; templates : Template.t list
  ; publish : Bridge.Mapping.t -> (unit, Bridge.Error.t) Result.t
  ; new_revision : unit -> DTO.Revision.t
  }

let templates t = t.templates

let find_template t profile =
  List.find t.templates ~f:(fun template ->
    DTO.Profile_id.equal template.Template.profile profile)
  |> Result.of_option ~error:Error.Missing_profile
;;

let read_selection t =
  let open Result.Let_syntax in
  let%bind bytes =
    S.Directory.read_bounded t.directory metadata_name ~max_bytes:(256 * 1024)
    |> Result.map_error ~f:(fun error ->
      if S.Error.equal_code (S.Error.code error) Missing
      then Error.Missing_setup
      else storage error)
  in
  let%bind json =
    Result.try_with (fun () -> Jsonaf.of_string (Bytes.to_string bytes))
    |> Result.map_error ~f:(fun _ -> Error.Invalid_template)
  in
  let%bind fields =
    match json with
    | `Object fields -> Ok fields
    | _ -> Error Error.Invalid_template
  in
  let get key =
    List.Assoc.find fields key ~equal:String.equal
    |> Result.of_option ~error:Error.Invalid_template
  in
  let%bind selection_json = get "selection" in
  let%bind selection =
    DTO.Selection_result.of_json selection_json
    |> Result.map_error ~f:(fun _ -> Error.Invalid_template)
  in
  let%bind version = get "version" in
  let%bind proofs, expected =
    match version with
    | `Number "2" ->
      let%map proofs = Result.bind (get "proofs") ~f:Selection_proofs.of_json in
      proofs, encode ~incarnation:t.incarnation ~templates:t.templates ~selection ~proofs
    | `Number "1" ->
      let%bind operation = get "operation" in
      let%bind () =
        match operation with
        | `Null -> Ok ()
        | `String v ->
          M.Id.create v
          |> Result.map ~f:ignore
          |> Result.map_error ~f:(fun _ -> Error.Invalid_template)
        | _ -> Error Error.Invalid_template
      in
      Ok
        ( Selection_proofs.empty
        , `Object
            [ "version", `Number "1"
            ; "incarnation", `String (M.Id.to_string t.incarnation)
            ; "templates", descriptors t.templates
            ; "selection", DTO.Selection_result.to_json selection
            ; "operation", operation
            ] )
    | _ -> Error Error.Invalid_template
  in
  let%bind () =
    if Selection_proofs.same_json json expected then Ok () else Error Error.Conflict
  in
  let%bind _ = find_template t selection.profile in
  let%bind () =
    List.fold_result proofs ~init:() ~f:(fun () proof ->
      find_template t proof.Selection_proofs.result.profile |> Result.map ~f:ignore)
  in
  let%map () =
    match List.last proofs with
    | None -> Ok ()
    | Some proof ->
      if
        Jsonaf.exactly_equal
          (DTO.Selection_result.to_json proof.result)
          (DTO.Selection_result.to_json selection)
      then Ok ()
      else Error Error.Conflict
  in
  selection, proofs
;;

let locked t f =
  Eio.Switch.run (fun sw ->
    match S.Lock.acquire t.directory lock_name ~sw ~mode:Exclusive with
    | Error error -> Error (storage error)
    | Ok lock -> Exn.protect ~finally:(fun () -> S.Lock.release lock) ~f)
;;

let selection t = locked t (fun () -> read_selection t |> Result.map ~f:fst)

let open_ directory ~incarnation ~registry ~templates ~publish ~new_revision =
  if not (valid_templates templates)
  then Error Error.Invalid_template
  else (
    let t = { directory; incarnation; registry; templates; publish; new_revision } in
    Result.map (selection t) ~f:(fun _ -> t))
;;

let publish_snapshot t template snapshot =
  let open Result.Let_syntax in
  let%bind identity =
    C.Host_snapshot.identity snapshot |> Result.of_option ~error:Error.Missing_profile
  in
  let%bind () =
    if
      M.Expectation.accepts template.Template.expectation identity
      &&
      match template.expected_account with
      | None -> true
      | Some account ->
        Option.equal String.equal (M.Identity.account identity) (Some account)
    then Ok ()
    else Error Error.Conflict
  in
  let%bind mapping =
    template.mapping identity |> Result.map_error ~f:(fun e -> Error.Bridge e)
  in
  let%bind () =
    if
      M.Id.equal (Bridge.Mapping.binding mapping) template.binding
      && String.equal
           (Bridge.Mapping.profile mapping)
           (DTO.Profile_id.to_string template.profile)
    then Ok ()
    else Error Error.Conflict
  in
  t.publish mapping |> Result.map_error ~f:(fun e -> Error.Bridge e)
;;

let synchronize t =
  let open Result.Let_syntax in
  let%bind _ = selection t in
  let%bind snapshot =
    C.synchronize t.registry |> Result.map_error ~f:(fun e -> Error.Registry e)
  in
  List.fold_result t.templates ~init:() ~f:(fun () template ->
    match
      List.find (C.Host_snapshot.bindings snapshot) ~f:(fun b ->
        M.Id.equal (C.Host_snapshot.id b) template.Template.binding)
    with
    | None -> Ok ()
    | Some binding ->
      (match C.Host_snapshot.identity binding with
       | None -> Ok ()
       | Some _ -> publish_snapshot t template binding))
;;

let publish_committed t ~template ~operation =
  let open Result.Let_syntax in
  let%bind state =
    C.reconcile_operation t.registry ~binding:template.Template.binding ~operation
    |> Result.map_error ~f:(fun e -> Error.Registry e)
  in
  let%bind () =
    match state with
    | M.Operation.Committed -> Ok ()
    | Pending | Unavailable -> Error Error.Publication_uncertain
    | Rejected -> Error Error.Conflict
  in
  synchronize t
;;

let select t ~principal ~operation ~reconcile (request : DTO.Select_request.t) =
  locked t (fun () ->
    let open Result.Let_syntax in
    let%bind _ = find_template t request.profile in
    let%bind current, proofs = read_selection t in
    let%bind previous = Selection_proofs.lookup proofs ~principal ~operation request in
    match previous with
    | Some original -> Ok original
    | None ->
      let%bind () = if reconcile then Error Error.Publication_uncertain else Ok () in
      let%bind () =
        if DTO.Revision.equal current.revision request.expected_revision
        then Ok ()
        else Error Error.Conflict
      in
      let updated =
        { DTO.Selection_result.profile = request.profile; revision = t.new_revision () }
      in
      let%bind proofs =
        Selection_proofs.add proofs ~principal ~operation request updated
      in
      let%bind encoded =
        bounded_metadata
          (encode
             ~incarnation:t.incarnation
             ~templates:t.templates
             ~selection:updated
             ~proofs)
      in
      let%map () =
        S.Directory.replace_metadata t.directory metadata_name encoded
        |> Result.map_error ~f:storage
      in
      updated)
;;

let backend t ~bridge ~principal =
  let rec view bridge =
    Inference_host.Backend.create
      ~capture:(fun ~current ~model ~settings ->
        let open Result.Let_syntax in
        let%bind default_profile =
          match current with
          | Some target -> Ok (Inference.Request.Target.profile target)
          | None ->
            selection t
            |> Result.map ~f:(fun selected -> DTO.Profile_id.to_string selected.profile)
            |> Result.map_error ~f:(fun _ ->
              Inference_runtime.Preparation_error.Target_unavailable)
        in
        Bridge.capture bridge ~principal ~default_profile ~current ~model ~settings
        |> Result.map_error ~f:Bridge.preparation_error)
      ~resolve:(Bridge.resolver bridge ~principal)
      ~with_response_limit:(fun ~max_body_bytes ->
        Bridge.with_response_limit bridge ~max_body_bytes
        |> Result.map ~f:view
        |> Result.map_error ~f:Bridge.preparation_error)
  in
  view bridge
;;
