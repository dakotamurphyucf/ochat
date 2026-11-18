open! Core
module P = Agent_protocol
module I = Instruction_content
module S = Agent_store.Prompt_program_bundle
module D = Document_schema
module X = Persistence_codec

module Kind = struct
  type t =
    | Project
    | Skills
  [@@deriving equal, sexp_of]
end

type t =
  { id : string
  ; kind : Kind.t
  ; root : I.Root_id.t option
  ; scope : I.Scope.t
  ; snapshot : I.Snapshot.t
  ; manifest : Jsonaf.t
  }

let id t = t.id
let kind t = t.kind
let root t = t.root
let scope t = t.scope
let loaded t = I.Snapshot.loaded t.snapshot
let payloads t = S.payloads (I.Snapshot.to_storage t.snapshot)

let file_key id =
  I.Root_id.to_string (I.Id.root id)
  ^ "/"
  ^ Source.Portable_path.to_string (I.Id.member id)
;;

let invalid message = Error (P.Error.invalid_request message)

let document result =
  Result.map_error result ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (D.Error.sexp_of_t e)))
;;

let store result =
  Result.map_error result ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (Agent_store.Store_error.sexp_of_t e)))
;;

let content result =
  Result.map_error result ~f:(fun e ->
    P.Error.invalid_request (Sexp.to_string_hum (I.Error.sexp_of_t e)))
;;

let scope_string scope =
  Option.value_map (I.Scope.path scope) ~default:"" ~f:Source.Portable_path.to_string
;;

let known_json ~kind ~root ~scope ~manifest =
  `Object
    [ ( "kind"
      , `String
          (match kind with
           | Kind.Project -> "project"
           | Skills -> "skills") )
    ; "root", X.option_json (fun r -> `String (I.Root_id.to_string r)) root
    ; "scope", `String (scope_string scope)
    ; "manifest", manifest
    ]
;;

let check ~kind ~root ~scope snapshot =
  let values = I.Snapshot.loaded snapshot in
  if List.length values > 64
  then invalid "instruction capture exceeds 64 members"
  else (
    match kind, root with
    | Kind.Skills, None ->
      if
        List.is_empty values
        || (not (String.is_empty (scope_string scope)))
        || List.exists values ~f:(fun value ->
          not (I.Kind.equal (I.Observed.kind (I.Loaded.observed value)) Skill))
      then invalid "skills capture requires nonempty skill members and no project scope"
      else Ok ()
    | Project, Some root ->
      let target = scope_string scope in
      List.fold_result values ~init:(-1) ~f:(fun previous value ->
        let observed = I.Loaded.observed value in
        let member =
          Source.Portable_path.to_string (I.Id.member (I.Observed.id observed))
        in
        let parent =
          if String.equal member "AGENTS.md"
          then Some ""
          else String.chop_suffix member ~suffix:"/AGENTS.md"
        in
        match parent with
        | None -> invalid "project capture member must be AGENTS.md"
        | Some parent ->
          let depth =
            if String.is_empty parent
            then 0
            else List.length (String.split parent ~on:'/')
          in
          if
            (not (I.Root_id.equal root (I.Id.root (I.Observed.id observed))))
            || (not (I.Kind.equal (I.Observed.kind observed) Project_guidance))
            || depth <= previous
            || not
                 (String.is_empty parent
                  || String.equal target parent
                  || String.is_prefix target ~prefix:(parent ^ "/"))
          then invalid "project capture must follow root-to-target guidance order"
          else Ok depth)
      |> Result.map ~f:(fun _ -> ())
    | Project, None | Skills, Some _ -> invalid "instruction capture root/kind mismatch")
;;

let construct ~kind ~root ~scope ~manifest snapshot =
  let open Result.Let_syntax in
  let%bind () = check ~kind ~root ~scope snapshot in
  let encoded = known_json ~kind ~root ~scope ~manifest |> Jsonaf.to_string in
  let id = Digestif.SHA256.(digest_string encoded |> to_hex) in
  Ok { id; kind; root; scope; snapshot; manifest }
;;

let project ~root ~scope snapshot =
  construct
    ~kind:Project
    ~root:(Some root)
    ~scope
    ~manifest:(S.Manifest.to_json (S.manifest (I.Snapshot.to_storage snapshot)))
    snapshot
;;

let skills snapshot =
  construct
    ~kind:Skills
    ~root:None
    ~scope:I.Scope.root
    ~manifest:(S.Manifest.to_json (S.manifest (I.Snapshot.to_storage snapshot)))
    snapshot
;;

let to_json t =
  match known_json ~kind:t.kind ~root:t.root ~scope:t.scope ~manifest:t.manifest with
  | `Object fields -> `Object (("id", `String t.id) :: fields)
  | `Null | `True | `False | `Number _ | `String _ | `Array _ -> assert false
;;

let decode_limits =
  D.Limits.create ~max_bytes:524288 ~max_depth:80 ~max_fields:32768 ~max_nodes:65536
  |> function
  | Ok limits -> limits
  | Error error -> raise_s [%sexp (error : D.Error.t)]
;;

let of_json json ~pool ~checkpoint =
  let open Result.Let_syntax in
  let%bind _ =
    D.Json.validate_and_measure_with_checkpoint ~limits:decode_limits ~checkpoint json
    |> document
  in
  let%bind fields = X.object_ json in
  let%bind expected_id = X.required fields "id" P.Json_codec.string in
  let%bind kind =
    X.required
      fields
      "kind"
      (P.Json_codec.enum
         ~name:"instruction capture kind"
         [ "project", Kind.Project; "skills", Skills ])
  in
  let%bind root =
    X.required
      fields
      "root"
      (X.nullable (fun json ->
         let%bind name = P.Json_codec.string json in
         I.Root_id.of_string name
         |> Result.map_error ~f:(fun _ ->
           P.Error.invalid_request "invalid instruction root label")))
  in
  let%bind scope = X.required fields "scope" P.Json_codec.string in
  let%bind scope =
    if String.is_empty scope
    then Ok I.Scope.root
    else
      Source.Portable_path.create scope
      |> Result.map ~f:I.Scope.of_path
      |> Result.map_error ~f:(fun _ ->
        P.Error.invalid_request "invalid instruction scope")
  in
  let%bind raw_manifest = X.required fields "manifest" X.raw in
  let%bind manifest = S.Manifest.of_json raw_manifest |> document in
  let%bind selected =
    List.map (S.Manifest.payloads manifest) ~f:(fun (sha, _) ->
      Map.find pool sha
      |> Result.of_option ~error:(P.Error.invalid_request "missing instruction payload"))
    |> Result.all
  in
  let%bind stored = S.restore manifest selected |> store in
  let%bind snapshot =
    I.Snapshot.of_storage stored ~limits:I.Limits.standard ~checkpoint |> content
  in
  let%bind value = construct ~kind ~root ~scope ~manifest:raw_manifest snapshot in
  if String.equal expected_id value.id
  then Ok value
  else invalid "instruction capture identity mismatch"
;;

let shape =
  X.shape_exn
    [ "id", D.Shape.value
    ; "kind", D.Shape.value
    ; "root", D.Shape.value
    ; "scope", D.Shape.value
    ; "manifest", D.Shape.value
    ]
;;
