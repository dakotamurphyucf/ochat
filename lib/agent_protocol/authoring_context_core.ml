open! Core
open Authoring_common
module C = Authoring_codec

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value
let source_limit = 8 * 1024 * 1024

let bounded_name value =
  if
    String.is_empty value
    || String.length value > 256
    || String.exists value ~f:(fun char ->
      let code = Char.to_int char in
      code < 0x20 || code = 0x7f)
  then C.invalid "invalid authoring selection name"
  else C.utf8_text ~max_bytes:256 (`String value)
;;

let name_json json =
  let* value = C.utf8_text ~max_bytes:256 json in
  bounded_name value
;;

let optional_name value =
  match value with
  | None -> Ok None
  | Some value -> Result.map (bounded_name value) ~f:Option.some
;;

let equal_source_id a b = Chatml_source.Id.equal (Source.id a) (Source.id b)

let unique_sources sources =
  let ids = List.map sources ~f:Source.id in
  List.length ids
  = List.length (List.dedup_and_sort ids ~compare:Chatml_source.Id.compare)
;;

module Context = struct
  module Selection = struct
    type target =
      | Entry of string
      | Library
    [@@deriving equal, sexp_of]

    type t =
      { target : target
      ; environment : string option
      }
    [@@deriving equal, sexp_of]

    let create ~target ~environment =
      let* () =
        match target with
        | Entry name -> Result.map (bounded_name name) ~f:(fun _ -> ())
        | Library -> Ok ()
      in
      let* environment = optional_name environment in
      Ok { target; environment }
    ;;

    let to_json t =
      let target_fields =
        match t.target with
        | Entry name -> [ "target", str "entry"; "entry", str name ]
        | Library -> [ "target", str "library" ]
      in
      obj (target_fields @ C.optional_field "environment" t.environment str)
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "target"; "entry"; "environment" ] json in
      let* target = C.required_as fields "target" (C.string ~max_bytes:16) in
      let* environment = C.optional_as fields "environment" name_json in
      match target with
      | "entry" ->
        let* name = C.required_as fields "entry" name_json in
        create ~target:(Entry name) ~environment
      | "library" ->
        if Option.is_some (Json_codec.optional fields "entry")
        then C.invalid "library selection has entry field"
        else create ~target:Library ~environment
      | _ -> C.invalid "unknown authoring target"
    ;;
  end

  type origin =
    | Host_project of
        { root : Id.Authoring_root.t
        ; dependency_roots : Id.Authoring_root.t list
        ; project_member : string
        ; selection : Selection.t
        }
    | Uploaded_project of Id.Authoring_bundle.t
    | Host_document of
        { root : Id.Authoring_root.t
        ; source : Source.t
        ; default_environment : string option
        }
    | Supplied_document of
        { source : Source.t
        ; default_environment : string option
        }
  [@@deriving equal, sexp_of]

  let root_list_json roots = C.array Id.Authoring_root.to_json roots
  let root_list_of_json = C.list ~max_items:64 Id.Authoring_root.of_json

  let validate_roots ~root dependencies =
    if
      List.length dependencies > 64
      || List.length dependencies
         <> List.length
              (List.dedup_and_sort dependencies ~compare:Id.Authoring_root.compare)
      || List.mem dependencies root ~equal:Id.Authoring_root.equal
    then C.invalid "duplicate authoring roots"
    else Ok ()
  ;;

  let validate_origin = function
    | Host_project { root; dependency_roots; project_member; selection = _ } ->
      let* _ = C.portable_member_string project_member in
      validate_roots ~root dependency_roots
    | Uploaded_project _ -> Ok ()
    | Host_document { source; default_environment; _ }
    | Supplied_document { source; default_environment } ->
      let* _ = optional_name default_environment in
      if Option.is_some (Source.editor_revision source)
      then C.invalid "base authoring origin cannot be an overlay"
      else Ok ()
  ;;

  let origin_to_json = function
    | Host_project { root; dependency_roots; project_member; selection } ->
      obj
        [ "kind", str "host_project"
        ; "root", Id.Authoring_root.to_json root
        ; "dependency_roots", root_list_json dependency_roots
        ; "project_member", str project_member
        ; "selection", Selection.to_json selection
        ]
    | Uploaded_project bundle ->
      obj [ "kind", str "uploaded_project"; "bundle", Id.Authoring_bundle.to_json bundle ]
    | Host_document { root; source; default_environment } ->
      obj
        ([ "kind", str "host_document"
         ; "root", Id.Authoring_root.to_json root
         ; "source", Source.to_json source
         ]
         @ C.optional_field "default_environment" default_environment str)
    | Supplied_document { source; default_environment } ->
      obj
        ([ "kind", str "supplied_document"; "source", Source.to_json source ]
         @ C.optional_field "default_environment" default_environment str)
  ;;

  let origin_of_json json =
    let* fields =
      C.fields
        ~allowed:
          [ "kind"
          ; "root"
          ; "dependency_roots"
          ; "project_member"
          ; "selection"
          ; "bundle"
          ; "source"
          ; "default_environment"
          ]
        json
    in
    let* kind = C.required_as fields "kind" (C.string ~max_bytes:32) in
    let expected =
      match kind with
      | "host_project" ->
        [ "kind"; "root"; "dependency_roots"; "project_member"; "selection" ]
      | "uploaded_project" -> [ "kind"; "bundle" ]
      | "host_document" -> [ "kind"; "root"; "source"; "default_environment" ]
      | "supplied_document" -> [ "kind"; "source"; "default_environment" ]
      | _ -> []
    in
    if
      List.is_empty expected
      || List.exists (Json_codec.to_alist fields) ~f:(fun (name, _) ->
        not (List.mem expected name ~equal:String.equal))
    then C.invalid "invalid authoring origin kind/fields"
    else
      let* origin =
        match kind with
        | "host_project" ->
          let* root = C.required_as fields "root" Id.Authoring_root.of_json in
          let* dependency_roots =
            C.required_as fields "dependency_roots" root_list_of_json
          in
          let* project_member = C.required_as fields "project_member" C.portable_member in
          let* selection = C.required_as fields "selection" Selection.of_json in
          Ok (Host_project { root; dependency_roots; project_member; selection })
        | "uploaded_project" ->
          let* bundle = C.required_as fields "bundle" Id.Authoring_bundle.of_json in
          Ok (Uploaded_project bundle)
        | "host_document" ->
          let* root = C.required_as fields "root" Id.Authoring_root.of_json in
          let* source = C.required_as fields "source" Source.of_json in
          let* default_environment =
            C.optional_as fields "default_environment" name_json
          in
          Ok (Host_document { root; source; default_environment })
        | "supplied_document" ->
          let* source = C.required_as fields "source" Source.of_json in
          let* default_environment =
            C.optional_as fields "default_environment" name_json
          in
          Ok (Supplied_document { source; default_environment })
        | _ -> assert false
      in
      let* () = validate_origin origin in
      Ok origin
  ;;

  module Open_request = struct
    type t =
      | Host_project of
          { root : Id.Authoring_root.t
          ; dependency_roots : Id.Authoring_root.t list
          ; project_member : string
          ; selection : Selection.t
          }
      | Uploaded_project of Id.Authoring_bundle.t
      | Host_document of
          { root : Id.Authoring_root.t
          ; member : string
          ; default_environment : string option
          }
      | Supplied_document of
          { member : string
          ; default_environment : string option
          ; contents : string
          }
    [@@deriving sexp_of]

    let create_host_project ~root ~dependency_roots ~project_member ~selection =
      let* project_member = C.portable_member_string project_member in
      let* () = validate_roots ~root dependency_roots in
      Ok (Host_project { root; dependency_roots; project_member; selection })
    ;;

    let create_uploaded_project bundle = Uploaded_project bundle

    let create_host_document ~root ~member ~default_environment =
      let* member = C.portable_member_string member in
      let* default_environment = optional_name default_environment in
      Ok (Host_document { root; member; default_environment })
    ;;

    let create_supplied_document ~member ~default_environment ~contents =
      let* member = C.portable_member_string member in
      let* default_environment = optional_name default_environment in
      if String.length contents > source_limit
      then C.invalid "supplied authoring document is too large"
      else Ok (Supplied_document { member; default_environment; contents })
    ;;

    let to_json = function
      | Host_project { root; dependency_roots; project_member; selection } ->
        obj
          [ "kind", str "host_project"
          ; "root", Id.Authoring_root.to_json root
          ; "dependency_roots", root_list_json dependency_roots
          ; "project_member", str project_member
          ; "selection", Selection.to_json selection
          ]
      | Uploaded_project bundle ->
        obj
          [ "kind", str "uploaded_project"; "bundle", Id.Authoring_bundle.to_json bundle ]
      | Host_document { root; member; default_environment } ->
        obj
          ([ "kind", str "host_document"
           ; "root", Id.Authoring_root.to_json root
           ; "member", str member
           ]
           @ C.optional_field "default_environment" default_environment str)
      | Supplied_document { member; default_environment; contents } ->
        obj
          ([ "kind", str "supplied_document"
           ; "member", str member
           ; "contents_base64", C.bytes_base64_json contents
           ]
           @ C.optional_field "default_environment" default_environment str)
    ;;

    let of_json json =
      let* fields =
        C.fields
          ~allowed:
            [ "kind"
            ; "root"
            ; "dependency_roots"
            ; "project_member"
            ; "selection"
            ; "bundle"
            ; "member"
            ; "default_environment"
            ; "contents_base64"
            ]
          json
      in
      let* kind = C.required_as fields "kind" (C.string ~max_bytes:32) in
      let allowed =
        match kind with
        | "host_project" ->
          [ "kind"; "root"; "dependency_roots"; "project_member"; "selection" ]
        | "uploaded_project" -> [ "kind"; "bundle" ]
        | "host_document" -> [ "kind"; "root"; "member"; "default_environment" ]
        | "supplied_document" ->
          [ "kind"; "member"; "default_environment"; "contents_base64" ]
        | _ -> []
      in
      if
        List.is_empty allowed
        || List.exists (Json_codec.to_alist fields) ~f:(fun (name, _) ->
          not (List.mem allowed name ~equal:String.equal))
      then C.invalid "invalid authoring open kind/fields"
      else (
        match kind with
        | "host_project" ->
          let* root = C.required_as fields "root" Id.Authoring_root.of_json in
          let* dependency_roots =
            C.required_as fields "dependency_roots" root_list_of_json
          in
          let* project_member = C.required_as fields "project_member" C.portable_member in
          let* selection = C.required_as fields "selection" Selection.of_json in
          create_host_project ~root ~dependency_roots ~project_member ~selection
        | "uploaded_project" ->
          Result.map
            (C.required_as fields "bundle" Id.Authoring_bundle.of_json)
            ~f:create_uploaded_project
        | "host_document" ->
          let* root = C.required_as fields "root" Id.Authoring_root.of_json in
          let* member = C.required_as fields "member" C.portable_member in
          let* default_environment =
            C.optional_as fields "default_environment" name_json
          in
          create_host_document ~root ~member ~default_environment
        | "supplied_document" ->
          let* member = C.required_as fields "member" C.portable_member in
          let* default_environment =
            C.optional_as fields "default_environment" name_json
          in
          let* contents =
            C.required_as
              fields
              "contents_base64"
              (C.bytes_base64 ~max_bytes:source_limit)
          in
          create_supplied_document ~member ~default_environment ~contents
        | _ -> assert false)
    ;;
  end

  type static_selection =
    { environment : string
    ; compiler_contract : Chatml_source.Digest.t
    }
  [@@deriving equal, sexp_of]

  let static_selection_to_json t =
    obj
      [ "environment", str t.environment
      ; "compiler_contract", C.digest_json t.compiler_contract
      ]
  ;;

  let static_selection_of_json json =
    let* fields = C.fields ~allowed:[ "environment"; "compiler_contract" ] json in
    let* environment = C.required_as fields "environment" name_json in
    let* compiler_contract = C.required_as fields "compiler_contract" C.digest in
    Ok { environment; compiler_contract }
  ;;

  type basis =
    | Project of
        { base_graph : Chatml_source.Digest.t
        ; selection : static_selection
        }
    | Document of
        { source : Source.t
        ; selection : static_selection option
        }
  [@@deriving equal, sexp_of]

  let basis_to_json = function
    | Project { base_graph; selection } ->
      obj
        [ "kind", str "project"
        ; "base_graph", C.digest_json base_graph
        ; "selection", static_selection_to_json selection
        ]
    | Document { source; selection } ->
      obj
        ([ "kind", str "document"; "source", Source.to_json source ]
         @ C.optional_field "selection" selection static_selection_to_json)
  ;;

  let basis_of_json json =
    let* fields =
      C.fields ~allowed:[ "kind"; "base_graph"; "selection"; "source" ] json
    in
    let* kind = C.required_as fields "kind" (C.string ~max_bytes:16) in
    match kind with
    | "project" ->
      if Option.is_some (Json_codec.optional fields "source")
      then C.invalid "project basis has document source"
      else
        let* base_graph = C.required_as fields "base_graph" C.digest in
        let* selection = C.required_as fields "selection" static_selection_of_json in
        Ok (Project { base_graph; selection })
    | "document" ->
      if Option.is_some (Json_codec.optional fields "base_graph")
      then C.invalid "document basis has project graph"
      else
        let* source = C.required_as fields "source" Source.of_json in
        let* selection = C.optional_as fields "selection" static_selection_of_json in
        Ok (Document { source; selection })
    | _ -> C.invalid "unknown authoring basis"
  ;;

  type t =
    { id : Id.Authoring_context.t
    ; host : Id.Server.t
    ; origin : origin
    ; generation : int
    ; basis : basis
    ; basis_fingerprint : Chatml_source.Digest.t
    ; editable_sources : Source.t list
    }
  [@@deriving sexp_of]

  let create ~id ~host ~origin ~generation ~basis ~basis_fingerprint ~editable_sources =
    let* () = validate_origin origin in
    if
      generation < 0
      || generation > C.maximum_wire_integer
      || List.length editable_sources > 4096
      || (generation = 0 && not (List.is_empty editable_sources))
      || not (unique_sources editable_sources)
    then C.invalid "invalid authoring context generation or source inventory"
    else
      let* () =
        match origin, basis with
        | (Host_project _ | Uploaded_project _), Project _ -> Ok ()
        | ( (Host_document { source = base; _ } | Supplied_document { source = base; _ })
          , Document { source = current; _ } ) ->
          if not (equal_source_id base current)
          then C.invalid "document basis has foreign source"
          else if generation = 0 && not (Source.equal base current)
          then C.invalid "initial document basis differs from base capture"
          else if
            Option.is_none (Source.editor_revision current)
            && not (Source.equal base current)
          then C.invalid "closed document basis differs from base capture"
          else Ok ()
        | _ -> C.invalid "authoring origin and basis disagree"
      in
      let* () =
        match basis with
        | Project _ -> Ok ()
        | Document { selection; _ } ->
          (match selection with
           | None -> Ok ()
           | Some { environment; _ } ->
             Result.map (bounded_name environment) ~f:(fun _ -> ()))
      in
      let* () =
        match basis with
        | Project _ ->
          if
            List.exists editable_sources ~f:(fun source ->
              Option.is_none (Source.editor_revision source))
          then C.invalid "project editable source lacks editor revision"
          else Ok ()
        | Document { source; _ } ->
          (match Source.editor_revision source, editable_sources with
           | None, [] -> Ok ()
           | Some _, [ item ] when Source.equal item source -> Ok ()
           | _ -> C.invalid "document context has inconsistent editable source")
      in
      Ok { id; host; origin; generation; basis; basis_fingerprint; editable_sources }
  ;;

  let id t = t.id
  let host t = t.host
  let origin t = t.origin
  let generation t = t.generation
  let basis t = t.basis
  let basis_fingerprint t = t.basis_fingerprint
  let editable_sources t = t.editable_sources

  type view = t

  let to_json t =
    obj
      [ "id", Id.Authoring_context.to_json t.id
      ; "host", Id.Server.to_json t.host
      ; "origin", origin_to_json t.origin
      ; "generation", C.integer t.generation
      ; "basis", basis_to_json t.basis
      ; "basis_fingerprint", C.digest_json t.basis_fingerprint
      ; "editable_sources", C.array Source.to_json t.editable_sources
      ]
  ;;

  let of_json json =
    let* fields =
      C.fields
        ~allowed:
          [ "id"
          ; "host"
          ; "origin"
          ; "generation"
          ; "basis"
          ; "basis_fingerprint"
          ; "editable_sources"
          ]
        json
    in
    let* id = C.required_as fields "id" Id.Authoring_context.of_json in
    let* host = C.required_as fields "host" Id.Server.of_json in
    let* origin = C.required_as fields "origin" origin_of_json in
    let* generation = C.required_as fields "generation" C.nonnegative in
    let* basis = C.required_as fields "basis" basis_of_json in
    let* basis_fingerprint = C.required_as fields "basis_fingerprint" C.digest in
    let* editable_sources =
      C.required_as fields "editable_sources" (C.list ~max_items:4096 Source.of_json)
    in
    create ~id ~host ~origin ~generation ~basis ~basis_fingerprint ~editable_sources
  ;;

  module Expected = struct
    type t =
      { context_id : Id.Authoring_context.t
      ; host : Id.Server.t
      ; generation : int
      ; basis_fingerprint : Chatml_source.Digest.t
      }
    [@@deriving equal, sexp_of]

    let of_view view =
      { context_id = view.id
      ; host = view.host
      ; generation = view.generation
      ; basis_fingerprint = view.basis_fingerprint
      }
    ;;

    let to_json t =
      obj
        [ "context_id", Id.Authoring_context.to_json t.context_id
        ; "host", Id.Server.to_json t.host
        ; "generation", C.integer t.generation
        ; "basis_fingerprint", C.digest_json t.basis_fingerprint
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "context_id"; "host"; "generation"; "basis_fingerprint" ] json
      in
      let* context_id = C.required_as fields "context_id" Id.Authoring_context.of_json in
      let* host = C.required_as fields "host" Id.Server.of_json in
      let* generation = C.required_as fields "generation" C.nonnegative in
      let* basis_fingerprint = C.required_as fields "basis_fingerprint" C.digest in
      Ok { context_id; host; generation; basis_fingerprint }
    ;;
  end

  module Refresh_request = struct
    type policy =
      | Reject_open_documents
      | Discard_open_documents
    [@@deriving equal, sexp_of]

    type t =
      { expected : Expected.t
      ; policy : policy
      }
    [@@deriving sexp_of]

    let create ~expected ~policy = { expected; policy }

    let policy_to_json = function
      | Reject_open_documents -> str "reject_open_documents"
      | Discard_open_documents -> str "discard_open_documents"
    ;;

    let policy_of_json =
      Json_codec.enum
        ~name:"authoring refresh policy"
        [ "reject_open_documents", Reject_open_documents
        ; "discard_open_documents", Discard_open_documents
        ]
    ;;

    let to_json t =
      obj [ "expected", Expected.to_json t.expected; "policy", policy_to_json t.policy ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "expected"; "policy" ] json in
      let* expected = C.required_as fields "expected" Expected.of_json in
      let* policy = C.required_as fields "policy" policy_of_json in
      Ok { expected; policy }
    ;;
  end

  module Close_request = struct
    type t = { expected : Expected.t } [@@deriving sexp_of]

    let create ~expected = { expected }
    let to_json t = obj [ "expected", Expected.to_json t.expected ]

    let of_json json =
      let* fields = C.fields ~allowed:[ "expected" ] json in
      let* expected = C.required_as fields "expected" Expected.of_json in
      Ok { expected }
    ;;
  end

  module Close_result = struct
    type t =
      { id : Id.Authoring_context.t
      ; host : Id.Server.t
      ; generation : int
      }
    [@@deriving sexp_of]

    let create ~id ~host ~generation =
      if generation < 0 || generation > C.maximum_wire_integer
      then C.invalid "invalid closed context generation"
      else Ok { id; host; generation }
    ;;

    let to_json t =
      obj
        [ "id", Id.Authoring_context.to_json t.id
        ; "host", Id.Server.to_json t.host
        ; "generation", C.integer t.generation
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "id"; "host"; "generation" ] json in
      let* id = C.required_as fields "id" Id.Authoring_context.of_json in
      let* host = C.required_as fields "host" Id.Server.of_json in
      let* generation = C.required_as fields "generation" C.nonnegative in
      create ~id ~host ~generation
    ;;
  end
end
