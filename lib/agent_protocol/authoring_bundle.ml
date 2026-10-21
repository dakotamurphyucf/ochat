open! Core
module C = Authoring_codec
module A = Authoring_common
module Context = Authoring_context.Context
module Path = Source.Portable_path

let ( let* ) result f = Result.bind result ~f
let obj fields = `Object fields
let str value = `String value

module Bundle = struct
  module Wire_limits = struct
    let max_members = 1024
    let max_manifest_bytes = 4 * 1024 * 1024
    let max_member_bytes = 32 * 1024 * 1024
    let max_total_declared_bytes = 512 * 1024 * 1024
    let max_chunk_bytes = 256 * 1024
  end

  module Owner = struct
    type t =
      { host : Id.Server.t
      ; principal : Id.Principal.t
      }
    [@@deriving equal, sexp_of]

    let create ~host ~principal = { host; principal }

    let to_json t =
      obj
        [ "host", Id.Server.to_json t.host
        ; "principal", Id.Principal.to_json t.principal
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "host"; "principal" ] json in
      let* host = C.required_as fields "host" Id.Server.of_json in
      let* principal = C.required_as fields "principal" Id.Principal.of_json in
      Ok (create ~host ~principal)
    ;;
  end

  module Member = struct
    type t = { source : A.Source.t } [@@deriving equal, sexp_of]

    let create ~source =
      let* () = C.validate_source_id (A.Source.id source) in
      if
        Option.is_some (A.Source.editor_revision source)
        || A.Source.byte_length source > Wire_limits.max_member_bytes
      then C.invalid "invalid immutable bundle member"
      else Ok { source }
    ;;

    let source t = t.source
    let to_json t = A.Source.to_json t.source

    let of_json json =
      let* source = A.Source.of_json json in
      create ~source
    ;;
  end

  module Manifest = struct
    module Route = Authoring_bundle_basis.Route
    module Project = Authoring_bundle_basis.Project_basis
    module Document_member = Authoring_bundle_basis.Document_member
    module Script = Authoring_bundle_basis.Script
    module Chatmd_document = Authoring_bundle_basis.Chatmd_document

    type basis = Authoring_bundle_basis.t =
      | Project of Project.t
      | Chatmd_document of Chatmd_document.t

    let equal_basis = Authoring_bundle_basis.equal

    type t =
      { basis : basis
      ; members : Member.t list
      ; fingerprint : Chatml_source.Digest.t
      }

    let basis t = t.basis
    let members t = t.members
    let fingerprint t = t.fingerprint
    let member_id member = A.Source.id (Member.source member)

    let equal left right =
      Authoring_bundle_basis.equal left.basis right.basis
      && List.equal Member.equal left.members right.members
      && Chatml_source.Digest.equal left.fingerprint right.fingerprint
    ;;

    let sexp_of_t t =
      [%sexp
        { fingerprint = (Chatml_source.Digest.to_hex t.fingerprint : string)
        ; basis = (Jsonaf.to_string (Authoring_bundle_basis.to_json t.basis) : string)
        ; member_count = (List.length t.members : int)
        }]
    ;;

    let encoded ~basis ~members =
      obj
        [ "basis", Authoring_bundle_basis.to_json basis
        ; "members", C.array Member.to_json members
        ]
    ;;

    let fingerprint_of_canonical canonical =
      let context = Digestif.SHA256.init () in
      let context =
        Digestif.SHA256.feed_string context "ochat-authoring-bundle-manifest-v2\000"
      in
      let context = Digestif.SHA256.feed_string context canonical in
      Digestif.SHA256.(to_hex (get context))
      |> Chatml_source.Digest.of_hex
      |> Or_error.ok_exn
    ;;

    let within_count members =
      let rec loop remaining = function
        | [] -> true
        | _ :: rest when remaining > 0 -> loop (remaining - 1) rest
        | _ :: _ -> false
      in
      loop Wire_limits.max_members members
    ;;

    let check_collisions members =
      let groups =
        List.fold members ~init:String.Map.empty ~f:(fun groups member ->
          let id = member_id member in
          let namespace = Chatml_source.Id.package_namespace id in
          let path = Path.create (Chatml_source.Id.relative_path id) |> Or_error.ok_exn in
          Map.update groups namespace ~f:(function
            | None -> [ path ]
            | Some paths -> path :: paths))
      in
      if
        Map.exists groups ~f:(fun paths ->
          Option.is_some (Path.first_collision paths ~checkpoint:Fn.id))
      then C.invalid "colliding immutable bundle members"
      else Ok ()
    ;;

    let create ~basis ~members =
      if not (within_count members)
      then C.invalid "too many immutable bundle members"
      else (
        let total = ref 0 in
        let* () =
          List.fold_result members ~init:() ~f:(fun () member ->
            let source = Member.source member in
            let* () = C.validate_source_id (A.Source.id source) in
            let length = A.Source.byte_length source in
            if
              Option.is_some (A.Source.editor_revision source)
              || length > Wire_limits.max_member_bytes
              || length > Wire_limits.max_total_declared_bytes - !total
            then C.invalid "bundle declared bytes exceed wire limit"
            else (
              total := !total + length;
              Ok ()))
        in
        let members =
          List.sort members ~compare:(fun left right ->
            Chatml_source.Id.compare (member_id left) (member_id right))
        in
        let* () = check_collisions members in
        let* () =
          Authoring_bundle_basis.validate_inventory
            basis
            ~member_sources:(List.map members ~f:Member.source)
        in
        let json = encoded ~basis ~members in
        let* () = C.admit_json ~max_bytes:Wire_limits.max_manifest_bytes json in
        let canonical = Jsonaf.to_string json in
        Ok { basis; members; fingerprint = fingerprint_of_canonical canonical })
    ;;

    let to_json t = encoded ~basis:t.basis ~members:t.members

    let of_json json =
      let* () = C.admit_json ~max_bytes:Wire_limits.max_manifest_bytes json in
      let* fields = C.fields ~allowed:[ "basis"; "members" ] json in
      let* basis = C.required_as fields "basis" Authoring_bundle_basis.of_json in
      let* members =
        C.required_as
          fields
          "members"
          (C.list ~max_items:Wire_limits.max_members Member.of_json)
      in
      create ~basis ~members
    ;;
  end

  module Begin = struct
    type request =
      { parent_context : Context.Expected.t option
      ; idempotency_key : Idempotency_key.t
      ; manifest : Manifest.t
      }
    [@@deriving sexp_of]

    type response =
      { owner : Owner.t
      ; upload_id : Id.Authoring_upload.t
      ; idempotency_key : Idempotency_key.t
      ; manifest_fingerprint : Chatml_source.Digest.t
      ; parent_context : Context.Expected.t option
      ; max_chunk_bytes : int
      ; expires_at : Timestamp.t
      }
    [@@deriving sexp_of]

    let create_request ?parent_context ~idempotency_key ~manifest () =
      { parent_context; idempotency_key; manifest }
    ;;

    let create_response
          ~owner
          ~upload_id
          ~idempotency_key
          ~manifest_fingerprint
          ~parent_context
          ~max_chunk_bytes
          ~expires_at
      =
      if max_chunk_bytes <= 0 || max_chunk_bytes > Wire_limits.max_chunk_bytes
      then C.invalid "invalid bundle chunk ceiling"
      else
        Ok
          { owner
          ; upload_id
          ; idempotency_key
          ; manifest_fingerprint
          ; parent_context
          ; max_chunk_bytes
          ; expires_at
          }
    ;;

    let owner t = t.owner
    let upload_id t = t.upload_id
    let idempotency_key t = t.idempotency_key
    let manifest_fingerprint t = t.manifest_fingerprint
    let parent_context t = t.parent_context
    let max_chunk_bytes t = t.max_chunk_bytes
    let expires_at t = t.expires_at

    let request_to_json (t : request) =
      obj
        ([ "idempotency_key", Idempotency_key.to_json t.idempotency_key
         ; "manifest", Manifest.to_json t.manifest
         ]
         @ C.optional_field "parent_context" t.parent_context Context.Expected.to_json)
    ;;

    let request_of_json json =
      let* fields =
        C.fields ~allowed:[ "idempotency_key"; "manifest"; "parent_context" ] json
      in
      let* idempotency_key =
        C.required_as fields "idempotency_key" Idempotency_key.of_json
      in
      let* manifest = C.required_as fields "manifest" Manifest.of_json in
      let* parent_context =
        C.optional_as fields "parent_context" Context.Expected.of_json
      in
      Ok (create_request ?parent_context ~idempotency_key ~manifest ())
    ;;

    let response_to_json (t : response) =
      obj
        ([ "owner", Owner.to_json t.owner
         ; "upload_id", Id.Authoring_upload.to_json t.upload_id
         ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
         ; "manifest_fingerprint", C.digest_json t.manifest_fingerprint
         ; "max_chunk_bytes", C.integer t.max_chunk_bytes
         ; "expires_at", Timestamp.to_json t.expires_at
         ]
         @ C.optional_field "parent_context" t.parent_context Context.Expected.to_json)
    ;;

    let response_of_json json =
      let* fields =
        C.fields
          ~allowed:
            [ "owner"
            ; "upload_id"
            ; "idempotency_key"
            ; "manifest_fingerprint"
            ; "parent_context"
            ; "max_chunk_bytes"
            ; "expires_at"
            ]
          json
      in
      let* owner = C.required_as fields "owner" Owner.of_json in
      let* upload_id = C.required_as fields "upload_id" Id.Authoring_upload.of_json in
      let* idempotency_key =
        C.required_as fields "idempotency_key" Idempotency_key.of_json
      in
      let* manifest_fingerprint = C.required_as fields "manifest_fingerprint" C.digest in
      let* parent_context =
        C.optional_as fields "parent_context" Context.Expected.of_json
      in
      let* max_chunk_bytes = C.required_as fields "max_chunk_bytes" C.positive in
      let* expires_at = C.required_as fields "expires_at" Timestamp.of_json in
      create_response
        ~owner
        ~upload_id
        ~idempotency_key
        ~manifest_fingerprint
        ~parent_context
        ~max_chunk_bytes
        ~expires_at
    ;;
  end

  module Chunk = struct
    type request =
      { upload_id : Id.Authoring_upload.t
      ; source : Chatml_source.Id.t
      ; byte_offset : int
      ; bytes : string
      }
    [@@deriving sexp_of]

    type response =
      { owner : Owner.t
      ; upload_id : Id.Authoring_upload.t
      ; source : Chatml_source.Id.t
      ; accepted_until_byte : int
      }
    [@@deriving sexp_of]

    let create_request ~upload_id ~source ~byte_offset ~bytes =
      let* () = C.validate_source_id source in
      let length = String.length bytes in
      if
        length = 0
        || length > Wire_limits.max_chunk_bytes
        || byte_offset < 0
        || byte_offset > Wire_limits.max_member_bytes - length
      then C.invalid "invalid bundle chunk range"
      else Ok { upload_id; source; byte_offset; bytes }
    ;;

    let create_response ~owner ~upload_id ~source ~accepted_until_byte =
      let* () = C.validate_source_id source in
      if accepted_until_byte < 0 || accepted_until_byte > Wire_limits.max_member_bytes
      then C.invalid "invalid accepted bundle offset"
      else Ok { owner; upload_id; source; accepted_until_byte }
    ;;

    let owner t = t.owner
    let upload_id t = t.upload_id
    let source t = t.source
    let accepted_until_byte t = t.accepted_until_byte

    let request_to_json (t : request) =
      obj
        [ "upload_id", Id.Authoring_upload.to_json t.upload_id
        ; "source", C.source_id_json t.source
        ; "byte_offset", C.integer t.byte_offset
        ; "bytes_base64", C.bytes_base64_json t.bytes
        ]
    ;;

    let request_of_json json =
      let* fields =
        C.fields ~allowed:[ "upload_id"; "source"; "byte_offset"; "bytes_base64" ] json
      in
      let* upload_id = C.required_as fields "upload_id" Id.Authoring_upload.of_json in
      let* source = C.required_as fields "source" C.source_id in
      let* byte_offset = C.required_as fields "byte_offset" C.nonnegative in
      let* bytes =
        C.required_as
          fields
          "bytes_base64"
          (C.bytes_base64 ~max_bytes:Wire_limits.max_chunk_bytes)
      in
      create_request ~upload_id ~source ~byte_offset ~bytes
    ;;

    let response_to_json (t : response) =
      obj
        [ "owner", Owner.to_json t.owner
        ; "upload_id", Id.Authoring_upload.to_json t.upload_id
        ; "source", C.source_id_json t.source
        ; "accepted_until_byte", C.integer t.accepted_until_byte
        ]
    ;;

    let response_of_json json =
      let* fields =
        C.fields ~allowed:[ "owner"; "upload_id"; "source"; "accepted_until_byte" ] json
      in
      let* owner = C.required_as fields "owner" Owner.of_json in
      let* upload_id = C.required_as fields "upload_id" Id.Authoring_upload.of_json in
      let* source = C.required_as fields "source" C.source_id in
      let* accepted_until_byte =
        C.required_as fields "accepted_until_byte" C.nonnegative
      in
      create_response ~owner ~upload_id ~source ~accepted_until_byte
    ;;
  end

  module Rejection = struct
    type t =
      | Invalid_closure
      | Unsupported_environment
      | Resource_limit
      | Internal_failure
    [@@deriving compare, equal, sexp_of]

    let to_json = function
      | Invalid_closure -> str "invalid_closure"
      | Unsupported_environment -> str "unsupported_environment"
      | Resource_limit -> str "resource_limit"
      | Internal_failure -> str "internal_failure"
    ;;

    let of_json =
      Json_codec.enum
        ~name:"bundle rejection"
        [ "invalid_closure", Invalid_closure
        ; "unsupported_environment", Unsupported_environment
        ; "resource_limit", Resource_limit
        ; "internal_failure", Internal_failure
        ]
    ;;
  end

  module Receipt = struct
    module Project = struct
      type t =
        { root_source : Chatml_source.Id.t
        ; entry_source : Chatml_source.Id.t
        ; selected_lock : Chatml_source.Id.t
        ; input_fingerprint : Chatml_source.Digest.t
        ; compiler_contract : Chatml_source.Digest.t
        ; environment : string
        }

      let equal left right =
        Chatml_source.Id.equal left.root_source right.root_source
        && Chatml_source.Id.equal left.entry_source right.entry_source
        && Chatml_source.Id.equal left.selected_lock right.selected_lock
        && Chatml_source.Digest.equal left.input_fingerprint right.input_fingerprint
        && Chatml_source.Digest.equal left.compiler_contract right.compiler_contract
        && String.equal left.environment right.environment
      ;;

      let create
            ~root_source
            ~entry_source
            ~selected_lock
            ~input_fingerprint
            ~compiler_contract
            ~environment
        =
        let* () = C.validate_source_id root_source in
        let* () = C.validate_source_id entry_source in
        let* () = C.validate_source_id selected_lock in
        if
          String.is_empty environment
          || String.length environment > 256
          || not (Stdlib.String.is_valid_utf_8 environment)
        then C.invalid "invalid bundle receipt environment"
        else
          Ok
            { root_source
            ; entry_source
            ; selected_lock
            ; input_fingerprint
            ; compiler_contract
            ; environment
            }
      ;;

      let root_source t = t.root_source
      let entry_source t = t.entry_source
      let selected_lock t = t.selected_lock
      let input_fingerprint t = t.input_fingerprint
      let compiler_contract t = t.compiler_contract
      let environment t = t.environment

      let to_json t =
        obj
          [ "root_source", C.source_id_json t.root_source
          ; "entry_source", C.source_id_json t.entry_source
          ; "selected_lock", C.source_id_json t.selected_lock
          ; "input_fingerprint", C.digest_json t.input_fingerprint
          ; "compiler_contract", C.digest_json t.compiler_contract
          ; "environment", str t.environment
          ]
      ;;

      let of_json json =
        let* fields =
          C.fields
            ~allowed:
              [ "root_source"
              ; "entry_source"
              ; "selected_lock"
              ; "input_fingerprint"
              ; "compiler_contract"
              ; "environment"
              ]
            json
        in
        let* root_source = C.required_as fields "root_source" C.source_id in
        let* entry_source = C.required_as fields "entry_source" C.source_id in
        let* selected_lock = C.required_as fields "selected_lock" C.source_id in
        let* input_fingerprint = C.required_as fields "input_fingerprint" C.digest in
        let* compiler_contract = C.required_as fields "compiler_contract" C.digest in
        let* environment =
          C.required_as fields "environment" (C.utf8_text ~max_bytes:256)
        in
        create
          ~root_source
          ~entry_source
          ~selected_lock
          ~input_fingerprint
          ~compiler_contract
          ~environment
      ;;
    end

    module Script = struct
      type result =
        | Source_only of
            { selector : Manifest.Script.Selector.t
            ; body_digest : Chatml_source.Digest.t
            }
        | Selected_project of
            { selector : Manifest.Script.Selector.t
            ; body_digest : Chatml_source.Digest.t
            ; project : Project.t
            }

      let selector = function
        | Source_only { selector; _ } | Selected_project { selector; _ } -> selector
      ;;

      let body_digest = function
        | Source_only { body_digest; _ } | Selected_project { body_digest; _ } ->
          body_digest
      ;;

      let equal_result left right =
        let common =
          Manifest.Script.Selector.equal (selector left) (selector right)
          && Chatml_source.Digest.equal (body_digest left) (body_digest right)
        in
        common
        &&
        match left, right with
        | Source_only _, Source_only _ -> true
        | Selected_project left, Selected_project right ->
          Project.equal left.project right.project
        | Source_only _, Selected_project _ | Selected_project _, Source_only _ -> false
      ;;

      let to_json = function
        | Source_only { selector; body_digest } ->
          obj
            [ "kind", str "source_only"
            ; "selector", Manifest.Script.Selector.to_json selector
            ; "body_digest", C.digest_json body_digest
            ]
        | Selected_project { selector; body_digest; project } ->
          obj
            [ "kind", str "selected_project"
            ; "selector", Manifest.Script.Selector.to_json selector
            ; "body_digest", C.digest_json body_digest
            ; "project", Project.to_json project
            ]
      ;;

      let of_json json =
        let* fields =
          C.fields ~allowed:[ "kind"; "selector"; "body_digest"; "project" ] json
        in
        let* kind = C.required_as fields "kind" (C.string ~max_bytes:24) in
        let* selector =
          C.required_as fields "selector" Manifest.Script.Selector.of_json
        in
        let* body_digest = C.required_as fields "body_digest" C.digest in
        if
          not
            (Chatml_source.Digest.equal
               body_digest
               (Manifest.Script.Selector.body_digest selector))
        then C.invalid "bundle receipt script digest mismatch"
        else (
          match kind with
          | "source_only" ->
            let* _ = C.fields ~allowed:[ "kind"; "selector"; "body_digest" ] json in
            Ok (Source_only { selector; body_digest })
          | "selected_project" ->
            let* project = C.required_as fields "project" Project.of_json in
            Ok (Selected_project { selector; body_digest; project })
          | _ -> C.invalid "invalid bundle receipt script kind")
      ;;
    end

    module Chatmd_document = struct
      type t =
        { root_source : Chatml_source.Id.t
        ; closure_fingerprint : Chatml_source.Digest.t
        ; scripts : Script.result list
        }

      let root_source t = t.root_source
      let closure_fingerprint t = t.closure_fingerprint
      let scripts t = t.scripts

      let compare_selector left right =
        Manifest.Script.Selector.key_compare
          (Script.selector left)
          (Script.selector right)
      ;;

      let create ~root_source ~closure_fingerprint ~scripts =
        let* () = C.validate_source_id root_source in
        if
          not
            (let rec within left = function
               | [] -> true
               | _ :: rest when left > 0 -> within (left - 1) rest
               | _ :: _ -> false
             in
             within 256 scripts)
        then C.invalid "too many bundle receipt scripts"
        else
          let* () =
            List.fold_result scripts ~init:() ~f:(fun () script ->
              if
                Chatml_source.Digest.equal
                  (Script.body_digest script)
                  (Manifest.Script.Selector.body_digest (Script.selector script))
              then Ok ()
              else C.invalid "bundle receipt script digest mismatch")
          in
          let scripts = List.sort scripts ~compare:compare_selector in
          let rec unique = function
            | [] | [ _ ] -> true
            | left :: (right :: _ as rest) ->
              compare_selector left right <> 0 && unique rest
          in
          if not (unique scripts)
          then C.invalid "duplicate bundle receipt script"
          else Ok { root_source; closure_fingerprint; scripts }
      ;;

      let equal left right =
        Chatml_source.Id.equal left.root_source right.root_source
        && Chatml_source.Digest.equal left.closure_fingerprint right.closure_fingerprint
        && List.equal Script.equal_result left.scripts right.scripts
      ;;

      let to_json t =
        obj
          [ "root_source", C.source_id_json t.root_source
          ; "closure_fingerprint", C.digest_json t.closure_fingerprint
          ; "scripts", C.array Script.to_json t.scripts
          ]
      ;;

      let of_json json =
        let* fields =
          C.fields ~allowed:[ "root_source"; "closure_fingerprint"; "scripts" ] json
        in
        let* root_source = C.required_as fields "root_source" C.source_id in
        let* closure_fingerprint = C.required_as fields "closure_fingerprint" C.digest in
        let* scripts =
          C.required_as fields "scripts" (C.list ~max_items:256 Script.of_json)
        in
        create ~root_source ~closure_fingerprint ~scripts
      ;;
    end

    type basis =
      | Project of Project.t
      | Chatmd_document of Chatmd_document.t

    let equal_basis left right =
      match left, right with
      | Project left, Project right -> Project.equal left right
      | Chatmd_document left, Chatmd_document right -> Chatmd_document.equal left right
      | Project _, Chatmd_document _ | Chatmd_document _, Project _ -> false
    ;;

    let basis_to_json = function
      | Project project ->
        obj [ "kind", str "project"; "project", Project.to_json project ]
      | Chatmd_document document ->
        obj
          [ "kind", str "chatmd_document"; "document", Chatmd_document.to_json document ]
    ;;

    let basis_of_json json =
      let* fields = C.fields ~allowed:[ "kind"; "project"; "document" ] json in
      let* kind = C.required_as fields "kind" (C.string ~max_bytes:24) in
      match kind with
      | "project" ->
        let* _ = C.fields ~allowed:[ "kind"; "project" ] json in
        let* project = C.required_as fields "project" Project.of_json in
        Ok (Project project)
      | "chatmd_document" ->
        let* _ = C.fields ~allowed:[ "kind"; "document" ] json in
        let* document = C.required_as fields "document" Chatmd_document.of_json in
        Ok (Chatmd_document document)
      | _ -> C.invalid "invalid bundle receipt basis"
    ;;

    type t =
      { owner : Owner.t
      ; upload_id : Id.Authoring_upload.t
      ; bundle_id : Id.Authoring_bundle.t
      ; manifest_fingerprint : Chatml_source.Digest.t
      ; basis : basis
      ; expires_at : Timestamp.t
      }

    let create ~owner ~upload_id ~bundle_id ~manifest_fingerprint ~basis ~expires_at =
      let json = basis_to_json basis in
      let* () = C.admit_json ~max_bytes:Wire_limits.max_manifest_bytes json in
      Ok { owner; upload_id; bundle_id; manifest_fingerprint; basis; expires_at }
    ;;

    let owner t = t.owner
    let upload_id t = t.upload_id
    let bundle_id t = t.bundle_id
    let manifest_fingerprint t = t.manifest_fingerprint
    let basis t = t.basis
    let expires_at t = t.expires_at

    let equal left right =
      Owner.equal left.owner right.owner
      && Id.Authoring_upload.equal left.upload_id right.upload_id
      && Id.Authoring_bundle.equal left.bundle_id right.bundle_id
      && Chatml_source.Digest.equal left.manifest_fingerprint right.manifest_fingerprint
      && equal_basis left.basis right.basis
      && Timestamp.equal left.expires_at right.expires_at
    ;;

    let to_json t =
      obj
        [ "owner", Owner.to_json t.owner
        ; "upload_id", Id.Authoring_upload.to_json t.upload_id
        ; "bundle_id", Id.Authoring_bundle.to_json t.bundle_id
        ; "manifest_fingerprint", C.digest_json t.manifest_fingerprint
        ; "basis", basis_to_json t.basis
        ; "expires_at", Timestamp.to_json t.expires_at
        ]
    ;;

    let sexp_of_t t = Sexp.Atom (Jsonaf.to_string (to_json t))

    let of_json json =
      let* () = C.admit_json ~max_bytes:Wire_limits.max_manifest_bytes json in
      let* fields =
        C.fields
          ~allowed:
            [ "owner"
            ; "upload_id"
            ; "bundle_id"
            ; "manifest_fingerprint"
            ; "basis"
            ; "expires_at"
            ]
          json
      in
      let* owner = C.required_as fields "owner" Owner.of_json in
      let* upload_id = C.required_as fields "upload_id" Id.Authoring_upload.of_json in
      let* bundle_id = C.required_as fields "bundle_id" Id.Authoring_bundle.of_json in
      let* manifest_fingerprint = C.required_as fields "manifest_fingerprint" C.digest in
      let* basis = C.required_as fields "basis" basis_of_json in
      let* expires_at = C.required_as fields "expires_at" Timestamp.of_json in
      create ~owner ~upload_id ~bundle_id ~manifest_fingerprint ~basis ~expires_at
    ;;
  end

  module Status = struct
    type request = { upload_id : Id.Authoring_upload.t } [@@deriving sexp_of]

    module Progress = struct
      type t =
        { source : Chatml_source.Id.t
        ; accepted_until_byte : int
        }
      [@@deriving equal, sexp_of]

      let create ~source ~accepted_until_byte =
        let* () = C.validate_source_id source in
        if accepted_until_byte < 0 || accepted_until_byte > Wire_limits.max_member_bytes
        then C.invalid "invalid bundle progress offset"
        else Ok { source; accepted_until_byte }
      ;;

      let source t = t.source
      let accepted_until_byte t = t.accepted_until_byte

      let to_json t =
        obj
          [ "source", C.source_id_json t.source
          ; "accepted_until_byte", C.integer t.accepted_until_byte
          ]
      ;;

      let of_json json =
        let* fields = C.fields ~allowed:[ "source"; "accepted_until_byte" ] json in
        let* source = C.required_as fields "source" C.source_id in
        let* accepted_until_byte =
          C.required_as fields "accepted_until_byte" C.nonnegative
        in
        create ~source ~accepted_until_byte
      ;;
    end

    type state =
      | Staging of Progress.t list
      | Validating
      | Published of Receipt.t
      | Rejected of Rejection.t
      | Expired
    [@@deriving sexp_of]

    type t =
      { owner : Owner.t
      ; upload_id : Id.Authoring_upload.t
      ; state : state
      }
    [@@deriving sexp_of]

    let create_request ~upload_id = { upload_id }

    let valid_progress progress =
      let rec loop previous total remaining = function
        | [] -> true
        | _ :: _ when remaining = 0 -> false
        | item :: rest ->
          let id = Progress.source item in
          let count = Progress.accepted_until_byte item in
          let ordered =
            match previous with
            | None -> true
            | Some before -> Chatml_source.Id.compare before id < 0
          in
          ordered
          && count <= Wire_limits.max_total_declared_bytes - total
          && loop (Some id) (total + count) (remaining - 1) rest
      in
      loop None 0 Wire_limits.max_members progress
    ;;

    let create ~owner ~upload_id ~state =
      let valid =
        match state with
        | Staging progress -> valid_progress progress
        | Published receipt ->
          Owner.equal owner (Receipt.owner receipt)
          && Id.Authoring_upload.equal upload_id (Receipt.upload_id receipt)
        | Validating | Rejected _ | Expired -> true
      in
      if valid then Ok { owner; upload_id; state } else C.invalid "invalid bundle status"
    ;;

    let owner t = t.owner
    let upload_id t = t.upload_id
    let state t = t.state

    let request_to_json (t : request) =
      obj [ "upload_id", Id.Authoring_upload.to_json t.upload_id ]
    ;;

    let request_of_json json =
      let* fields = C.fields ~allowed:[ "upload_id" ] json in
      let* upload_id = C.required_as fields "upload_id" Id.Authoring_upload.of_json in
      Ok (create_request ~upload_id)
    ;;

    let state_to_json = function
      | Staging progress ->
        obj [ "kind", str "staging"; "progress", C.array Progress.to_json progress ]
      | Validating -> obj [ "kind", str "validating" ]
      | Published receipt ->
        obj [ "kind", str "published"; "receipt", Receipt.to_json receipt ]
      | Rejected reason ->
        obj [ "kind", str "rejected"; "reason", Rejection.to_json reason ]
      | Expired -> obj [ "kind", str "expired" ]
    ;;

    let state_of_json json =
      let* fields = C.fields ~allowed:[ "kind"; "progress"; "receipt"; "reason" ] json in
      let* kind = C.required_as fields "kind" (C.string ~max_bytes:20) in
      let permitted =
        match kind with
        | "staging" -> [ "kind"; "progress" ]
        | "validating" | "expired" -> [ "kind" ]
        | "published" -> [ "kind"; "receipt" ]
        | "rejected" -> [ "kind"; "reason" ]
        | _ -> []
      in
      if
        List.is_empty permitted
        || List.exists (Json_codec.to_alist fields) ~f:(fun (name, _) ->
          not (List.mem permitted name ~equal:String.equal))
      then C.invalid "invalid bundle status kind/fields"
      else (
        match kind with
        | "staging" ->
          let* progress =
            C.required_as
              fields
              "progress"
              (C.list ~max_items:Wire_limits.max_members Progress.of_json)
          in
          Ok (Staging progress)
        | "validating" -> Ok Validating
        | "published" ->
          let* receipt = C.required_as fields "receipt" Receipt.of_json in
          Ok (Published receipt)
        | "rejected" ->
          let* reason = C.required_as fields "reason" Rejection.of_json in
          Ok (Rejected reason)
        | "expired" -> Ok Expired
        | _ -> assert false)
    ;;

    let to_json t =
      obj
        [ "owner", Owner.to_json t.owner
        ; "upload_id", Id.Authoring_upload.to_json t.upload_id
        ; "state", state_to_json t.state
        ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "owner"; "upload_id"; "state" ] json in
      let* owner = C.required_as fields "owner" Owner.of_json in
      let* upload_id = C.required_as fields "upload_id" Id.Authoring_upload.of_json in
      let* state = C.required_as fields "state" state_of_json in
      create ~owner ~upload_id ~state
    ;;
  end

  module Commit = struct
    type request =
      { upload_id : Id.Authoring_upload.t
      ; idempotency_key : Idempotency_key.t
      }
    [@@deriving sexp_of]

    type outcome =
      | Accepted_for_validation
      | Published of Receipt.t
      | Rejected of Rejection.t
    [@@deriving sexp_of]

    type response =
      { owner : Owner.t
      ; upload_id : Id.Authoring_upload.t
      ; idempotency_key : Idempotency_key.t
      ; outcome : outcome
      }
    [@@deriving sexp_of]

    let create_request ~upload_id ~idempotency_key = { upload_id; idempotency_key }

    let create_response ~owner ~upload_id ~idempotency_key ~outcome =
      let valid =
        match outcome with
        | Published receipt ->
          Owner.equal owner (Receipt.owner receipt)
          && Id.Authoring_upload.equal upload_id (Receipt.upload_id receipt)
        | Accepted_for_validation | Rejected _ -> true
      in
      if valid
      then Ok { owner; upload_id; idempotency_key; outcome }
      else C.invalid "invalid bundle commit receipt"
    ;;

    let owner t = t.owner
    let upload_id t = t.upload_id
    let idempotency_key t = t.idempotency_key
    let outcome t = t.outcome

    let request_to_json (t : request) =
      obj
        [ "upload_id", Id.Authoring_upload.to_json t.upload_id
        ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
        ]
    ;;

    let request_of_json json =
      let* fields = C.fields ~allowed:[ "upload_id"; "idempotency_key" ] json in
      let* upload_id = C.required_as fields "upload_id" Id.Authoring_upload.of_json in
      let* idempotency_key =
        C.required_as fields "idempotency_key" Idempotency_key.of_json
      in
      Ok (create_request ~upload_id ~idempotency_key)
    ;;

    let outcome_to_json = function
      | Accepted_for_validation -> obj [ "kind", str "accepted_for_validation" ]
      | Published receipt ->
        obj [ "kind", str "published"; "receipt", Receipt.to_json receipt ]
      | Rejected reason ->
        obj [ "kind", str "rejected"; "reason", Rejection.to_json reason ]
    ;;

    let outcome_of_json json =
      let* fields = C.fields ~allowed:[ "kind"; "receipt"; "reason" ] json in
      let* kind = C.required_as fields "kind" (C.string ~max_bytes:25) in
      let permitted =
        match kind with
        | "accepted_for_validation" -> [ "kind" ]
        | "published" -> [ "kind"; "receipt" ]
        | "rejected" -> [ "kind"; "reason" ]
        | _ -> []
      in
      if
        List.is_empty permitted
        || List.exists (Json_codec.to_alist fields) ~f:(fun (name, _) ->
          not (List.mem permitted name ~equal:String.equal))
      then C.invalid "invalid bundle commit kind/fields"
      else (
        match kind with
        | "accepted_for_validation" -> Ok Accepted_for_validation
        | "published" ->
          let* receipt = C.required_as fields "receipt" Receipt.of_json in
          Ok (Published receipt)
        | "rejected" ->
          let* reason = C.required_as fields "reason" Rejection.of_json in
          Ok (Rejected reason)
        | _ -> assert false)
    ;;

    let response_to_json (t : response) =
      obj
        [ "owner", Owner.to_json t.owner
        ; "upload_id", Id.Authoring_upload.to_json t.upload_id
        ; "idempotency_key", Idempotency_key.to_json t.idempotency_key
        ; "outcome", outcome_to_json t.outcome
        ]
    ;;

    let response_of_json json =
      let* fields =
        C.fields ~allowed:[ "owner"; "upload_id"; "idempotency_key"; "outcome" ] json
      in
      let* owner = C.required_as fields "owner" Owner.of_json in
      let* upload_id = C.required_as fields "upload_id" Id.Authoring_upload.of_json in
      let* idempotency_key =
        C.required_as fields "idempotency_key" Idempotency_key.of_json
      in
      let* outcome = C.required_as fields "outcome" outcome_of_json in
      create_response ~owner ~upload_id ~idempotency_key ~outcome
    ;;
  end
end
