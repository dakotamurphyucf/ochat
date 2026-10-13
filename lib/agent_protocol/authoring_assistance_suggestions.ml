open! Core
open Authoring_assistance_base

module Assistance = struct
  module Request = struct
    type t =
      { context : Context.Expected.t
      ; source : Source.t
      ; cursor_byte : int
      ; max_items : int
      }

    let create ~context ~source ~cursor_byte ~max_items =
      let* _ = bounded cursor_byte in
      if cursor_byte > Source.byte_length source
      then C.invalid "assistance cursor exceeds source"
      else if max_items <= 0 || max_items > Limits.max_suggestions
      then C.invalid "invalid assistance item limit"
      else Ok { context; source; cursor_byte; max_items }
    ;;

    let context t = t.context
    let source t = t.source
    let cursor_byte t = t.cursor_byte
    let max_items t = t.max_items

    let equal a b =
      Context.Expected.equal a.context b.context
      && Source.equal a.source b.source
      && Int.equal a.cursor_byte b.cursor_byte
      && Int.equal a.max_items b.max_items
    ;;

    let sexp_of_t _ = Sexp.Atom "authoring_assistance_request"

    let to_json t =
      obj
        [ "context", Context.Expected.to_json t.context
        ; "source", Source.to_json t.source
        ; "cursor_byte", C.integer t.cursor_byte
        ; "max_items", C.integer t.max_items
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "context"; "source"; "cursor_byte"; "max_items" ] json
      in
      let* context = field fields "context" Context.Expected.of_json in
      let* source = field fields "source" Source.of_json in
      let* cursor_byte = field fields "cursor_byte" C.nonnegative in
      let* max_items = field fields "max_items" C.positive in
      create ~context ~source ~cursor_byte ~max_items
    ;;
  end

  module Selected_catalog = struct
    type t =
      { environment : string
      ; input_fingerprint : Digest.t
      ; compiler_contract : Digest.t
      }
    [@@deriving sexp_of]

    let create ~environment ~input_fingerprint ~compiler_contract =
      let* _ = text ~max:256 environment in
      if String.is_empty environment
      then C.invalid "empty selected environment"
      else Ok { environment; input_fingerprint; compiler_contract }
    ;;

    let environment t = t.environment
    let input_fingerprint t = t.input_fingerprint
    let compiler_contract t = t.compiler_contract

    let to_json t =
      obj
        [ "environment", str t.environment
        ; "input_fingerprint", C.digest_json t.input_fingerprint
        ; "compiler_contract", C.digest_json t.compiler_contract
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "environment"; "input_fingerprint"; "compiler_contract" ] json
      in
      let* environment = field fields "environment" (C.utf8_text ~max_bytes:256) in
      let* input_fingerprint = field fields "input_fingerprint" C.digest in
      let* compiler_contract = field fields "compiler_contract" C.digest in
      create ~environment ~input_fingerprint ~compiler_contract
    ;;
  end

  module Range = Authoring_analysis.Byte_range

  module Suggestion = struct
    type kind =
      | Tag
      | Attribute
      | Value
      | Reference
      | Path
      | Template
      | Environment
    [@@deriving compare, equal, sexp_of]

    type evidence =
      | Lexical
      | Parsed_document of Source.t
      | Parsed_project of Source.t
      | Installed_builtin of
          { id : string
          ; sha256 : Digest.t
          }
      | Selected_catalog of Selected_catalog.t
      | Host_root of Id.Authoring_root.t
    [@@deriving sexp_of]

    type t =
      { kind : kind
      ; label : string
      ; insert_text : string
      ; replace : Range.t
      ; evidence : evidence
      }

    let allowed kind evidence =
      match kind, evidence with
      | (Tag | Attribute | Value), (Lexical | Parsed_document _)
      | Reference, (Parsed_document _ | Selected_catalog _)
      | Path, (Parsed_project _ | Selected_catalog _ | Host_root _)
      | Template, (Lexical | Parsed_document _ | Installed_builtin _)
      | Environment, (Parsed_project _ | Selected_catalog _) -> true
      | _ -> false
    ;;

    let valid_builtin_id id =
      let valid_first = function
        | 'a' .. 'z' -> true
        | _ -> false
      in
      let valid_rest = function
        | 'a' .. 'z' | '0' .. '9' | '_' | '-' -> true
        | _ -> false
      in
      if
        String.length id > 0
        && String.length id <= 64
        && valid_first id.[0]
        && String.for_all id ~f:valid_rest
      then Ok ()
      else C.invalid "invalid installed builtin identity"
    ;;

    let create ~kind ~label ~insert_text ~replace ~evidence =
      let* _ = text ~max:4096 label in
      let* _ = text ~max:4096 insert_text in
      if String.is_empty label || not (allowed kind evidence)
      then C.invalid "invalid assistance suggestion kind or evidence"
      else
        let* () =
          match evidence with
          | Installed_builtin { id; _ } -> valid_builtin_id id
          | Host_root root ->
            Result.map
              (Id.Authoring_root.of_json (Id.Authoring_root.to_json root))
              ~f:ignore
          | _ -> Ok ()
        in
        Ok { kind; label; insert_text; replace; evidence }
    ;;

    let kind t = t.kind
    let label t = t.label
    let insert_text t = t.insert_text
    let evidence t = t.evidence
    let replace t = t.replace

    let kind_to_json = function
      | Tag -> str "tag"
      | Attribute -> str "attribute"
      | Value -> str "value"
      | Reference -> str "reference"
      | Path -> str "path"
      | Template -> str "template"
      | Environment -> str "environment"
    ;;

    let kind_of_json json =
      let* value = C.string ~max_bytes:16 json in
      match value with
      | "tag" -> Ok Tag
      | "attribute" -> Ok Attribute
      | "value" -> Ok Value
      | "reference" -> Ok Reference
      | "path" -> Ok Path
      | "template" -> Ok Template
      | "environment" -> Ok Environment
      | _ -> C.invalid "unknown assistance suggestion kind"
    ;;

    let evidence_to_json = function
      | Lexical -> obj [ "kind", str "lexical" ]
      | Parsed_document source ->
        obj [ "kind", str "parsed_document"; "source", Source.to_json source ]
      | Parsed_project source ->
        obj [ "kind", str "parsed_project"; "source", Source.to_json source ]
      | Installed_builtin { id; sha256 } ->
        obj
          [ "kind", str "installed_builtin"
          ; "id", str id
          ; "sha256", C.digest_json sha256
          ]
      | Selected_catalog selected ->
        obj
          [ "kind", str "selected_catalog"; "catalog", Selected_catalog.to_json selected ]
      | Host_root root ->
        obj [ "kind", str "host_root"; "root", Id.Authoring_root.to_json root ]
    ;;

    let evidence_of_json json =
      let* fields =
        C.fields ~allowed:[ "kind"; "source"; "id"; "sha256"; "catalog"; "root" ] json
      in
      let* kind = field fields "kind" (C.string ~max_bytes:24) in
      match kind with
      | "lexical" ->
        let* _ = C.fields ~allowed:[ "kind" ] json in
        Ok Lexical
      | "parsed_document" | "parsed_project" ->
        let* fields = C.fields ~allowed:[ "kind"; "source" ] json in
        let* source = field fields "source" Source.of_json in
        Ok
          (if String.equal kind "parsed_document"
           then Parsed_document source
           else Parsed_project source)
      | "installed_builtin" ->
        let* fields = C.fields ~allowed:[ "kind"; "id"; "sha256" ] json in
        let* id = field fields "id" (C.utf8_text ~max_bytes:64) in
        let* sha256 = field fields "sha256" C.digest in
        Ok (Installed_builtin { id; sha256 })
      | "selected_catalog" ->
        let* fields = C.fields ~allowed:[ "kind"; "catalog" ] json in
        let* value = field fields "catalog" Selected_catalog.of_json in
        Ok (Selected_catalog value)
      | "host_root" ->
        let* fields = C.fields ~allowed:[ "kind"; "root" ] json in
        let* root = field fields "root" Id.Authoring_root.of_json in
        Ok (Host_root root)
      | _ -> C.invalid "unknown assistance evidence"
    ;;

    let to_json t =
      obj
        [ "kind", kind_to_json t.kind
        ; "label", str t.label
        ; "insert_text", str t.insert_text
        ; "replace", Range.to_json t.replace
        ; "evidence", evidence_to_json t.evidence
        ]
    ;;

    let of_json json =
      let* fields =
        C.fields ~allowed:[ "kind"; "label"; "insert_text"; "replace"; "evidence" ] json
      in
      let* kind = field fields "kind" kind_of_json in
      let* label = field fields "label" (C.utf8_text ~max_bytes:4096) in
      let* insert_text = field fields "insert_text" (C.utf8_text ~max_bytes:4096) in
      let* replace = field fields "replace" Range.of_json in
      let* evidence = field fields "evidence" evidence_of_json in
      create ~kind ~label ~insert_text ~replace ~evidence
    ;;
  end

  module Status = struct
    type unavailable =
      | No_local_context
      | Catalog_unavailable
      | Source_map_unavailable
    [@@deriving compare, equal, sexp_of]

    type truncation =
      | Item_limit
      | Work_limit
      | Result_bytes
    [@@deriving compare, equal, sexp_of]

    type t =
      | Complete
      | Truncated of truncation
      | Unavailable of unavailable
    [@@deriving compare, equal, sexp_of]

    let to_json = function
      | Complete -> obj [ "kind", str "complete" ]
      | Truncated reason ->
        obj
          [ "kind", str "truncated"
          ; ( "reason"
            , str
                (match reason with
                 | Item_limit -> "item_limit"
                 | Work_limit -> "work_limit"
                 | Result_bytes -> "result_bytes") )
          ]
      | Unavailable reason ->
        obj
          [ "kind", str "unavailable"
          ; ( "reason"
            , str
                (match reason with
                 | No_local_context -> "no_local_context"
                 | Catalog_unavailable -> "catalog_unavailable"
                 | Source_map_unavailable -> "source_map_unavailable") )
          ]
    ;;

    let of_json json =
      let* fields = C.fields ~allowed:[ "kind"; "reason" ] json in
      let* kind = field fields "kind" (C.string ~max_bytes:16) in
      match kind with
      | "complete" ->
        let* _ = C.fields ~allowed:[ "kind" ] json in
        Ok Complete
      | "truncated" ->
        let* reason = field fields "reason" (C.string ~max_bytes:24) in
        (match reason with
         | "item_limit" -> Ok (Truncated Item_limit)
         | "work_limit" -> Ok (Truncated Work_limit)
         | "result_bytes" -> Ok (Truncated Result_bytes)
         | _ -> C.invalid "unknown assistance truncation reason")
      | "unavailable" ->
        let* reason = field fields "reason" (C.string ~max_bytes:24) in
        (match reason with
         | "no_local_context" -> Ok (Unavailable No_local_context)
         | "catalog_unavailable" -> Ok (Unavailable Catalog_unavailable)
         | "source_map_unavailable" -> Ok (Unavailable Source_map_unavailable)
         | _ -> C.invalid "unknown assistance unavailable reason")
      | _ -> C.invalid "unknown assistance status"
    ;;
  end

  module Result = struct
    type t =
      { request : Request.t
      ; items : Suggestion.t list
      ; status : Status.t
      }

    let request t = t.request
    let items t = t.items
    let status t = t.status
    let sexp_of_t _ = Sexp.Atom "authoring_assistance_result"

    let to_json t =
      obj
        [ "request", Request.to_json t.request
        ; "items", C.array Suggestion.to_json t.items
        ; "status", Status.to_json t.status
        ]
    ;;

    let preflight ~request ~items ~status =
      let* budget = C.Budget.create ~max_bytes:Limits.max_result_bytes in
      let* () =
        C.Budget.charge_json
          budget
          ~parent_depth:0
          (obj
             [ "request", Request.to_json request
             ; "items", `Array []
             ; "status", Status.to_json status
             ])
      in
      List.fold items ~init:(Ok ()) ~f:(fun result item ->
        let* () = result in
        let* () = C.Budget.reserve budget ~bytes:1 ~nodes:0 in
        C.Budget.charge_json budget ~parent_depth:2 (Suggestion.to_json item))
    ;;

    let create ~request ~items ~status =
      if
        (not (at_most (Request.max_items request) items))
        || not (at_most Limits.max_suggestions items)
      then C.invalid "too many assistance suggestions"
      else if
        match status with
        | Status.Unavailable _ -> not (List.is_empty items)
        | _ -> false
      then C.invalid "unavailable assistance cannot have suggestions"
      else if
        not
          (List.for_all items ~f:(fun item ->
             let range = Suggestion.replace item in
             Source.equal (Range.source range) (Request.source request)
             && Range.start_byte range <= Request.cursor_byte request
             && Request.cursor_byte request <= Range.end_byte range))
      then C.invalid "assistance replacement differs from authored request"
      else
        let* _ =
          List.fold items ~init:(Ok Limits.max_label_bytes) ~f:(fun remaining item ->
            let* remaining = remaining in
            let label = String.length (Suggestion.label item) in
            let insert = String.length (Suggestion.insert_text item) in
            if label > remaining || insert > remaining - label
            then C.invalid "assistance label bytes exceed aggregate limit"
            else Ok (remaining - label - insert))
        in
        let* () = preflight ~request ~items ~status in
        Ok { request; items; status }
    ;;

    let of_json json =
      let* () = C.admit_json ~max_bytes:Limits.max_result_bytes json in
      let* fields = C.fields ~allowed:[ "request"; "items"; "status" ] json in
      let* request = field fields "request" Request.of_json in
      let* items =
        field fields "items" (C.list ~max_items:Limits.max_suggestions Suggestion.of_json)
      in
      let* status = field fields "status" Status.of_json in
      create ~request ~items ~status
    ;;
  end
end
